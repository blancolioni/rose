with System;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;                        use Rose.Words;

with Rose.Devices.Port_IO;

with Text_Console.Cells;

package body VGA_Console.Screen is

   Num_Lines   : constant := 24;
   Num_Columns : constant := 80;

   Blank       : constant Word_16 := 16#0720#;
   Colour      : constant Word_16 := 16#0700#;

   type Screen_Array is
     array (0 .. Num_Lines * Num_Columns - 1) of Word_16
     with Component_Size => 16;

   Screen_Memory : Screen_Array;
   pragma Import (Ada, Screen_Memory);
   for Screen_Memory'Address use System'To_Address (16#000B_80A0#);
   pragma Volatile (Screen_Memory);

   Taken_Over    : Boolean := False;

   --  The cell model's drawing, on rows 1 to 24 of the VGA text page.

   procedure Draw (Column, Row : Natural; Item : Character);
   procedure Scroll (Lines : Positive);
   procedure Clear;
   procedure Show_Cursor (Column, Row : Natural);
   procedure Flush is null;
   --  The VGA page shows each cell as it is written.

   package Cells is
     new Text_Console.Cells
       (Max_Columns => Num_Columns,
        Max_Rows    => Num_Lines,
        Draw        => Draw,
        Scroll      => Scroll,
        Clear       => Clear,
        Show_Cursor => Show_Cursor,
        Flush       => Flush);

   procedure Take_Over;
   --  Take the screen as the boot console left it: its cells, and the
   --  cursor position from the CRT controller.

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Screen_Memory := (others => Blank);
   end Clear;

   ----------
   -- Draw --
   ----------

   procedure Draw (Column, Row : Natural; Item : Character) is
   begin
      Screen_Memory (Row * Num_Columns + Column) :=
        Colour + Character'Pos (Item);
   end Draw;

   ---------
   -- Map --
   ---------

   procedure Map is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Screen_Page_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Map;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      if not Taken_Over then
         Take_Over;
      end if;

      Cells.Put (Text);
   end Put;

   ------------
   -- Redraw --
   ------------

   procedure Redraw is
   begin
      Map;
      Cells.Redraw;
   end Redraw;

   ------------
   -- Scroll --
   ------------

   procedure Scroll (Lines : Positive) is
      Kept : constant Natural :=
               (if Lines < Num_Lines then Num_Lines - Lines else 0);
   begin
      Screen_Memory (0 .. Kept * Num_Columns - 1) :=
        Screen_Memory ((Num_Lines - Kept) * Num_Columns
                       .. Num_Lines * Num_Columns - 1);
      Screen_Memory (Kept * Num_Columns .. Num_Lines * Num_Columns - 1) :=
        (others => Blank);
   end Scroll;

   -----------------
   -- Show_Cursor --
   -----------------

   procedure Show_Cursor (Column, Row : Natural) is
      Position : constant Natural := (Row + 1) * Num_Columns + Column;
   begin
      --  The cursor position counts the status line.  Index 16#0E# of
      --  the CRT controller is its high byte, and 16#0F# its low byte.

      Rose.Devices.Port_IO.Port_Out_8
        (Cursor_Out_Cap,
         ((0, 16#0E#), (1, Word_8 (Position / 256)),
          (0, 16#0F#), (1, Word_8 (Position mod 256))));
   end Show_Cursor;

   ---------------
   -- Take_Over --
   ---------------

   procedure Take_Over is
      Position : Natural;
   begin
      Cells.Set_Size (Num_Columns, Num_Lines);
      for Row in 0 .. Num_Lines - 1 loop
         for Column in 0 .. Num_Columns - 1 loop
            Cells.Load
              (Column, Row,
               Character'Val
                 (Screen_Memory (Row * Num_Columns + Column) mod 256));
         end loop;
      end loop;

      Rose.Devices.Port_IO.Port_Out_8 (Cursor_Out_Cap, 0, 16#0E#);
      Position :=
        Natural (Rose.Devices.Port_IO.Port_In_8 (Cursor_In_Cap)) * 256;
      Rose.Devices.Port_IO.Port_Out_8 (Cursor_Out_Cap, 0, 16#0F#);
      Position :=
        Position + Natural (Rose.Devices.Port_IO.Port_In_8 (Cursor_In_Cap));

      --  The cursor position counts the status line.

      if Position < Num_Columns
        or else Position >= (Num_Lines + 1) * Num_Columns
      then
         Cells.Move_Cursor (0, Num_Lines - 1);
      else
         Cells.Move_Cursor
           (Position mod Num_Columns, Position / Num_Columns - 1);
      end if;

      Taken_Over := True;
   end Take_Over;

end VGA_Console.Screen;
