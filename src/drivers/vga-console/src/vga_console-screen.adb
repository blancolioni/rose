with System;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;                        use Rose.Words;

with Rose.Devices.Port_IO;

with Text_Console.Cells;
with Text_Console.Serial;

package body VGA_Console.Screen is

   Num_Lines   : constant := 24;
   Num_Columns : constant := 80;

   Blank       : constant Word_16 := 16#0720#;

   function Colour (Attribute : Text_Console.Attributes) return Word_16
   is (case Attribute.Bold is
          when False =>
            (if Attribute.Reverse_Video then 16#7000# else 16#0700#),
          when True  =>
            (if Attribute.Reverse_Video then 16#7F00# else 16#0F00#));
   --  Light grey on black, white for bold, and black on light grey, or
   --  white on light grey, in reverse video.

   type Screen_Array is
     array (0 .. Num_Lines * Num_Columns - 1) of Word_16
     with Component_Size => 16;

   Screen_Memory : Screen_Array;
   pragma Import (Ada, Screen_Memory);
   for Screen_Memory'Address use System'To_Address (16#000B_80A0#);
   pragma Volatile (Screen_Memory);

   Taken_Over    : Boolean := False;

   --  The cell model's drawing, on rows 1 to 24 of the VGA text page.

   procedure Draw
     (Column, Row : Natural;
      Item        : Character;
      Attribute   : Text_Console.Attributes);
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

   -------------
   -- Columns --
   -------------

   function Columns return Positive is (Num_Columns);

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Column, Row : Natural;
      Item        : Character;
      Attribute   : Text_Console.Attributes)
   is
   begin
      Screen_Memory (Row * Num_Columns + Column) :=
        Colour (Attribute) + Character'Pos (Item);
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

   -------------
   -- Operate --
   -------------

   procedure Operate (Operation : Text_Console.Screen_Operation) is
   begin
      if not Taken_Over then
         Take_Over;
      end if;

      Cells.Operate (Operation);
   end Operate;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      if not Taken_Over then
         Take_Over;
      end if;

      Cells.Put (Text);
      Text_Console.Serial.Put (Text);
   end Put;


   ------------
   -- Redraw --
   ------------

   procedure Redraw is
   begin
      Map;
      Cells.Redraw;
   end Redraw;

   ----------
   -- Rows --
   ----------

   function Rows return Positive is (Num_Lines);

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
