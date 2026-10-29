with System;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;                        use Rose.Words;

with Rose.Devices.Port_IO;

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

   Saved_Screen  : Screen_Array := (others => Blank);
   Line, Column  : Natural := 0;
   Taken_Over    : Boolean := False;

   procedure Take_Over;
   procedure New_Line;
   procedure Scroll;
   procedure Set_Cursor;

   ---------
   -- Map --
   ---------

   procedure Map is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Screen_Page_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Map;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Line := Line + 1;
      Column := 0;
      Scroll;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      if not Taken_Over then
         Take_Over;
      end if;

      for Ch of Text loop
         if Character'Pos (Ch) = 10 then
            New_Line;
         elsif Character'Pos (Ch) = 8 then

            --  Backspace moves the cursor back without erasing, and to
            --  the end of the line above from the start of a line, so
            --  that a line that wrapped can be erased (#397).

            if Column > 0 then
               Column := Column - 1;
            elsif Line > 0 then
               Line := Line - 1;
               Column := Num_Columns - 1;
            end if;
         elsif Character'Pos (Ch) = 9 then
            Column := (Column / 8 + 1) * 8;
            if Column >= Num_Columns then
               New_Line;
            end if;
         elsif Character'Pos (Ch) = 13 then
            Column := 0;
         elsif Character'Pos (Ch) < 32 or else Character'Pos (Ch) = 127 then

            --  Other control characters draw nothing.

            null;
         else
            declare
               Offset : constant Natural := Line * Num_Columns + Column;
               Cell   : constant Word_16 := Colour + Character'Pos (Ch);
            begin
               Saved_Screen (Offset) := Cell;
               Screen_Memory (Offset) := Cell;
            end;
            Column := Column + 1;
            if Column = Num_Columns then
               New_Line;
            end if;
         end if;
      end loop;

      Set_Cursor;
   end Put;

   ------------
   -- Redraw --
   ------------

   procedure Redraw is
   begin
      Map;
      Screen_Memory := Saved_Screen;
      Set_Cursor;
   end Redraw;

   ------------
   -- Scroll --
   ------------

   procedure Scroll is
   begin
      if Line >= Num_Lines then
         while Line >= Num_Lines loop
            Saved_Screen (0 .. (Num_Lines - 1) * Num_Columns - 1) :=
              Saved_Screen (Num_Columns .. Num_Lines * Num_Columns - 1);
            Saved_Screen ((Num_Lines - 1) * Num_Columns ..
                            Num_Lines * Num_Columns - 1) :=
              (others => Blank);
            Line := Line - 1;
         end loop;
         Screen_Memory := Saved_Screen;
      end if;
   end Scroll;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor is
      Position : constant Natural := (Line + 1) * Num_Columns + Column;
   begin
      --  The cursor position counts the status line.  Index 16#0E# of
      --  the CRT controller is its high byte, and 16#0F# its low byte.

      Rose.Devices.Port_IO.Port_Out_8
        (Cursor_Out_Cap,
         ((0, 16#0E#), (1, Word_8 (Position / 256)),
          (0, 16#0F#), (1, Word_8 (Position mod 256))));
   end Set_Cursor;

   ---------------
   -- Take_Over --
   ---------------

   procedure Take_Over is
      Position : Natural;
   begin
      Saved_Screen := Screen_Memory;

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
         Line := Num_Lines - 1;
         Column := 0;
      else
         Line := Position / Num_Columns - 1;
         Column := Position mod Num_Columns;
      end if;

      Taken_Over := True;
   end Take_Over;

end VGA_Console.Screen;
