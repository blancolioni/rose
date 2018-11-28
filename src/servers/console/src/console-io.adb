with System;
with Rose.Words;                        use Rose.Words;

with Console.Calls;

package body Console.IO is

   Current_Line, Current_Column : Natural := 0;
   Current_Colour               : constant Word_16 := 16#700#;

   Console_Start : constant := 16#000B_80A0#;

   Num_Lines   : constant := 24;
   Num_Columns : constant := 80;

   type Console_Memory_Array is
     array (0 .. Num_Lines * Num_Columns - 1) of Word_16;
   for Console_Memory_Array'Component_Size use 16;

   Console_Memory : Console_Memory_Array;
   pragma Import (Ada, Console_Memory);
   for Console_Memory'Address use System'To_Address (Console_Start);

   --  Current_Offset: current index into Console_Memory
   function Current_Offset return Natural
   is (Current_Line * Num_Columns + Current_Column);

   --  Update then cursor position based on the given offset
   --  into the console memory
   procedure Update_Cursor_Position (Offset : Natural);

   --  Scroll the cursor position onto screen if necessary
   procedure Scroll;

   procedure Update_Cursor;

   procedure Put_String (Text   : String;
                         Update : Boolean);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Console_Memory := (others => 16#0720#);
      Current_Line := 0;
      Current_Column := 0;
      Update_Cursor;
   end Clear;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Current_Line := Current_Line + 1;
      Current_Column := 0;
      Scroll;
   end New_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      Put_String (Text, True);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
      Offset : constant Natural := Current_Offset;
   begin
      Console_Memory (Offset) := Current_Colour + Character'Pos (Ch);
      Update_Cursor_Position (Offset + 1);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Put_String (Text, False);
      New_Line;
   end Put_Line;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Text   : String;
                         Update : Boolean)
   is
      Offset : Natural := Current_Offset;
   begin
      for I in Text'Range loop
         if Character'Pos (Text (I)) = 10 then
            New_Line;
            Offset := Current_Offset;
         else
            Console_Memory (Offset) :=
              Current_Colour + Character'Pos (Text (I));
            Offset := Offset + 1;
         end if;
      end loop;
      if Update then
         Update_Cursor_Position (Offset);
      end if;
   end Put_String;

   ------------
   -- Scroll --
   ------------

   procedure Scroll is
   begin
      while Current_Line >= Num_Lines loop
         Console_Memory (0 .. (Num_Lines - 1) * Num_Columns - 1) :=
           Console_Memory (Num_Columns .. Num_Lines * Num_Columns - 1);
         Console_Memory ((Num_Lines - 1) * Num_Columns ..
                           Num_Lines * Num_Columns - 1) :=
           (others => 16#0720#);

         Current_Line := Current_Line - 1;
      end loop;
      Update_Cursor;
   end Scroll;

   -------------------
   -- Update_Cursor --
   -------------------

   procedure Update_Cursor is
      Position    : constant Natural :=
        (Current_Line + 1) * Num_Columns + Current_Column;
   begin
      Console.Calls.Send_Cursor_Position
        (Rose.Words.Word_16 (Position));
   end Update_Cursor;

   ----------------------------
   -- Update_Cursor_Position --
   ----------------------------

   procedure Update_Cursor_Position (Offset : Natural) is
   begin
      Current_Line := Offset / Num_Columns;
      Current_Column := Offset mod Num_Columns;
      Scroll;
   end Update_Cursor_Position;

end Console.IO;
