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

   Serial : Boolean := False;
   --  Set by Use_Serial_Port

   Copy_Size : constant := 16 * 1024;
   --  More than a 128 by 48 screen shows

   Copy      : String (1 .. Copy_Size);
   Copy_Next : Positive := 1;
   Wrapped   : Boolean := False;
   --  The last Copy_Size characters written, for Replay. Copy_Next is
   --  where the next one goes, and if Wrapped, the oldest one is there.

   procedure Put_Serial (Text : String);
   --  Write Text to the serial port, and keep it in Copy

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
      if Serial then
         return;
      end if;

      Console_Memory := (others => 16#720#);
      Current_Line := 0;
      Current_Column := 0;
      Update_Cursor;
   end Clear;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      if Serial then
         Put_Serial ((1 => Character'Val (10)));
         return;
      end if;

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
      if Serial then
         Put_Serial ((1 => Ch));
         return;
      end if;

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
   -- Put_Serial --
   ----------------

   procedure Put_Serial (Text : String) is
   begin
      Console.Calls.Write_Serial (Text);
      for Ch of Text loop
         Copy (Copy_Next) := Ch;
         if Copy_Next = Copy'Last then
            Copy_Next := Copy'First;
            Wrapped := True;
         else
            Copy_Next := Copy_Next + 1;
         end if;
      end loop;
   end Put_Serial;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (Text   : String;
                         Update : Boolean)
   is
      Offset : Natural := Current_Offset;
   begin
      if Serial then
         Put_Serial (Text);
         return;
      end if;

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
   -- Replay --
   ------------

   procedure Replay
     (Send : not null access
        procedure (Text : System.Storage_Elements.Storage_Array))
   is
      Piece_Size : constant := 1024;

      procedure Send_Text (Text : String);
      --  Give Text to Send, a piece at a time

      ---------------
      -- Send_Text --
      ---------------

      procedure Send_Text (Text : String) is
         use System.Storage_Elements;
         Start : Positive := Text'First;
      begin
         while Start <= Text'Last loop
            declare
               Last  : constant Natural :=
                 Natural'Min (Start + Piece_Size - 1, Text'Last);
               Piece : Storage_Array
                 (1 .. Storage_Offset (Last - Start + 1));
            begin
               for I in Piece'Range loop
                  Piece (I) :=
                    Character'Pos (Text (Start + Natural (I) - 1));
               end loop;
               Send (Piece);
               Start := Last + 1;
            end;
         end loop;
      end Send_Text;

      First : Positive := Copy_Next;
   begin
      if not Serial then
         return;
      elsif not Wrapped then
         Send_Text (Copy (1 .. Copy_Next - 1));
         return;
      end if;

      --  The oldest line is cut, so it starts after the first line feed.

      Send_Text ("[earlier boot output dropped]" & Character'Val (10));

      for I in Copy_Next .. Copy'Last loop
         if Character'Pos (Copy (I)) = 10 then
            First := I + 1;
            exit;
         end if;
      end loop;

      if First <= Copy'Last then
         Send_Text (Copy (First .. Copy'Last));
      end if;
      Send_Text (Copy (1 .. Copy_Next - 1));
   end Replay;

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

   ---------------------
   -- Use_Serial_Port --
   ---------------------

   procedure Use_Serial_Port is
   begin
      Serial := True;
   end Use_Serial_Port;

end Console.IO;
