--  Kilo, a full-screen text editor for the console, ported to Ada from
--  kilo (#735). Where kilo wrote VT100 escape sequences and read them
--  back, this uses the console's Terminal (#734). Syntax highlighting is
--  left out, and only printable ASCII and Tab can be typed (#140).
--
--  Kilo -- A very simple editor in less than 1-kilo lines of code (as
--          counted by "cloc"). Does not depend on libcurses, directly
--          emits VT100 escapes on the terminal.
--
--  -----------------------------------------------------------------------
--
--  Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
--
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--   *  Redistributions of source code must retain the above copyright
--      notice, this list of conditions and the following disclaimer.
--
--   *  Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in
--      the documentation and/or other materials provided with the
--      distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

with Ada.Streams;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Writer;
with Rose.Keys;
with Rose.Words;

procedure Rose.Core.Editors.Kilo
  (Console : in out Rose.Interfaces.Terminal.Terminal_Object'Class;
   File    : in out
     Rose.Interfaces.Transactional_Writable_File
       .Transactional_Writable_File_Object'Class)
is
   use Ada.Strings.Unbounded;
   use type Rose.Words.Word;

   subtype Key_Code is Rose.Keys.Key_Code;

   subtype Reader_Class is
     Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class;
   subtype Writer_Class is
     Rose.Interfaces.Releasable_Writer.Releasable_Writer_Object'Class;

   package Row_Vectors is
     new Ada.Containers.Vectors (Natural, Unbounded_String);

   Kilo_Version : constant String := "0.0.1";
   Tab_Stop     : constant := 8;
   Quit_Times   : constant := 3;
   --  How many more times Ctrl-Q must be pressed to quit when there are
   --  unsaved changes.

   Ctrl_C    : constant Key_Code := 3;
   Ctrl_F    : constant Key_Code := 6;
   Backspace : constant Key_Code := 8;
   Tab       : constant Key_Code := 9;
   Enter     : constant Key_Code := 10;
   Ctrl_L    : constant Key_Code := 12;
   Ctrl_Q    : constant Key_Code := 17;
   Ctrl_S    : constant Key_Code := 19;
   Escape    : constant Key_Code := 27;
   Del       : constant Key_Code := 127;
   --  Return is a line feed, and Backspace is Ctrl-H (Rose.Keys).

   Rows        : Row_Vectors.Vector;
   --  The lines of the file, without their line feeds.

   Cursor_Row  : Natural := 0;
   Cursor_Col  : Natural := 0;
   --  Where the cursor is in the file: a row, which can be the one after
   --  the last, and a character in it.

   Row_Offset  : Natural := 0;
   Col_Offset  : Natural := 0;
   --  The first row of the file on the screen, and the first column of
   --  the rows as they are drawn.

   Screen_Rows : Natural := 1;
   Screen_Cols : Natural := 1;
   --  The size of the text: the screen, less two rows for the status bar
   --  and the message.

   Dirty       : Boolean := False;
   --  The rows have changes that haven't been saved.

   Message     : Unbounded_String;
   --  The message under the status bar. Kilo shows it for five seconds;
   --  here it is shown until the next key.

   Quit_Count  : Natural := Quit_Times;

   function Current_Line return String;
   --  The row the cursor is on, or "" after the last row.

   procedure Delete_Char;
   --  Delete the character before the cursor, joining the row to the one
   --  before it at the start of a row.

   procedure Find;
   --  Find text as it is typed: the arrows go to the next or the previous
   --  match, Escape goes back to where the cursor was, and Enter stays.

   function Image (Value : Natural) return String;
   --  Value in decimal, without a leading space.

   procedure Insert_Char (C : Character);
   --  Insert C at the cursor.

   procedure Insert_Newline;
   --  Split the row at the cursor.

   function Line_Length (Row : Natural) return Natural;
   --  The number of characters in Row, or 0 after the last row.

   procedure Move_Cursor (Key : Key_Code);
   --  Move the cursor for an arrow key, Home or End.

   procedure Open_File;
   --  Read the rows from the file.

   procedure Process_Key
     (Key  : Key_Code;
      Done : out Boolean);
   --  Do what Key does. Done is True if it quits.

   procedure Put (Text : String);
   --  Write Text at the cursor.

   procedure Refresh_Screen;
   --  Draw the rows, the status bar and the message, and put the cursor
   --  where it is in the file.

   function Render (Line : String) return String;
   --  Line as it is drawn: a Tab is spaces to the next tab stop, and a
   --  character that isn't printable ASCII is a question mark.

   function Render_Column
     (Line : String;
      Col  : Natural)
      return Natural;
   --  The column at which character Col of Line is drawn.

   procedure Save;
   --  Write the rows to the file, each with a line feed, and commit it
   --  (ADR 0026).

   procedure Scroll;
   --  Move the offsets so that the cursor is on the screen.

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line return String is
   begin
      if Cursor_Row < Natural (Rows.Length) then
         return To_String (Rows.Element (Cursor_Row));
      else
         return "";
      end if;
   end Current_Line;

   -----------------
   -- Delete_Char --
   -----------------

   procedure Delete_Char is
   begin
      if Cursor_Row >= Natural (Rows.Length)
        or else (Cursor_Col = 0 and then Cursor_Row = 0)
      then
         return;
      end if;

      if Cursor_Col = 0 then
         declare
            Previous : constant Unbounded_String :=
              Rows.Element (Cursor_Row - 1);
         begin
            Rows.Replace_Element
              (Cursor_Row - 1, Previous & Rows.Element (Cursor_Row));
            Rows.Delete (Cursor_Row);
            Cursor_Row := Cursor_Row - 1;
            Cursor_Col := Length (Previous);
         end;
      else
         declare
            Line : Unbounded_String := Rows.Element (Cursor_Row);
         begin
            Delete (Line, Cursor_Col, Cursor_Col);
            Rows.Replace_Element (Cursor_Row, Line);
            Cursor_Col := Cursor_Col - 1;
         end;
      end if;
      Dirty := True;
   end Delete_Char;

   ----------
   -- Find --
   ----------

   procedure Find is
      Query            : Unbounded_String;
      Last_Match       : Integer := -1;
      Direction        : Integer := 0;
      Saved_Row        : constant Natural := Cursor_Row;
      Saved_Col        : constant Natural := Cursor_Col;
      Saved_Row_Offset : constant Natural := Row_Offset;
      Saved_Col_Offset : constant Natural := Col_Offset;
      Key              : Rose.Words.Word;
      Modifiers        : Rose.Words.Word;
   begin
      loop
         Message :=
           "Search: " & Query & " (Use ESC/Arrows/Enter)";
         Refresh_Screen;
         Console.Read_Key (Key, Modifiers);

         if (Modifiers and Rose.Keys.Alt) /= 0 then
            null;
         elsif Key = Backspace or else Key = Del
           or else Key = Rose.Keys.Delete
         then
            if Length (Query) > 0 then
               Delete (Query, Length (Query), Length (Query));
            end if;
            Last_Match := -1;
         elsif Key = Escape or else Key = Enter then
            if Key = Escape then
               Cursor_Row := Saved_Row;
               Cursor_Col := Saved_Col;
               Row_Offset := Saved_Row_Offset;
               Col_Offset := Saved_Col_Offset;
            end if;
            Message := Null_Unbounded_String;
            return;
         elsif Key = Rose.Keys.Right or else Key = Rose.Keys.Down then
            Direction := 1;
         elsif Key = Rose.Keys.Left or else Key = Rose.Keys.Up then
            Direction := -1;
         elsif Key in 32 .. 126 then
            Append (Query, Character'Val (Key));
            Last_Match := -1;
         end if;

         if Last_Match = -1 then
            Direction := 1;
         end if;

         if Direction /= 0 then
            declare
               Count   : constant Natural := Natural (Rows.Length);
               Current : Integer := Last_Match;
               Found   : Natural;
            begin
               for I in 1 .. Count loop
                  Current := Current + Direction;
                  if Current = -1 then
                     Current := Count - 1;
                  elsif Current = Count then
                     Current := 0;
                  end if;

                  Found :=
                    (if Length (Query) = 0 then 1
                     else Index (Rows.Element (Current), To_String (Query)));

                  if Found /= 0 then

                     --  The row of the match goes at the top of the
                     --  screen, as in kilo.

                     Last_Match := Current;
                     Cursor_Row := Current;
                     Cursor_Col := Found - 1;
                     Row_Offset := Current;
                     exit;
                  end if;
               end loop;
            end;
            Direction := 0;
         end if;
      end loop;
   end Find;

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   -----------------
   -- Insert_Char --
   -----------------

   procedure Insert_Char (C : Character) is
   begin
      if Cursor_Row = Natural (Rows.Length) then
         Rows.Append (Null_Unbounded_String);
      end if;

      declare
         Line : Unbounded_String := Rows.Element (Cursor_Row);
      begin
         Insert (Line, Cursor_Col + 1, (1 => C));
         Rows.Replace_Element (Cursor_Row, Line);
      end;
      Cursor_Col := Cursor_Col + 1;
      Dirty := True;
   end Insert_Char;

   --------------------
   -- Insert_Newline --
   --------------------

   procedure Insert_Newline is
   begin
      if Cursor_Row = Natural (Rows.Length) then
         Rows.Append (Null_Unbounded_String);
      else
         declare
            Line  : constant String := Current_Line;
            Split : constant Positive := Line'First + Cursor_Col;
            --  The first character of the new row.
         begin
            Rows.Replace_Element
              (Cursor_Row,
               To_Unbounded_String (Line (Line'First .. Split - 1)));
            Rows.Insert
              (Cursor_Row + 1,
               To_Unbounded_String (Line (Split .. Line'Last)));
         end;
      end if;
      Cursor_Row := Cursor_Row + 1;
      Cursor_Col := 0;
      Dirty := True;
   end Insert_Newline;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length (Row : Natural) return Natural is
   begin
      if Row < Natural (Rows.Length) then
         return Length (Rows.Element (Row));
      else
         return 0;
      end if;
   end Line_Length;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor (Key : Key_Code) is
      Count : constant Natural := Natural (Rows.Length);
   begin
      if Key = Rose.Keys.Left then
         if Cursor_Col > 0 then
            Cursor_Col := Cursor_Col - 1;
         elsif Cursor_Row > 0 then
            Cursor_Row := Cursor_Row - 1;
            Cursor_Col := Line_Length (Cursor_Row);
         end if;
      elsif Key = Rose.Keys.Right then
         if Cursor_Row < Count then
            if Cursor_Col < Line_Length (Cursor_Row) then
               Cursor_Col := Cursor_Col + 1;
            else
               Cursor_Row := Cursor_Row + 1;
               Cursor_Col := 0;
            end if;
         end if;
      elsif Key = Rose.Keys.Up then
         if Cursor_Row > 0 then
            Cursor_Row := Cursor_Row - 1;
         end if;
      elsif Key = Rose.Keys.Down then
         if Cursor_Row < Count then
            Cursor_Row := Cursor_Row + 1;
         end if;
      elsif Key = Rose.Keys.Home then
         Cursor_Col := 0;
      elsif Key = Rose.Keys.End_Key then
         Cursor_Col := Line_Length (Cursor_Row);
      end if;

      --  A shorter row moves the cursor back to its end.

      Cursor_Col := Natural'Min (Cursor_Col, Line_Length (Cursor_Row));
   end Move_Cursor;

   ---------------
   -- Open_File --
   ---------------

   --  The file is a version, which starts as a copy of the file, or empty
   --  if it was made for kilo (ADR 0026). A line feed ends a row, and a
   --  carriage return before it is dropped, as in kilo.

   procedure Open_File is
      use Ada.Streams;
      Reader : Reader_Class := File.Open_Reader;
      Buffer : Stream_Element_Array (1 .. 4096);
      Last   : Stream_Element_Offset;
      Line   : Unbounded_String;
   begin
      loop
         Reader.Read (Buffer, Last);
         exit when Last < Buffer'First;
         for Item of Buffer (Buffer'First .. Last) loop
            declare
               C : constant Character := Character'Val (Item);
            begin
               if C = ASCII.LF then
                  if Length (Line) > 0
                    and then Element (Line, Length (Line)) = ASCII.CR
                  then
                     Delete (Line, Length (Line), Length (Line));
                  end if;
                  Rows.Append (Line);
                  Line := Null_Unbounded_String;
               else
                  Append (Line, C);
               end if;
            end;
         end loop;
      end loop;
      Reader.Release;

      if Length (Line) > 0 then
         Rows.Append (Line);
      end if;
   end Open_File;

   -----------------
   -- Process_Key --
   -----------------

   procedure Process_Key
     (Key  : Key_Code;
      Done : out Boolean)
   is
   begin
      Done := False;

      if Key = Enter then
         Insert_Newline;
      elsif Key = Ctrl_C or else Key = Ctrl_L or else Key = Escape then

         --  Kilo ignores Ctrl-C, since losing changes shouldn't be so
         --  easy.

         null;
      elsif Key = Ctrl_Q then
         if Dirty and then Quit_Count > 0 then
            Message :=
              To_Unbounded_String
                ("WARNING!!! File has unsaved changes. Press Ctrl-Q "
                 & Image (Quit_Count) & " more times to quit.");
            Quit_Count := Quit_Count - 1;
            return;
         end if;
         Done := True;
      elsif Key = Ctrl_S then
         Save;
      elsif Key = Ctrl_F then
         Find;
      elsif Key = Backspace or else Key = Del then
         Delete_Char;
      elsif Key = Rose.Keys.Delete then

         --  Kilo treats Delete as Backspace. Here it deletes the
         --  character under the cursor.

         if Cursor_Col < Line_Length (Cursor_Row)
           or else Cursor_Row + 1 < Natural (Rows.Length)
         then
            Move_Cursor (Rose.Keys.Right);
            Delete_Char;
         end if;
      elsif Key = Rose.Keys.Page_Up or else Key = Rose.Keys.Page_Down then
         if Key = Rose.Keys.Page_Up then
            Cursor_Row := Row_Offset;
         else
            Cursor_Row :=
              Natural'Min
                (Row_Offset + Screen_Rows - 1, Natural (Rows.Length));
         end if;
         for I in 1 .. Screen_Rows loop
            Move_Cursor
              (if Key = Rose.Keys.Page_Up then Rose.Keys.Up
               else Rose.Keys.Down);
         end loop;
      elsif Key = Rose.Keys.Up or else Key = Rose.Keys.Down
        or else Key = Rose.Keys.Left or else Key = Rose.Keys.Right
        or else Key = Rose.Keys.Home or else Key = Rose.Keys.End_Key
      then
         Move_Cursor (Key);
      elsif Key = Tab or else Key in 32 .. 126 then
         Insert_Char (Character'Val (Key));
      end if;

      Quit_Count := Quit_Times;
   end Process_Key;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Text'Length)
        with Import, Address => Text'Address;
   begin
      if Text'Length > 0 then
         Console.Write (Buffer);
      end if;
   end Put;

   --------------------
   -- Refresh_Screen --
   --------------------

   procedure Refresh_Screen is
      Count : constant Natural := Natural (Rows.Length);
   begin
      Scroll;

      for Y in 0 .. Screen_Rows - 1 loop
         Console.Move_Cursor (0, Rose.Words.Word (Y));
         if Row_Offset + Y < Count then
            declare
               Text  : constant String :=
                 Render (To_String (Rows.Element (Row_Offset + Y)));
               First : constant Positive := Text'First + Col_Offset;
            begin
               if First <= Text'Last then
                  Put (Text (First .. Natural'Min
                                        (Text'Last,
                                         First + Screen_Cols - 1)));
               end if;
            end;
         elsif Count = 0 and then Y = Screen_Rows / 3 then
            declare
               Welcome : constant String :=
                 "Kilo editor -- version " & Kilo_Version;
               Padding : constant Natural :=
                 (if Screen_Cols > Welcome'Length
                  then (Screen_Cols - Welcome'Length) / 2
                  else 0);
            begin
               if Padding > 0 then
                  Put ("~" & (1 .. Padding - 1 => ' '));
               end if;
               Put (Welcome (Welcome'First
                             .. Natural'Min
                               (Welcome'Last,
                                Welcome'First + Screen_Cols - 1)));
            end;
         else
            Put ("~");
         end if;
         Console.Clear_To_End_Of_Line;
      end loop;

      --  The status bar, in reverse video: the number of lines, whether
      --  they have changed, and the cursor's row. The file's name isn't
      --  shown, since kilo is given the file and not its name.

      declare
         Left  : constant String :=
           "kilo - " & Image (Count) & " lines"
           & (if Dirty then " (modified)" else "");
         Right : constant String :=
           Image (Cursor_Row + 1) & "/" & Image (Count);
         Bar   : String (1 .. Screen_Cols) := (others => ' ');
      begin
         for I in Left'Range loop
            exit when I - Left'First + 1 > Bar'Last;
            Bar (I - Left'First + 1) := Left (I);
         end loop;
         if Left'Length + Right'Length < Bar'Length then
            Bar (Bar'Last - Right'Length + 1 .. Bar'Last) := Right;
         end if;
         Console.Move_Cursor (0, Rose.Words.Word (Screen_Rows));
         Console.Set_Attributes (Bold => False, Reverse_Video => True);
         Put (Bar);
         Console.Set_Attributes (Bold => False, Reverse_Video => False);
      end;

      --  The message, which stops short of the last column so that
      --  nothing is written to the last cell of the screen.

      Console.Move_Cursor (0, Rose.Words.Word (Screen_Rows + 1));
      Console.Clear_To_End_Of_Line;
      declare
         Text : constant String := To_String (Message);
      begin
         Put (Text (Text'First
                    .. Natural'Min (Text'Last, Text'First + Screen_Cols - 2)));
      end;

      Console.Move_Cursor
        (Rose.Words.Word
           (Render_Column (Current_Line, Cursor_Col) - Col_Offset),
         Rose.Words.Word (Cursor_Row - Row_Offset));
   end Refresh_Screen;

   ------------
   -- Render --
   ------------

   function Render (Line : String) return String is
      Result : Unbounded_String;
   begin
      for C of Line loop
         if C = ASCII.HT then
            loop
               Append (Result, ' ');
               exit when Length (Result) mod Tab_Stop = 0;
            end loop;
         elsif C in ' ' .. '~' then
            Append (Result, C);
         else
            Append (Result, '?');
         end if;
      end loop;
      return To_String (Result);
   end Render;

   -------------------
   -- Render_Column --
   -------------------

   function Render_Column
     (Line : String;
      Col  : Natural)
      return Natural
   is
      Column : Natural := 0;
   begin
      for C of Line (Line'First .. Line'First + Col - 1) loop
         if C = ASCII.HT then
            Column := Column + Tab_Stop - Column mod Tab_Stop;
         else
            Column := Column + 1;
         end if;
      end loop;
      return Column;
   end Render_Column;

   ----------
   -- Save --
   ----------

   --  The version is emptied with Rewrite, written, and committed, so
   --  that the file changes at once (ADR 0009, section 4, and ADR 0026).
   --  A write past the largest file fails, and nothing is committed.

   procedure Save is
      use Ada.Streams;
      Total : Natural := 0;
   begin
      declare
         Writer : Writer_Class := File.Rewrite;
         Buffer : Stream_Element_Array (1 .. 4096);
         Last   : Stream_Element_Offset := 0;
      begin
         for Line of Rows loop
            for C of String'(To_String (Line) & ASCII.LF) loop
               Last := Last + 1;
               Buffer (Last) := Character'Pos (C);
               if Last = Buffer'Last then
                  Writer.Write (Buffer);
                  Last := 0;
               end if;
               Total := Total + 1;
            end loop;
         end loop;
         if Last > 0 then
            Writer.Write (Buffer (1 .. Last));
         end if;
         Writer.Release;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Writer.Release;
            raise;
      end;

      File.Commit;
      Dirty := False;
      Message :=
        To_Unbounded_String (Image (Total) & " bytes written on disk");
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Message :=
           To_Unbounded_String
             ("Can't save! A file holds at most 64 KiB");
   end Save;

   ------------
   -- Scroll --
   ------------

   procedure Scroll is
      Column : constant Natural := Render_Column (Current_Line, Cursor_Col);
   begin
      if Cursor_Row < Row_Offset then
         Row_Offset := Cursor_Row;
      elsif Cursor_Row >= Row_Offset + Screen_Rows then
         Row_Offset := Cursor_Row - Screen_Rows + 1;
      end if;

      if Column < Col_Offset then
         Col_Offset := Column;
      elsif Column >= Col_Offset + Screen_Cols then
         Col_Offset := Column - Screen_Cols + 1;
      end if;
   end Scroll;

   Columns, Lines : Rose.Words.Word;
   Key            : Rose.Words.Word;
   Modifiers      : Rose.Words.Word;
   Done           : Boolean := False;

begin
   Console.Disable_Line_Editing;
   Console.Clear;
   Console.Get_Size (Columns, Lines);
   Screen_Cols := Natural'Max (Natural (Columns), 2);
   Screen_Rows := Natural'Max (Natural (Lines), 3) - 2;

   Open_File;
   Message :=
     To_Unbounded_String
       ("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

   loop
      Refresh_Screen;
      Console.Read_Key (Key, Modifiers);
      Message := Null_Unbounded_String;

      --  A key typed with Alt does nothing.

      if (Modifiers and Rose.Keys.Alt) = 0 then
         Process_Key (Key, Done);
      end if;
      exit when Done;
   end loop;

   Console.Clear;
   Console.Enable_Line_Editing;
end Rose.Core.Editors.Kilo;
