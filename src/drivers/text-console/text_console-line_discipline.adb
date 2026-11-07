with System.Storage_Elements;

package body Text_Console.Line_Discipline is

   use System.Storage_Elements;
   use type Rose.Keys.Key_Code;

   EOT : constant Storage_Element := 4;
   ETX : constant Storage_Element := 3;
   BS  : constant Storage_Element := 8;
   HT  : constant Storage_Element := 9;
   LF  : constant Storage_Element := 10;
   NAK : constant Storage_Element := 21;
   ETB : constant Storage_Element := 23;
   DEL : constant Storage_Element := 127;

   Line_Editing : Boolean := True;

   Input_Buffer : Storage_Array (1 .. 4096);
   Input_Last   : Storage_Count := 0;
   Line_Start   : Storage_Count := 0;
   --  Input_Buffer (1 .. Line_Start) are the lines that are complete and
   --  haven't been read, and Input_Buffer (Line_Start + 1 .. Input_Last)
   --  is the line being typed.

   End_Of_File  : Boolean := False;
   --  An end of file that hasn't been read yet (ADR 0011, section 3).

   Echo_Input   : Boolean := True;

   type Key_Press is
      record
         Key       : Rose.Keys.Key_Code := 0;
         Modifiers : Rose.Keys.Modifier_Set := 0;
      end record;

   Max_Keys : constant := 256;

   type Key_Index is mod Max_Keys;

   Keys       : array (Key_Index) of Key_Press;
   First_Key  : Key_Index := 0;
   Key_Count  : Natural range 0 .. Max_Keys := 0;
   --  The keys typed with line editing off that haven't been read, from
   --  Keys (First_Key). A key that is typed when it is full is dropped.

   function Is_ASCII (Key : Rose.Keys.Key_Code) return Boolean
   is (Key < 128);

   procedure Append (Element : Storage_Element);
   --  Add Element to the line being typed, and echo it.

   procedure Echo (Text : String);

   procedure Rub_Out;
   --  Remove the last character of the line being typed, and erase it
   --  from the screen.

   ------------
   -- Append --
   ------------

   procedure Append (Element : Storage_Element) is
   begin
      if Input_Last < Input_Buffer'Last then
         Input_Last := Input_Last + 1;
         Input_Buffer (Input_Last) := Element;
         Echo ((1 => Character'Val (Element)));
      end if;

      if Element = LF or else Input_Last = Input_Buffer'Last then
         Line_Start := Input_Last;
      end if;
   end Append;

   ----------
   -- Echo --
   ----------

   procedure Echo (Text : String) is
   begin
      if Echo_Input then
         Put (Text);
      end if;
   end Echo;

   -------------
   -- Editing --
   -------------

   function Editing return Boolean is (Line_Editing);

   ---------------
   -- Key_Ready --
   ---------------

   function Key_Ready return Boolean is
   begin
      return not Line_Editing and then Key_Count > 0;
   end Key_Ready;

   ------------
   -- On_Key --
   ------------

   procedure On_Key
     (Key       : Rose.Keys.Key_Code;
      Modifiers : Rose.Keys.Modifier_Set)
   is
   begin
      if not Line_Editing then
         if Key_Count < Max_Keys then
            Keys (First_Key + Key_Index (Key_Count)) := (Key, Modifiers);
            Key_Count := Key_Count + 1;
         end if;
         return;
      end if;

      --  Line editing builds lines from ASCII characters. Named keys,
      --  other characters, and keys typed with Alt are ignored, until
      --  the console is a stream of code points (#140).

      if not Is_ASCII (Key)
        or else (Modifiers and Rose.Keys.Alt) /= 0
      then
         return;
      end if;

      declare
         Element : constant Storage_Element := Storage_Element (Key);
      begin
         case Element is
            when LF | HT | 32 .. 126 =>
               Append (Element);

            when BS | DEL =>
               Rub_Out;

            when NAK =>
               while Input_Last > Line_Start loop
                  Rub_Out;
               end loop;

            when ETB =>
               while Input_Last > Line_Start
                 and then Input_Buffer (Input_Last) = Character'Pos (' ')
               loop
                  Rub_Out;
               end loop;
               while Input_Last > Line_Start
                 and then Input_Buffer (Input_Last) /= Character'Pos (' ')
               loop
                  Rub_Out;
               end loop;

            when ETX =>

               --  Discard the line. Once ADR 0006 is implemented, this
               --  also sends Interrupt to the job that is reading.

               Input_Last := Line_Start;
               Echo ("^C" & Character'Val (10));

            when EOT =>

               --  End of file (ADR 0011, section 2). A line that is
               --  partly typed is returned without a line break, and a
               --  second Ctrl-D then ends the input.

               if Input_Last = Line_Start then
                  End_Of_File := True;
               end if;
               Line_Start := Input_Last;

            when others =>

               --  Other control characters, including Escape, and Ctrl-Z
               --  until ADR 0006 gives it Stop, are ignored.

               null;
         end case;
      end;
   end On_Key;

   -----------
   -- Ready --
   -----------

   function Ready return Boolean is
   begin
      if Line_Editing then
         return Line_Start > 0 or else End_Of_File;
      else
         for I in 0 .. Key_Count - 1 loop
            if Is_ASCII (Keys (First_Key + Key_Index (I)).Key) then
               return True;
            end if;
         end loop;
         return False;
      end if;
   end Ready;

   -------------
   -- Rub_Out --
   -------------

   procedure Rub_Out is
   begin
      if Input_Last > Line_Start then
         Input_Last := Input_Last - 1;
         Echo ((Character'Val (BS), ' ', Character'Val (BS)));
      end if;
   end Rub_Out;

   --------------
   -- Set_Echo --
   --------------

   procedure Set_Echo (Enabled : Boolean) is
   begin
      Echo_Input := Enabled;
   end Set_Echo;

   -----------------
   -- Set_Editing --
   -----------------

   procedure Set_Editing (Enabled : Boolean) is
   begin
      if Enabled and then not Line_Editing then
         Key_Count := 0;
      elsif not Enabled and then Line_Editing then
         Input_Last := Line_Start;
      end if;
      Line_Editing := Enabled;
   end Set_Editing;

   ----------
   -- Take --
   ----------

   procedure Take
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Last := Buffer'First - 1;

      if not Line_Editing then
         while Key_Count > 0 and then Last < Buffer'Last loop
            declare
               Key : constant Rose.Keys.Key_Code := Keys (First_Key).Key;
            begin
               First_Key := First_Key + 1;
               Key_Count := Key_Count - 1;
               if Is_ASCII (Key) then
                  Last := Last + 1;
                  Buffer (Last) := Ada.Streams.Stream_Element (Key);
               end if;
            end;
         end loop;
      elsif Line_Start > 0 then
         declare
            Count  : constant Storage_Count :=
                       Storage_Count'Min (Buffer'Length, Line_Start);
            Target : Storage_Array (1 .. Count)
              with Import, Address => Buffer'Address;
            --  The start of Buffer, as the line is kept.
         begin
            Last :=
              Buffer'First + Ada.Streams.Stream_Element_Offset (Count) - 1;
            Target := Input_Buffer (1 .. Count);
            Input_Buffer (1 .. Input_Last - Count) :=
              Input_Buffer (Count + 1 .. Input_Last);
            Input_Last := Input_Last - Count;
            Line_Start := Line_Start - Count;
         end;
      else

         --  A Read that returns nothing is the end of the stream.

         End_Of_File := False;
      end if;
   end Take;

   --------------
   -- Take_Key --
   --------------

   procedure Take_Key
     (Key       : out Rose.Keys.Key_Code;
      Modifiers : out Rose.Keys.Modifier_Set)
   is
   begin
      Key := Keys (First_Key).Key;
      Modifiers := Keys (First_Key).Modifiers;
      First_Key := First_Key + 1;
      Key_Count := Key_Count - 1;
   end Take_Key;

end Text_Console.Line_Discipline;
