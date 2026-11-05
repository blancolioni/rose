with System.Storage_Elements;

with Rose.Keys;
with Rose.Words;

procedure Rose.Core.Keytest
  (Console : in out Rose.Interfaces.Terminal.Terminal_Object'Class)
is
   use Rose.Words;

   Columns, Rows : Word;
   Last_Column   : Natural;
   Last_Row      : Natural;

   X, Y          : Natural;
   --  Where the mark is.

   function Image (Value : Natural) return String;
   --  Value in decimal, without a leading space.

   function Key_Name
     (Key       : Rose.Keys.Key_Code;
      Modifiers : Rose.Keys.Modifier_Set)
      return String;
   --  The key's name, as Up, 'a' or U+00E9, after its modifiers, as in
   --  Ctrl-Left.

   procedure Put (Text : String);
   --  Write Text at the cursor.

   procedure Put_At
     (Column, Row : Natural;
      Text        : String);
   --  Write Text from Column, Row, cut at the edge of the screen.

   procedure Show_Status (Name : String);
   --  Write the last key's name and where the mark is on the bottom line.

   -----------
   -- Image --
   -----------

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --------------
   -- Key_Name --
   --------------

   function Key_Name
     (Key       : Rose.Keys.Key_Code;
      Modifiers : Rose.Keys.Modifier_Set)
      return String
   is
      use Rose.Keys;

      Hex : constant String := "0123456789ABCDEF";

      function Base_Name return String;

      function Prefix return String
      is ((if (Modifiers and Control) /= 0 then "Ctrl-" else "")
          & (if (Modifiers and Alt) /= 0 then "Alt-" else "")
          & (if (Modifiers and Shift) /= 0 then "Shift-" else ""));

      ---------------
      -- Base_Name --
      ---------------

      function Base_Name return String is
      begin
         case Key is
            when Up        => return "Up";
            when Down      => return "Down";
            when Left      => return "Left";
            when Right     => return "Right";
            when Home      => return "Home";
            when End_Key   => return "End";
            when Page_Up   => return "Page_Up";
            when Page_Down => return "Page_Down";
            when Insert    => return "Insert";
            when Delete    => return "Delete";
            when F1 .. F1 + 11 =>
               return "F" & Image (Natural (Key - F1 + 1));
            when 32 .. 126 =>
               return (''', Character'Val (Key), ''');
            when others =>
               declare
                  Text  : String (1 .. 6) := "U+0000";
                  Value : Word := Key;
               begin
                  for I in reverse 3 .. 6 loop
                     Text (I) := Hex (Natural (Value mod 16) + 1);
                     Value := Value / 16;
                  end loop;
                  return Text;
               end;
         end case;
      end Base_Name;

   begin
      return Prefix & Base_Name;
   end Key_Name;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
      Buffer : System.Storage_Elements.Storage_Array (1 .. Text'Length)
        with Import, Address => Text'Address;
   begin
      Console.Write (Buffer);
   end Put;

   ------------
   -- Put_At --
   ------------

   procedure Put_At
     (Column, Row : Natural;
      Text        : String)
   is
   begin
      if Column <= Last_Column and then Row <= Last_Row then
         Console.Move_Cursor (Word (Column), Word (Row));
         Put (Text (Text'First
                    .. Natural'Min
                      (Text'Last, Text'First + Last_Column - Column)));
      end if;
   end Put_At;

   -----------------
   -- Show_Status --
   -----------------

   procedure Show_Status (Name : String) is
      Row : constant Natural := Natural'Max (Last_Row - 1, 0);
   begin
      Console.Move_Cursor (2, Word (Row));
      Console.Clear_To_End_Of_Line;
      Put_At (2, Row,
              "key " & Name & " at " & Image (X) & "," & Image (Y));
      Console.Move_Cursor (Word (X), Word (Y));
   end Show_Status;

begin
   Console.Disable_Line_Editing;
   Console.Clear;
   Console.Get_Size (Columns, Rows);
   Last_Column := Natural (Columns) - 1;
   Last_Row := Natural (Rows) - 1;

   Put_At (0, 0, "+");
   Put_At (Last_Column, 0, "+");
   Put_At (0, Last_Row, "+");
   Put_At (Last_Column, Last_Row, "+");

   Console.Set_Attributes (Bold => True, Reverse_Video => False);
   Put_At (2, 1, "keytest " & Image (Natural (Columns)) & "x"
           & Image (Natural (Rows)));
   Console.Set_Attributes (Bold => False, Reverse_Video => True);
   Put (" arrows move, q quits ");
   Console.Set_Attributes (Bold => False, Reverse_Video => False);

   X := Natural (Columns) / 2;
   Y := Natural (Rows) / 2;
   Put_At (X, Y, "@");
   Show_Status ("none");

   loop
      declare
         use Rose.Keys;
         Key       : Word;
         Modifiers : Word;
         New_X     : Natural := X;
         New_Y     : Natural := Y;
      begin
         Console.Read_Key (Key, Modifiers);
         exit when Key = Character'Pos ('q')
           or else Key = Character'Pos ('Q');

         --  The mark stays off the corners, the title and the bottom
         --  line.

         if Key = Up and then Y > 2 then
            New_Y := Y - 1;
         elsif Key = Down and then Y + 3 <= Last_Row then
            New_Y := Y + 1;
         elsif Key = Left and then X > 1 then
            New_X := X - 1;
         elsif Key = Right and then X + 2 <= Last_Column then
            New_X := X + 1;
         end if;

         if New_X /= X or else New_Y /= Y then
            Put_At (X, Y, " ");
            X := New_X;
            Y := New_Y;
            Put_At (X, Y, "@");
         end if;

         Show_Status (Key_Name (Key, Modifiers mod 8));
      end;
   end loop;

   Console.Clear;
   Console.Enable_Line_Editing;
end Rose.Core.Keytest;
