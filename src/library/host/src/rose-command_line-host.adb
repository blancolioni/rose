with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Finalization;
with Ada.Text_IO;

with GNAT.OS_Lib;

with Rose.Command_Line.Items;
with Rose.Command_Signatures.Help;

package body Rose.Command_Line.Host is

   use Ada.Strings.Unbounded;
   use Rose.Command_Signatures;
   use type System.Storage_Elements.Storage_Offset;

   package Help is
     new Rose.Command_Signatures.Help
       (Put      => Ada.Text_IO.Put,
        Put_Line => Ada.Text_IO.Put_Line);

   Finished : Boolean := False;
   Status   : Integer := 0;
   --  Set when the main has finished normally: the command completed or
   --  failed, or Receive stopped it. Status is the exit status.

   procedure Finish (Exit_Status : Integer);
   --  The main has finished normally, with Exit_Status.

   type Quiet_Exit is new Ada.Finalization.Limited_Controlled
     with null record;

   overriding procedure Finalize (Item : in out Quiet_Exit);
   --  If the main finished normally, flush the standard streams and end
   --  the process with its exit status. This is finalized after the
   --  main's objects, and before Ada.Text_IO, which would otherwise end
   --  an unfinished last line on standard output with a line feed that
   --  the command didn't write, as Rose's run-time system doesn't: echo
   --  -n must write no line feed here either. The C library's exit flushes
   --  every other open file. A main that ends with an unhandled exception
   --  hasn't finished normally, so the run-time system reports it as
   --  usual.

   Ender : Quiet_Exit;
   pragma Unreferenced (Ender);

   function Name_Of (Call : Launch_Call) return String
   is (Value (Call.Signature, Call.Signature.Name));
   --  The command's name, as the user runs it.

   procedure Encode
     (Call  : in out Launch_Call;
      Items : Text_Array);
   --  Make Call's buffer from Items, in the format of a launch buffer:
   --  each item a 32-bit little-endian length, the characters, and
   --  padding to a multiple of four bytes.

   ----------------------
   -- Boolean_Argument --
   ----------------------

   function Boolean_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Boolean
   is (Call.Words (Word) /= 0);

   --------------
   -- Complete --
   --------------

   procedure Complete (Call : in out Launch_Call) is
      pragma Unreferenced (Call);
   begin
      Finish (0);
   end Complete;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Call  : in out Launch_Call;
      Items : Text_Array)
   is
      use System.Storage_Elements;
      Length : Storage_Count := 0;
      Last   : Storage_Count := 0;
   begin
      for Item of Items loop
         Length := Length + 4
           + Storage_Count ((Ada.Strings.Unbounded.Length (Item) + 3)
                            / 4 * 4);
      end loop;

      Call.Buffer := new Storage_Array'(1 .. Length => 0);

      for Item of Items loop
         declare
            Text  : constant String := To_String (Item);
            Count : Natural := Text'Length;
         begin
            for Byte in 1 .. 4 loop
               Call.Buffer (Last + Storage_Offset (Byte)) :=
                 Storage_Element (Count mod 256);
               Count := Count / 256;
            end loop;
            Last := Last + 4;
            for Ch of Text loop
               Last := Last + 1;
               Call.Buffer (Last) := Character'Pos (Ch);
            end loop;
            Last := Last + Storage_Count ((4 - Text'Length mod 4) mod 4);
         end;
      end loop;
   end Encode;

   ----------
   -- Fail --
   ----------

   procedure Fail
     (Call    : in out Launch_Call;
      Failure : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Text_IO.Flush;
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Name_Of (Call) & ": " & Ada.Exceptions.Exception_Message (Failure));
      Finish (1);
   end Fail;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Item : in out Quiet_Exit) is
      pragma Unreferenced (Item);
   begin
      if Finished then
         Ada.Text_IO.Flush (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);
         GNAT.OS_Lib.OS_Exit (Status);
      end if;
   end Finalize;

   ------------
   -- Finish --
   ------------

   procedure Finish (Exit_Status : Integer) is
   begin
      Finished := True;
      Status := Exit_Status;
      Ada.Command_Line.Set_Exit_Status
        (Ada.Command_Line.Exit_Status (Exit_Status));
   end Finish;

   ------------------------
   -- Interface_Argument --
   ------------------------

   function Interface_Argument
     (Call : Launch_Call;
      Cap  : Natural)
      return String
   is (if Cap in Call.Caps'Range then To_String (Call.Caps (Cap)) else "");

   ----------------------
   -- Natural_Argument --
   ----------------------

   function Natural_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Natural
   is (Call.Words (Word));

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Call     : in out Launch_Call;
      Metadata : String)
   is
      Item  : Signature renames Call.Signature;
      Count : constant Natural := Ada.Command_Line.Argument_Count;

      type Bound_Value is
         record
            Is_Set : Boolean := False;
            Word   : Natural := 0;
            Text   : Unbounded_String;
         end record;

      Values       : array (1 .. Max_Arguments) of Bound_Value;
      List         : Text_Array (1 .. Count);
      List_Count   : Natural := 0;
      Items        : Text_Array (1 .. Max_Items);
      Item_Count   : Natural := 0;
      Next_Index   : Positive := 1;
      Options_Done : Boolean := False;

      function Word_Text (Index : Positive) return String
      is (Ada.Command_Line.Argument (Index));

      function Argument_Name (Index : Positive) return String
      is (Value (Item, Item.Arguments (Index).Name));

      function Options_Last return Natural;
      --  The last word before "--", or the last word.

      procedure Fail (Message : String)
        with No_Return;
      --  Say why the command line is wrong, as petal does, and stop.

      function Find_Named (Name : String) return Natural;
      function Find_Short (Name : Character) return Natural;
      function Next_Positional return Natural;

      procedure Set_Value
        (Index : Positive;
         Text  : String);
      --  Bind Text to the argument Index.

      procedure Place_Item
        (Index : Positive;
         Text  : Unbounded_String);
      --  Put Text in the buffer's item Index.

      ----------
      -- Fail --
      ----------

      procedure Fail (Message : String) is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Name_Of (Call) & ": " & Message);
         Finish (2);
         raise Stopped;
      end Fail;

      ----------------
      -- Find_Named --
      ----------------

      function Find_Named (Name : String) return Natural is
      begin
         for I in 1 .. Item.Argument_Count loop
            if Same_Name (Item, Item.Arguments (I), Name) then
               return I;
            end if;
         end loop;
         return 0;
      end Find_Named;

      ----------------
      -- Find_Short --
      ----------------

      function Find_Short (Name : Character) return Natural is
      begin
         for I in 1 .. Item.Argument_Count loop
            if Item.Arguments (I).Short_Name = Name then
               return I;
            end if;
         end loop;
         return 0;
      end Find_Short;

      ---------------------
      -- Next_Positional --
      ---------------------

      function Next_Positional return Natural is
      begin
         for I in 1 .. Item.Argument_Count loop
            if Item.Arguments (I).Short_Name = ' '
              and then (not Values (I).Is_Set
                        or else Item.Arguments (I).Of_Type
                                  = String_List_Type)
            then
               return I;
            end if;
         end loop;
         return 0;
      end Next_Positional;

      ------------------
      -- Options_Last --
      ------------------

      function Options_Last return Natural is
      begin
         for I in 1 .. Count loop
            if Word_Text (I) = "--" then
               return I - 1;
            end if;
         end loop;
         return Count;
      end Options_Last;

      ----------------
      -- Place_Item --
      ----------------

      procedure Place_Item
        (Index : Positive;
         Text  : Unbounded_String)
      is
      begin
         if Index > Items'Last then
            Fail ("too many words");
         end if;
         Items (Index) := Text;
         Item_Count := Natural'Max (Item_Count, Index);
      end Place_Item;

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Index : Positive;
         Text  : String)
      is
         Argument : Argument_Record renames Item.Arguments (Index);
         Value    : Bound_Value renames Values (Index);
      begin
         Value.Is_Set := True;
         case Argument.Of_Type is
            when Boolean_Type =>
               if Ada.Characters.Handling.To_Lower (Text) = "true" then
                  Value.Word := 1;
               elsif Ada.Characters.Handling.To_Lower (Text) = "false" then
                  Value.Word := 0;
               else
                  Fail ("invalid value '" & Text & "' for "
                        & Argument_Name (Index) & ": True or False");
               end if;
            when Natural_Type | Positive_Type =>
               if Text'Length = 0 or else Text'Length > 9
                 or else (for some Ch of Text => Ch not in '0' .. '9')
               then
                  Fail ("invalid number '" & Text & "' for "
                        & Argument_Name (Index));
               end if;
               Value.Word := Natural'Value (Text);
               if Argument.Of_Type = Positive_Type and then Value.Word = 0
               then
                  Fail (Argument_Name (Index) & " must be positive");
               end if;
            when String_Type | Interface_Type =>
               Value.Text := To_Unbounded_String (Text);
            when String_List_Type =>
               List_Count := List_Count + 1;
               List (List_Count) := To_Unbounded_String (Text);
         end case;
      end Set_Value;

   begin
      Rose.Command_Signatures.Parse (Metadata, Item);
      if not Item.Valid then
         raise Program_Error with "the command's metadata can't be read";
      end if;

      if (for some I in 1 .. Options_Last => Word_Text (I) = "--help") then
         Help.Put_Help (Item);
         Finish (0);
         raise Stopped;
      elsif (for some I in 1 .. Options_Last =>
               Word_Text (I) = "--version")
      then
         Help.Put_Version (Item);
         Finish (0);
         raise Stopped;
      end if;

      --  Bind the words: options by name, then the positional arguments
      --  in declaration order, with a String_List taking the rest.

      while Next_Index <= Count loop
         declare
            Word : constant String := Word_Text (Next_Index);
         begin
            if not Options_Done and then Word = "--" then
               Options_Done := True;

            elsif not Options_Done
              and then Word'Length > 2
              and then Word (Word'First .. Word'First + 1) = "--"
            then
               declare
                  Option  : constant String :=
                    Word (Word'First + 2 .. Word'Last);
                  Equals  : Natural := 0;
                  Index   : Natural;
                  Negated : Boolean := False;
               begin
                  for I in Option'Range loop
                     if Option (I) = '=' then
                        Equals := I;
                        exit;
                     end if;
                  end loop;

                  declare
                     Name : constant String :=
                       (if Equals = 0 then Option
                        else Option (Option'First .. Equals - 1));
                  begin
                     Index := Find_Named (Name);
                     if Index = 0
                       and then Name'Length > 3
                       and then Name (Name'First .. Name'First + 2) = "no-"
                     then
                        Index := Find_Named
                          (Name (Name'First + 3 .. Name'Last));
                        Negated := Index > 0
                          and then Item.Arguments (Index).Of_Type
                                     = Boolean_Type;
                        if not Negated then
                           Index := 0;
                        end if;
                     end if;

                     if Index = 0 then
                        Fail ("unknown option --" & Name);
                     elsif Item.Arguments (Index).Of_Type = Boolean_Type
                       and then Equals = 0
                     then
                        Values (Index) :=
                          (Is_Set => True,
                           Word   => (if Negated then 0 else 1),
                           others => <>);
                     elsif Equals > 0 then
                        Set_Value (Index, Option (Equals + 1 .. Option'Last));
                     elsif Next_Index < Count then
                        Next_Index := Next_Index + 1;
                        Set_Value (Index, Word_Text (Next_Index));
                     else
                        Fail ("option --" & Name & " needs a value");
                     end if;
                  end;
               end;

            elsif not Options_Done
              and then Word'Length > 1
              and then Word (Word'First) = '-'
              and then Word (Word'First + 1) not in '0' .. '9'
            then
               for I in Word'First + 1 .. Word'Last loop
                  declare
                     Index : constant Natural := Find_Short (Word (I));
                  begin
                     if Index = 0 then
                        Fail ("invalid option -- '" & Word (I) & "'");
                     elsif Item.Arguments (Index).Of_Type = Boolean_Type
                     then
                        Values (Index) :=
                          (Is_Set => True,
                           Word   =>
                             (if not Item.Arguments (Index).Has_Short_Value
                              or else Item.Arguments (Index).Short_Value
                              then 1 else 0),
                           others => <>);
                     else
                        if I < Word'Last then
                           Set_Value (Index, Word (I + 1 .. Word'Last));
                        elsif Next_Index < Count then
                           Next_Index := Next_Index + 1;
                           Set_Value (Index, Word_Text (Next_Index));
                        else
                           Fail ("option -" & Word (I) & " needs a value");
                        end if;
                        exit;
                     end if;
                  end;
               end loop;

            else
               declare
                  Index : constant Natural := Next_Positional;
               begin
                  if Index = 0 then
                     Fail ("extra operand '" & Word & "'");
                  else
                     Set_Value (Index, Word);
                  end if;
               end;
            end if;
         end;
         Next_Index := Next_Index + 1;
      end loop;

      --  Defaults, as petal supplies them. A directory that isn't given is
      --  the current one, and a reader that defaults to Standard_Input is
      --  standard input: the host backing opens both for "".

      for Index in 1 .. Item.Argument_Count loop
         declare
            Argument : Argument_Record renames Item.Arguments (Index);
         begin
            if Values (Index).Is_Set
              or else Argument.Of_Type = String_List_Type
            then
               null;
            elsif Is_Input_Stream (Item, Argument)
              or else Is_In_Directory (Item, Argument)
            then
               Values (Index).Is_Set := True;
            elsif Argument.Has_Default then
               Set_Value (Index, Value (Item, Argument.Default));
            else
               Fail ("missing argument " & Argument_Name (Index));
            end if;
         end;
      end loop;

      --  The places (see Rose.Command_Line.Launch).

      for Index in 1 .. Item.Argument_Count loop
         declare
            Argument : Argument_Record renames Item.Arguments (Index);
            Bound    : Bound_Value renames Values (Index);
         begin
            case Argument.Place is
               when Word_Place =>
                  Call.Words (Argument.Index) := Bound.Word;
               when Item_Place =>
                  Place_Item (Argument.Index, Bound.Text);
               when List_Place =>
                  Call.Words (Argument.Count_Word) := List_Count;
                  for I in 1 .. List_Count loop
                     Place_Item (Argument.Index + I - 1, List (I));
                  end loop;
               when Cap_Place =>
                  Call.Caps (Argument.Index) := Bound.Text;
            end case;
         end;
      end loop;

      Encode (Call, Items (1 .. Item_Count));
   end Receive;

   ---------------------
   -- String_Argument --
   ---------------------

   function String_Argument
     (Call : Launch_Call;
      Item : Positive)
      return String
   is (Rose.Command_Line.Items.Element
         (Buffer     => Call.Buffer.all,
          First_Item => Item,
          Count      => 1,
          Index      => 1));

   --------------------------
   -- String_List_Argument --
   --------------------------

   function String_List_Argument
     (Call       : Launch_Call;
      Item       : Positive;
      Count_Word : Natural)
      return String_List
   is (Buffer        => Call.Buffer.all'Address,
       Buffer_Length => Call.Buffer'Length,
       First_Item    => Item,
       Count         => Call.Words (Count_Word));

end Rose.Command_Line.Host;
