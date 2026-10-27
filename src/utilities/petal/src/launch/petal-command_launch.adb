with System.Storage_Elements;

with Ada_Text_IO;

with Rose.Capabilities.Standard;
with Rose.Interfaces.Cap_Set;
with Rose.Interfaces.Launch.Client;
with Rose.Invocation;
with Rose.Standard_Streams;
with Rose.System_Calls.Client;
with Rose.Trace;
with Rose.Words;

with Petal.Session;
with Petal.Signatures;

package body Petal.Command_Launch is

   use Petal.Signatures;
   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word;

   --  Petal's argument cap set: the caps it uses here.

   Delete_Cap_Index      : constant := 6;
   Rescind_Cap_Index     : constant := 7;
   Create_Endpoint_Index : constant := 8;
   Command_Map_Index     : constant := 10;
   --  The command registry's Find (interfaces/map.idl).
   Trace_Index           : constant := 11;
   --  For trace events (Rose.Trace).

   Find_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   Max_Words : constant := 64;
   type Word_Array is array (1 .. Max_Words) of Slice;
   --  The words of a command line.

   Argument_Buffer : Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  The buffer of the launch invocation, which the kernel shares with
   --  the command page by page.

   type Bound_Value is
      record
         Is_Set    : Boolean := False;
         Word      : Rose.Words.Word := 0;
         From_Line : Boolean := True;
         Text      : Slice;
         Cap       : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
      end record;
   --  The value bound to an argument: a data word, the text of a String
   --  in the command line or in the signature's default, or the cap of
   --  an interface.

   type Bound_Value_Array is array (1 .. Max_Arguments) of Bound_Value;

   Metadata : String (1 .. Max_Text);
   Current  : Signature;
   --  The metadata and signature of the command being run. They are too
   --  big for petal's stack.

   procedure Error (Message : String);
   --  Report Message on standard error.

   function Is_In_Directory
     (Item     : Signature;
      Argument : Argument_Record)
      return Boolean
   is (Argument.Of_Type = Interface_Type
       and then Argument.Mode = In_Mode
       and then Value (Item, Argument.Interface_Name) = "Directory");
   --  True if Argument is an in Directory. It names a directory in
   --  petal's table, and is bound to home if it is not given (ADR 0003,
   --  section 6).

   function Is_In_File
     (Item     : Signature;
      Argument : Argument_Record)
      return Boolean
   is (Argument.Of_Type = Interface_Type
       and then Argument.Mode = In_Mode
       and then Value (Item, Argument.Interface_Name) = "File");
   --  True if Argument is an in File, which names a file in the current
   --  directory (#344). It must be given, unless it has a default.

   procedure Launch
     (Command_Name : String;
      Item         : Signature;
      Client       : Rose.Interfaces.Launch.Client.Launch_Client;
      Line         : String;
      Words        : Word_Array;
      Count        : Natural);
   --  Bind words 2 .. Count of Line to the arguments of Item, launch the
   --  command through Client, and call it.

   function Lower (Text : String) return String;

   procedure Open_Registry;
   --  Find the registry, and the caps for deleting caps, the first time.

   procedure Put_Help (Item : Signature);
   procedure Put_Version (Item : Signature);

   procedure Split
     (Line  : String;
      Words : out Word_Array;
      Count : out Natural);

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada_Text_IO.Put_Line ("petal: " & Message);
   end Error;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Command_Name : String;
      Item         : Signature;
      Client       : Rose.Interfaces.Launch.Client.Launch_Client;
      Line         : String;
      Words        : Word_Array;
      Count        : Natural)
   is
      --  Slices of the command line keep its indices, so a slice of a
      --  word is also a slice of Line.

      Values       : Bound_Value_Array;
      List_Words   : Word_Array;
      List_Count   : Natural := 0;
      Next_Index   : Positive := 2;
      Options_Done : Boolean := False;
      Failed       : Boolean := False;

      function Word_Text (Index : Positive) return String
      is (Line (Words (Index).First .. Words (Index).Last));

      function Argument_Name (Index : Positive) return String
      is (Item.Text (Item.Arguments (Index).Name.First
                     .. Item.Arguments (Index).Name.Last));

      function Find_Named (Name : String) return Natural;
      function Find_Short (Name : Character) return Natural;
      function Next_Positional return Natural;

      procedure Fail (Message : String);

      procedure Set_Value
        (Index     : Positive;
         Text      : String;
         Where     : Slice;
         From_Line : Boolean);
      --  Bind Text, which is at Where in the command line or in the
      --  signature's text, to argument Index.

      ----------
      -- Fail --
      ----------

      procedure Fail (Message : String) is
      begin
         if not Failed then
            Error (Command_Name & ": " & Message);
            Failed := True;
         end if;
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

      ---------------
      -- Set_Value --
      ---------------

      procedure Set_Value
        (Index     : Positive;
         Text      : String;
         Where     : Slice;
         From_Line : Boolean)
      is
         Argument : Argument_Record renames Item.Arguments (Index);
         Value    : Bound_Value renames Values (Index);
      begin
         Value.Is_Set := True;
         case Argument.Of_Type is
            when Boolean_Type =>
               if Lower (Text) = "true" then
                  Value.Word := 1;
               elsif Lower (Text) = "false" then
                  Value.Word := 0;
               else
                  Fail ("invalid value '" & Text & "' for "
                        & Argument_Name (Index) & ": True or False");
               end if;
            when Natural_Type | Positive_Type =>
               declare
                  Number : Rose.Words.Word := 0;
               begin
                  if Text'Length = 0 or else Text'Length > 9 then
                     Fail ("invalid number '" & Text & "' for "
                           & Argument_Name (Index));
                     return;
                  end if;
                  for Ch of Text loop
                     if Ch not in '0' .. '9' then
                        Fail ("invalid number '" & Text & "' for "
                              & Argument_Name (Index));
                        return;
                     end if;
                     Number := Number * 10 + Character'Pos (Ch) - 48;
                  end loop;
                  if Argument.Of_Type = Positive_Type and then Number = 0
                  then
                     Fail (Argument_Name (Index) & " must be positive");
                  end if;
                  Value.Word := Number;
               end;
            when String_Type =>
               Value.From_Line := From_Line;
               Value.Text := Where;
            when String_List_Type =>
               if List_Count < Max_Words then
                  List_Count := List_Count + 1;
                  List_Words (List_Count) := Where;
               end if;
            when Interface_Type =>
               if Is_In_Directory (Item, Argument) then
                  declare
                     Found : Boolean;
                  begin
                     Petal.Session.Find_Grant (Text, Value.Cap, Found);
                     if not Found then
                        Fail ("no directory named '" & Text & "'");
                     end if;
                  end;
               elsif Is_In_File (Item, Argument) then
                  declare
                     use all type Petal.Session.File_Lookup;
                     Result : Petal.Session.File_Lookup;
                  begin
                     Petal.Session.Find_File (Text, Value.Cap, Result);
                     case Result is
                        when File_Found =>
                           null;
                        when No_Such_File =>
                           Fail ("no file named '" & Text & "'");
                        when Not_A_File =>
                           Fail ("'" & Text & "' is not a file");
                        when No_Directory =>
                           Fail ("no directory to find '" & Text & "' in");
                     end case;
                  end;
               else
                  Fail ("cannot bind " & Argument_Name (Index)
                        & ": only in Directory and in File arguments are "
                        & "supported");
               end if;
         end case;
      end Set_Value;

   begin
      --  Bind the words: options by name, then the positional arguments
      --  in declaration order, with a String_List taking the rest.

      while Next_Index <= Count and then not Failed loop
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
                        Set_Value
                          (Index, Option (Equals + 1 .. Option'Last),
                           (Equals + 1, Option'Last),
                           From_Line => True);
                     elsif Next_Index < Count then
                        Next_Index := Next_Index + 1;
                        Set_Value
                          (Index, Word_Text (Next_Index),
                           Words (Next_Index), From_Line => True);
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
                        exit;
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
                           Set_Value
                             (Index, Word (I + 1 .. Word'Last),
                              (I + 1, Word'Last),
                              From_Line => True);
                        elsif Next_Index < Count then
                           Next_Index := Next_Index + 1;
                           Set_Value
                             (Index, Word_Text (Next_Index),
                              Words (Next_Index), From_Line => True);
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
                     Set_Value
                       (Index, Word, Words (Next_Index), From_Line => True);
                  end if;
               end;
            end if;
         end;
         Next_Index := Next_Index + 1;
      end loop;

      --  Defaults. The shell supplies them, so the command receives every
      --  argument.

      for Index in 1 .. Item.Argument_Count loop
         exit when Failed;
         declare
            Argument : Argument_Record renames Item.Arguments (Index);
         begin
            if Values (Index).Is_Set
              or else Argument.Of_Type = String_List_Type
            then
               null;
            elsif Argument.Has_Default then
               Set_Value (Index, Value (Item, Argument.Default),
                          Argument.Default, From_Line => False);
            elsif Is_In_Directory (Item, Argument) then
               declare
                  Found : Boolean;
               begin
                  Petal.Session.Find_Grant
                    (Petal.Session.Home_Name, Values (Index).Cap, Found);
                  if Found then
                     Values (Index).Is_Set := True;
                  else
                     Fail ("missing argument " & Argument_Name (Index));
                  end if;
               end;
            else
               Fail ("missing argument " & Argument_Name (Index));
            end if;
         end;
      end loop;

      if Failed then
         return;
      end if;

      --  The launch invocation (see Rose.Command_Line.Launch).

      declare
         Params      : aliased Rose.Invocation.Invocation_Record;
         Data        : array (0 .. 15) of Rose.Words.Word := (others => 0);
         Word_Count  : Natural := 0;
         Caps        : Rose.Capabilities.Capability_Array (1 .. 16) :=
           (others => Rose.Capabilities.Null_Capability);
         Cap_Count   : Natural := 0;
         Buffer_Last : Storage_Count := 0;
         Endpoint    : Rose.Capabilities.Capability;
         No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);

         procedure Add_Item (Text : String);
         --  Add Text to the buffer, as the next item.

         --------------
         -- Add_Item --
         --------------

         procedure Add_Item (Text : String) is
            Length : constant Storage_Count := Text'Length;
            Size   : constant Storage_Count := 4 + (Length + 3) / 4 * 4;
            Rest   : Storage_Count := Length;
         begin
            if Buffer_Last + Size > Argument_Buffer'Last then
               Fail ("the arguments are too long");
               return;
            end if;

            --  The length, little-endian.

            for I in Storage_Offset range 1 .. 4 loop
               Argument_Buffer (Buffer_Last + I) :=
                 Storage_Element (Rest mod 256);
               Rest := Rest / 256;
            end loop;
            for I in Text'Range loop
               Argument_Buffer
                 (Buffer_Last + 5 + Storage_Offset (I - Text'First)) :=
                 Character'Pos (Text (I));
            end loop;
            Argument_Buffer (Buffer_Last + 5 + Length .. Buffer_Last + Size)
              := (others => 0);
            Buffer_Last := Buffer_Last + Size;
         end Add_Item;

      begin
         for Index in 1 .. Item.Argument_Count loop
            declare
               Argument : Argument_Record renames Item.Arguments (Index);
            begin
               case Argument.Place is
                  when Word_Place =>
                     Data (Argument.Index) := Values (Index).Word;
                     Word_Count :=
                       Natural'Max (Word_Count, Argument.Index + 1);
                  when List_Place =>
                     Data (Argument.Count_Word) :=
                       Rose.Words.Word (List_Count);
                     Word_Count :=
                       Natural'Max (Word_Count, Argument.Count_Word + 1);
                  when Item_Place =>
                     null;
                  when Cap_Place =>
                     Caps (Argument.Index + 1) := Values (Index).Cap;
                     Cap_Count := Natural'Max (Cap_Count, Argument.Index + 1);
               end case;
            end;
         end loop;

         --  String arguments are items 1 onwards, in order of their item
         --  numbers; then the strings of the list.

         for Item_Number in 1 .. Item.Argument_Count loop
            for Index in 1 .. Item.Argument_Count loop
               if Item.Arguments (Index).Place = Item_Place
                 and then Item.Arguments (Index).Index = Item_Number
               then
                  if Values (Index).From_Line then
                     Add_Item (Line (Values (Index).Text.First
                                     .. Values (Index).Text.Last));
                  else
                     Add_Item (Value (Item, Values (Index).Text));
                  end if;
               end if;
            end loop;
         end loop;

         for I in 1 .. List_Count loop
            Add_Item (Line (List_Words (I).First .. List_Words (I).Last));
         end loop;

         for Stream of Item.Streams (1 .. Item.Stream_Count) loop
            Caps (Stream.Cap_Index + 1) :=
              (case Stream.Kind is
                  when Input_Stream  => Rose.Standard_Streams.Standard_Input,
                  when Output_Stream => Rose.Standard_Streams.Standard_Output,
                  when Error_Stream  => Rose.Standard_Streams.Standard_Error);
            Cap_Count := Natural'Max (Cap_Count, Stream.Cap_Index + 1);
         end loop;

         if Failed then
            return;
         end if;

         Rose.Trace.Event ("arguments bound");
         Endpoint :=
           Rose.Interfaces.Launch.Client.Launch_Command (Client, No_Caps);
         Rose.Trace.Event ("launched");
         if Endpoint = Rose.Capabilities.Null_Capability then
            Fail ("cannot launch");
            return;
         end if;

         Rose.System_Calls.Initialize_Send (Params, Endpoint);
         for I in 0 .. Word_Count - 1 loop
            Rose.System_Calls.Send_Word (Params, Data (I));
         end loop;
         for I in 1 .. Cap_Count loop
            Rose.System_Calls.Send_Cap (Params, Caps (I));
         end loop;
         if Buffer_Last > 0 then
            Rose.System_Calls.Send_Buffer
              (Params, Buffer_Last, Argument_Buffer'Address, False);
         end if;
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         Rose.Trace.Event ("reply received");

         if Params.Control.Flags (Rose.Invocation.Error) then
            Fail ("the launch call failed");
         elsif Params.Control.Flags (Rose.Invocation.Send_Words)
           and then Params.Data (0) /= 0
         then
            if Params.Control.Flags (Rose.Invocation.Send_Buffer) then
               declare
                  Message : constant String
                    (1 .. Natural (Params.Buffer_Length))
                    with Import, Address => Params.Buffer_Address;
               begin
                  Fail (Message);
               end;
            else
               Fail ("failed");
            end if;
         end if;

         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Endpoint);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Launch;

   -----------
   -- Lower --
   -----------

   function Lower (Text : String) return String is
      Result : String := Text;
   begin
      for Ch of Result loop
         if Ch in 'A' .. 'Z' then
            Ch := Character'Val (Character'Pos (Ch) + 32);
         end if;
      end loop;
      return Result;
   end Lower;

   -------------------
   -- Open_Registry --
   -------------------

   procedure Open_Registry is
      Params  : aliased Rose.Invocation.Invocation_Record;
      Get_Cap : Rose.Capabilities.Capability;

      function Argument_Cap
        (Index : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability (Get_Cap, (1 => Index)));

   begin
      if Find_Cap /= Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Standard.Argument_Cap_Set);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Cap_Set.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);
      Get_Cap := Params.Caps (Rose.Interfaces.Cap_Set.Get_Cap_Index);

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Argument_Cap (Create_Endpoint_Index),
         Delete_Cap      => Argument_Cap (Delete_Cap_Index),
         Rescind_Cap     => Argument_Cap (Rescind_Cap_Index));
      Find_Cap := Argument_Cap (Command_Map_Index);
      Rose.Trace.Set_Trace_Cap (Argument_Cap (Trace_Index));
   end Open_Registry;

   --------------
   -- Put_Help --
   --------------

   procedure Put_Help (Item : Signature) is
      use Ada_Text_IO;
      Name : constant String := Value (Item, Item.Name);
   begin
      Put ("Usage: " & Name);
      if (for some I in 1 .. Item.Argument_Count =>
            Item.Arguments (I).Short_Name /= ' ')
      then
         Put (" [OPTION]...");
      end if;
      for Argument of Item.Arguments (1 .. Item.Argument_Count) loop
         if Argument.Short_Name = ' ' then
            declare
               Upper : String := Value (Item, Argument.Name);
            begin
               for Ch of Upper loop
                  if Ch in 'a' .. 'z' then
                     Ch := Character'Val (Character'Pos (Ch) - 32);
                  end if;
               end loop;
               if Argument.Of_Type = String_List_Type then
                  Put (" [" & Upper & "]...");
               elsif Argument.Has_Default
                 or else Is_In_Directory (Item, Argument)
               then
                  Put (" [" & Upper & "]");
               else
                  Put (" " & Upper);
               end if;
            end;
         end if;
      end loop;
      New_Line;

      Put_Line (Value (Item, Item.Summary));
      New_Line;

      --  The options. A String_List argument takes the remaining words,
      --  so it is only in the usage line.

      for Argument of Item.Arguments (1 .. Item.Argument_Count) loop
         if Argument.Of_Type /= String_List_Type then
            declare
               Long : String := Value (Item, Argument.Name);
            begin
               for Ch of Long loop
                  if Ch = '_' then
                     Ch := '-';
                  elsif Ch in 'A' .. 'Z' then
                     Ch := Character'Val (Character'Pos (Ch) + 32);
                  end if;
               end loop;
               Put ("  "
                    & (if Argument.Short_Name = ' ' then "    "
                       else "-" & Argument.Short_Name & ", ")
                    & "--"
                    & (if Argument.Of_Type = Boolean_Type then "[no-]" else "")
                    & Long);
               if Argument.Of_Type /= Boolean_Type then
                  Put ("=VALUE");
               end if;
               New_Line;
               Put_Line ("        " & Value (Item, Argument.Help));
            end;
         end if;
      end loop;
      Put_Line ("      --help     display this help and exit");
      Put_Line ("      --version  output version information and exit");

      if Item.Description.Last >= Item.Description.First then
         New_Line;
         Put_Line (Value (Item, Item.Description));
      end if;

      if Item.Help_Footer.Last >= Item.Help_Footer.First then
         New_Line;
         Put_Line (Value (Item, Item.Help_Footer));
      end if;
   end Put_Help;

   -----------------
   -- Put_Version --
   -----------------

   procedure Put_Version (Item : Signature) is
      use Ada_Text_IO;
   begin
      Put_Line (Value (Item, Item.Name)
                & " (" & Value (Item, Item.Title) & ") "
                & Value (Item, Item.Version));
      if Item.Copyright.Last >= Item.Copyright.First then
         Put_Line (Value (Item, Item.Copyright));
      end if;
      if Item.License.Last >= Item.License.First then
         Put_Line (Value (Item, Item.License));
      end if;
      if Item.Authors.Last >= Item.Authors.First then
         New_Line;
         Put_Line ("Written by " & Value (Item, Item.Authors) & ".");
      end if;
   end Put_Version;

   ---------
   -- Run --
   ---------

   procedure Run (Line : String) is
      Words : Word_Array;
      Count : Natural;
   begin
      Split (Line, Words, Count);
      if Count = 0 then
         return;
      end if;

      Open_Registry;

      declare
         Name   : constant String :=
           Lower (Line (Words (1).First .. Words (1).Last));
         Params : aliased Rose.Invocation.Invocation_Record;
         Client : Rose.Interfaces.Launch.Client.Launch_Client;
         Last   : Natural;
      begin
         Rose.Trace.Event ("run " & Name);
         Rose.System_Calls.Initialize_Send (Params, Find_Cap);
         Rose.System_Calls.Send_Text (Params, Name);
         Rose.System_Calls.Receive_Caps (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         Rose.Trace.Event ("found in registry");

         if Params.Control.Flags (Rose.Invocation.Error)
           or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
           or else Params.Caps (0) = Rose.Capabilities.Null_Capability
         then
            Error (Name & ": command not found");
            return;
         end if;

         Rose.Interfaces.Launch.Client.Open (Client, Params.Caps (0));
         Rose.Interfaces.Launch.Client.Metadata (Client, Metadata, Last);
         Parse (Metadata (1 .. Last), Current);
         Rose.Trace.Event ("metadata read, bytes ", Last);

         if not Current.Valid then
            Error (Name & ": not a command");
         elsif (for some I in 2 .. Count =>
                  Line (Words (I).First .. Words (I).Last) = "--help")
         then
            Put_Help (Current);
         elsif (for some I in 2 .. Count =>
                  Line (Words (I).First .. Words (I).Last) = "--version")
         then
            Put_Version (Current);
         else
            Launch (Name, Current, Client, Line, Words, Count);
            Petal.Session.Release_Files;
         end if;

         Rose.Interfaces.Launch.Client.Close (Client);
      end;
   end Run;

   -----------
   -- Split --
   -----------

   procedure Split
     (Line  : String;
      Words : out Word_Array;
      Count : out Natural)
   is
      Index : Positive := Line'First;
   begin
      Count := 0;
      while Index <= Line'Last and then Count < Max_Words loop
         if Line (Index) = ' ' then
            Index := Index + 1;
         elsif Line (Index) = '"' then
            declare
               First : constant Positive := Index + 1;
            begin
               Index := First;
               while Index <= Line'Last and then Line (Index) /= '"' loop
                  Index := Index + 1;
               end loop;
               Count := Count + 1;
               Words (Count) := (First, Index - 1);
               Index := Index + 1;
            end;
         else
            declare
               First : constant Positive := Index;
            begin
               while Index <= Line'Last and then Line (Index) /= ' ' loop
                  Index := Index + 1;
               end loop;
               Count := Count + 1;
               Words (Count) := (First, Index - 1);
            end;
         end if;
      end loop;
   end Split;

end Petal.Command_Launch;
