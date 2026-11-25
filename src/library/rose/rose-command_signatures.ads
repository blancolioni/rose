package Rose.Command_Signatures is

   --  The signature of a command, from the metadata that the IDL compiler
   --  puts in its executable (ADR 0002): lines of a key, a space and a
   --  value, in which a line break is written \n and a backslash \\.
   --  Petal reads it to bind a command line, and so does a command built
   --  for the host (Rose.Command_Line.Host, #1101), so that both bind it
   --  the same way.

   Max_Text      : constant := 4096;
   Max_Arguments : constant := 16;
   Max_Streams   : constant := 3;
   Max_Role_Caps : constant := 16;
   --  As many as a command's launch call can carry (Petal.Command_Launch
   --  has 16 cap slots). It was 4, which dropped run_self_tests' fifth
   --  role and the ones after it, its bank among them (#517).

   type Slice is
      record
         First : Positive := 1;
         Last  : Natural := 0;
      end record;
   --  A value in the metadata, as written.

   type Argument_Type is
     (Boolean_Type, Natural_Type, Positive_Type, String_Type,
      String_List_Type, Interface_Type);
   --  An Interface_Type argument is a cap of the interface Interface_Name
   --  (ADR 0002, section 1).

   type Argument_Mode is (In_Mode, Out_Mode);
   --  The direction (ADR 0012): the shell supplies an in argument, and
   --  the command returns an out one. What a command can do with an
   --  interface is its type.

   type Place_Kind is (Word_Place, Item_Place, List_Place, Cap_Place);
   --  Where an argument goes in the launch invocation: a data word, a
   --  buffer item, buffer items from Index with their number in data
   --  word Count_Word, or a cap (see Rose.Command_Line.Launch).

   Max_Accepts : constant := 3;
   --  The most interfaces after the first that an argument's list can
   --  have, here (ADR 0012, section 6). Petal ignores any more, and can
   --  still bind the first, with selector 0.

   type Accepted_Record is
      record
         Interface_Name : Slice;
         Transactional  : Boolean := False;
      end record;
   --  One of the later interfaces of an argument's list: its name, and
   --  whether the shell gives the command a version of it to commit.

   type Accepted_Array is array (1 .. Max_Accepts) of Accepted_Record;

   type Argument_Record is
      record
         Name            : Slice;
         Of_Type         : Argument_Type := String_Type;
         Interface_Name  : Slice;
         Mode            : Argument_Mode := In_Mode;
         Transactional   : Boolean := False;
         --  An in interface that is Transactional: the shell gives the
         --  command a version, which the command commits (ADR 0026).
         Place           : Place_Kind := Word_Place;
         Index           : Natural := 0;
         Count_Word      : Natural := 0;
         Has_Default     : Boolean := False;
         Default         : Slice;
         Short_Name      : Character := ' ';
         Has_Short_Value : Boolean := False;
         Short_Value     : Boolean := True;
         Help            : Slice;
         Accept_Count    : Natural := 0;
         Accepts         : Accepted_Array;
         --  The interfaces after the first of a list, in order, for an
         --  interface argument that takes one of several (ADR 0012,
         --  section 6).
         Has_Selector    : Boolean := False;
         Selector_Word   : Natural := 0;
         --  The data word that says which of the list the cap is for: 0
         --  for the first, N for the Nth after it.
      end record;
   --  A command's argument. One with a short name is an option, given by
   --  name; the others are positional, in declaration order.

   type Argument_Array is array (1 .. Max_Arguments) of Argument_Record;

   type Stream_Kind is (Input_Stream, Output_Stream, Error_Stream);

   type Stream_Record is
      record
         Kind      : Stream_Kind := Output_Stream;
         Cap_Index : Natural := 0;
      end record;

   type Stream_Array is array (1 .. Max_Streams) of Stream_Record;

   type Role_Cap_Kind is
     (Create_Endpoint_Cap, Delete_Cap, Rescind_Cap, Find_Command_Cap,
      Console_Cap, Clock_Service_Cap, Storage_Service_Cap,
      Screen_Service_Cap, Create_Signal_Cap, Cap_Set_Service_Cap,
      Package_State_Cap,
      Replace_Process_Cap, Other_Role_Cap);
   --  A cap that one of the command's roles needs, other than a stream:
   --  a system cap, a bank for the Storage_Service role, which petal
   --  makes below its own for each command (ADR 0025, #517), the screen
   --  for the Screen_Service role, which petal gives as the null cap
   --  (#582), the cap set server for the Cap_Set_Service role, which
   --  petal gives as the null cap as well (#825), petal's own cap that
   --  makes signal caps for the Create_Signal role, which it doesn't have
   --  yet either (#708), the command registry's Commands interface for
   --  the Find_Command role (#460), the console's Terminal for the
   --  Console role (#454), the null cap for the Replace_Process role,
   --  since petal holds no Replacer (#866), or a role the shell does not
   --  supply.

   type Role_Cap_Record is
      record
         Kind      : Role_Cap_Kind := Other_Role_Cap;
         Name      : Slice;
         Cap_Index : Natural := 0;
         Category  : Slice;
         --  The role's category (ADR 0021), empty if it has none. The
         --  authority of a categorised role comes from the rights
         --  broker, and not from petal.
      end record;

   type Role_Cap_Array is array (1 .. Max_Role_Caps) of Role_Cap_Record;

   type Signature is
      record
         Valid          : Boolean := False;
         Text           : String (1 .. Max_Text);
         Length         : Natural := 0;
         Name           : Slice;
         Package_Name   : Slice;
         Title          : Slice;
         Version        : Slice;
         Copyright      : Slice;
         Authors        : Slice;
         License        : Slice;
         Help_Footer    : Slice;
         Summary        : Slice;
         Description    : Slice;
         Argument_Count : Natural := 0;
         Arguments      : Argument_Array;
         Stream_Count   : Natural := 0;
         Streams        : Stream_Array;
         Role_Cap_Count : Natural := 0;
         Role_Caps      : Role_Cap_Array;
      end record;

   procedure Parse
     (Text   : String;
      Result : out Signature);
   --  The signature in the metadata Text. Result.Valid is False if Text is
   --  not command metadata.

   function Is_Categorised (Role : Role_Cap_Record) return Boolean
   is (Role.Category.Last >= Role.Category.First);
   --  True if Role has a category.

   function Value
     (Item  : Signature;
      Field : Slice)
      return String;
   --  The value of Field, with its escapes replaced.

   function Is_Writable_Directory
     (Item     : Signature;
      Argument : Argument_Record)
      return Boolean
   is (Argument.Of_Type = Interface_Type
       and then Argument.Mode = In_Mode
       and then Value (Item, Argument.Interface_Name)
                  = "Files.Writable_Directory");
   --  True if Argument is an in Writable_Directory, which petal gives its
   --  own writable cap (ADR 0012, section 4).

   function Is_In_Directory
     (Item     : Signature;
      Argument : Argument_Record)
      return Boolean
   is (Is_Writable_Directory (Item, Argument)
       or else (Argument.Of_Type = Interface_Type
                and then Argument.Mode = In_Mode
                and then Value (Item, Argument.Interface_Name)
                           = "Files.Directory"));
   --  True if Argument is an in Directory or an in Writable_Directory. It
   --  names a directory, and is bound to the current directory if it is
   --  not given (ADR 0003, section 6; #342).

   function Is_Input_Stream
     (Item     : Signature;
      Argument : Argument_Record)
      return Boolean
   is (Argument.Of_Type = Interface_Type
       and then Argument.Mode = In_Mode
       and then Argument.Has_Default
       and then Value (Item, Argument.Default)
                  = "Streams.Standard_Input");
   --  True if Argument is an in Reader whose default is standard input
   --  (#395; idle#52). It names a file in the current directory, which
   --  the shell opens a reader on, or if it is not given, it is the
   --  shell's own standard input. The command is given the Read cap of
   --  either, as it is for the Standard_Input role, and can't tell which
   --  it has.

   function Same_Name
     (Item     : Signature;
      Argument : Argument_Record;
      Name     : String)
      return Boolean;
   --  True if Name, as written on the command line, names Argument: case
   --  does not matter, and a hyphen is the same as an underscore.

end Rose.Command_Signatures;
