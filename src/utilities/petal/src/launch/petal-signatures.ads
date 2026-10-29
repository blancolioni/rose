package Petal.Signatures is

   --  The signature of a command, from the metadata that the IDL compiler
   --  puts in its executable (ADR 0002): lines of a key, a space and a
   --  value, in which a line break is written \n and a backslash \\.

   Max_Text      : constant := 4096;
   Max_Arguments : constant := 16;
   Max_Streams   : constant := 3;
   Max_Role_Caps : constant := 4;

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

   type Argument_Record is
      record
         Name            : Slice;
         Of_Type         : Argument_Type := String_Type;
         Interface_Name  : Slice;
         Mode            : Argument_Mode := In_Mode;
         Versioned       : Boolean := False;
         --  An in interface whose objects the shell can version and
         --  commit (ADR 0012, section 2).
         Place           : Place_Kind := Word_Place;
         Index           : Natural := 0;
         Count_Word      : Natural := 0;
         Has_Default     : Boolean := False;
         Default         : Slice;
         Short_Name      : Character := ' ';
         Has_Short_Value : Boolean := False;
         Short_Value     : Boolean := True;
         Help            : Slice;
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
      Console_Cap, Other_Role_Cap);
   --  A cap that one of the command's roles needs, other than a stream:
   --  a system cap, the command registry's Commands interface for the
   --  Find_Command role (#460), the console's Terminal for the Console
   --  role (#454), or a role the shell does not supply.

   type Role_Cap_Record is
      record
         Kind      : Role_Cap_Kind := Other_Role_Cap;
         Name      : Slice;
         Cap_Index : Natural := 0;
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
         Live           : Boolean := False;
         --  The command changes its Versioned arguments as it runs,
         --  rather than being a transaction over them (ADR 0012,
         --  section 3).
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

   function Value
     (Item  : Signature;
      Field : Slice)
      return String;
   --  The value of Field, with its escapes replaced.

   function Same_Name
     (Item     : Signature;
      Argument : Argument_Record;
      Name     : String)
      return Boolean;
   --  True if Name, as written on the command line, names Argument: case
   --  does not matter, and a hyphen is the same as an underscore.

end Petal.Signatures;
