package Petal.Signatures is

   --  The signature of a command, from the metadata that the IDL compiler
   --  puts in its executable (ADR 0002): lines of a key, a space and a
   --  value, in which a line break is written \n and a backslash \\.

   Max_Text      : constant := 4096;
   Max_Arguments : constant := 16;
   Max_Streams   : constant := 3;

   type Slice is
      record
         First : Positive := 1;
         Last  : Natural := 0;
      end record;
   --  A value in the metadata, as written.

   type Argument_Type is
     (Boolean_Type, Natural_Type, Positive_Type, String_Type,
      String_List_Type);

   type Place_Kind is (Word_Place, Item_Place, List_Place);
   --  Where an argument goes in the launch invocation: a data word, a
   --  buffer item, or buffer items from Index, with their number in data
   --  word Count_Word (see Rose.Command_Line.Launch).

   type Argument_Record is
      record
         Name            : Slice;
         Of_Type         : Argument_Type := String_Type;
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
