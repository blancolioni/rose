with Petal.Values;

package Petal.Formal_Arguments is

   type Argument_Type is
     (Value_Argument,
      Read_Stream_Argument,
      Write_Stream_Argument);

   type Formal_Argument is private;

   No_Formal_Argument : constant Formal_Argument;

   type Formal_Argument_Array is
     array (Positive range <>) of Formal_Argument;

   function Is_Named
     (Argument : Formal_Argument)
      return Boolean;

   function Is_Positional
     (Argument : Formal_Argument)
      return Boolean;

   function Long_Name
     (Argument : Formal_Argument)
      return String;

   function Short_Name
     (Argument : Formal_Argument)
      return Character;

   function Expected_Type
     (Argument : Formal_Argument)
      return Argument_Type;

   function Default_Value
     (Argument : Formal_Argument)
      return Boolean;

   function Default_Value
     (Argument : Formal_Argument)
      return Integer;

   function Default_Value
     (Argument : Formal_Argument)
      return String;

   function Named_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Value_Type    : Petal.Values.Value_Type;
      Expected_Type : Argument_Type)
      return Formal_Argument;

   function Positional_Argument
     (Key           : String;
      Required      : Boolean;
      Multiple      : Boolean;
      Value_Type    : Petal.Values.Value_Type;
      Expected_Type : Argument_Type)
      return Formal_Argument;

   function Named_Boolean_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Default_Value : Boolean    := False)
      return Formal_Argument;

   function Named_Integer_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Default_Value : Integer    := 0)
      return Formal_Argument;

   function Named_String_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Default_Value : String     := "")
      return Formal_Argument;

   function Named_Read_Stream_Argument
     (Long_Name     : String;
      Short_Name    : Character)
      return Formal_Argument;

   function Named_Write_Stream_Argument
     (Long_Name     : String;
      Short_Name    : Character)
      return Formal_Argument;

   function Positional_Integer_Argument
     (Key                : String;
      Required, Multiple : Boolean;
      Default_Value      : Integer := 0)
      return Formal_Argument;

   function Positional_String_Argument
     (Key                : String;
      Required, Multiple : Boolean;
      Default_Value      : String  := "")
      return Formal_Argument;

   function Positional_Read_Stream_Argument
     (Key                : String;
      Required, Multiple : Boolean)
      return Formal_Argument;

   function Positional_Write_Stream_Argument
     (Key                : String;
      Required, Multiple : Boolean)
      return Formal_Argument;

private

   type Formal_Argument_Record;

   type Formal_Argument is access constant Formal_Argument_Record;

   No_Formal_Argument : constant Formal_Argument := null;

end Petal.Formal_Arguments;
