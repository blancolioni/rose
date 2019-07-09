with Petal.Actual_Arguments;
with Petal.Commands;

package Petal.Parser is

   type Parse_Element is private;

   function Is_Command (Element : Parse_Element) return Boolean;

   function Command
     (Element : Parse_Element)
      return Petal.Commands.Petal_Command;

   function Context
     (Element : Parse_Element)
      return Petal.Commands.Launch_Context;

   function Arguments
     (Element : Parse_Element)
      return Petal.Actual_Arguments.Actual_Argument_Container;

   type Parse_Result is private;

   procedure Parse
     (Line   : String;
      Result : out Parse_Result);

   function Has_Element
     (Result : Parse_Result)
      return Boolean;

   function Element
     (Result : Parse_Result)
      return Parse_Element;

   procedure Next
     (Result : in out Parse_Result);

private

   type Parse_Element is
      record
         Is_Command : Boolean;
         Command    : Petal.Commands.Petal_Command;
         Context    : Petal.Commands.Launch_Context;
         Arguments  : Petal.Actual_Arguments.Actual_Argument_Container;
      end record;

   Max_Parse_Elements : constant := 10;

   type Parse_Element_Array is
     array (1 .. Max_Parse_Elements) of Parse_Element;

   type Parse_Result is
      record
         Elements : Parse_Element_Array;
         Count    : Natural;
         Current  : Positive;
      end record;

end Petal.Parser;
