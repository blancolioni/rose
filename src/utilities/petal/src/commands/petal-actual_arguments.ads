with Petal.Formal_Arguments;
with Petal.Values;

package Petal.Actual_Arguments is

   type Actual_Argument is private;

   function To_Actual_Argument
     (Formal : Petal.Formal_Arguments.Formal_Argument;
      Value  : String)
      return Actual_Argument;

   function To_Boolean
     (Argument : Actual_Argument)
      return Boolean;

   function To_String
     (Argument : Actual_Argument)
      return String;

   function To_Stream_Reader
     (Argument : Actual_Argument)
      return Rose.Capabilities.Capability;

   type Actual_Argument_Container is private;

   procedure Add_Argument
     (Container : in out Actual_Argument_Container;
      Argument  : Actual_Argument);

   function Named_Argument
     (Container : Actual_Argument_Container;
      Long_Name : String)
      return Actual_Argument;

   function Named_Argument
     (Container     : Actual_Argument_Container;
      Short_Name    : Character;
      Default_Value : Petal.Values.Petal_Value :=
        Petal.Values.To_Value (False))
      return Petal.Values.Petal_Value;

   function Extra_Argument_Count
     (Container : Actual_Argument_Container)
      return Natural;

   function Extra_Argument
     (Container : Actual_Argument_Container;
      Index     : Positive)
      return Actual_Argument;

private

   type Actual_Argument is
      record
         Valid  : Boolean := False;
         Formal : Petal.Formal_Arguments.Formal_Argument :=
                    Petal.Formal_Arguments.No_Formal_Argument;
         Value  : Petal.Values.Petal_Value;
      end record;

   Max_Actual_Arguments : constant := 100;

   type Actual_Argument_Container is
     array (1 .. Max_Actual_Arguments) of Actual_Argument;

   function To_Actual_Argument
     (Formal : Petal.Formal_Arguments.Formal_Argument;
      Value  : String)
      return Actual_Argument
   is (True, Formal, Petal.Values.To_Value (Value));

end Petal.Actual_Arguments;
