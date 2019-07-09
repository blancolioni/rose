package Petal.Values is

   type Value_Type is
     (Boolean_Value,
      Integer_Value,
      String_Value,
      Capability_Value);

   type Petal_Value is private;

   function To_Value (From : Boolean) return Petal_Value;
   function To_Value (From : String) return Petal_Value;

   function To_Boolean (Value : Petal_Value) return Boolean;
   function To_Integer (Value : Petal_Value) return Integer;
   function To_String (Value : Petal_Value) return String;
   function To_Capability
     (Value : Petal_Value)
      return Rose.Capabilities.Capability;

private

   Max_String_Length : constant := 100;

   type Petal_Value is new String (1 .. Max_String_Length);

end Petal.Values;
