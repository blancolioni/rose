package body Petal.Actual_Arguments is

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Container : in out Actual_Argument_Container;
      Argument  : Actual_Argument)
   is
      use type Petal.Formal_Arguments.Formal_Argument;
      Named : constant Boolean :=
                Petal.Formal_Arguments.Is_Named (Argument.Formal);
   begin
      for Arg of Container loop
         if Named and then Arg.Formal = Argument.Formal then
            Arg := Argument;
            exit;
         elsif not Arg.Valid then
            Arg := Argument;
            exit;
         end if;
      end loop;
   end Add_Argument;

   --------------------
   -- Extra_Argument --
   --------------------

   function Extra_Argument
     (Container : Actual_Argument_Container; Index : Positive)
      return Actual_Argument
   is
      Count : Natural := 0;
   begin
      for Argument of Container loop
         exit when not Argument.Valid;
         if Petal.Formal_Arguments.Is_Positional (Argument.Formal) then
            Count := Count + 1;
            if Count = Index then
               return Argument;
            end if;
         end if;
      end loop;
      return Argument : Actual_Argument do
         Argument.Valid := False;
      end return;
   end Extra_Argument;

   --------------------------
   -- Extra_Argument_Count --
   --------------------------

   function Extra_Argument_Count
     (Container : Actual_Argument_Container) return Natural
   is
      Count : Natural := 0;
   begin
      for Argument of Container loop
         exit when not Argument.Valid;
         if Petal.Formal_Arguments.Is_Positional (Argument.Formal) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Extra_Argument_Count;

   --------------------
   -- Named_Argument --
   --------------------

   function Named_Argument
     (Container : Actual_Argument_Container; Long_Name : String)
      return Actual_Argument
   is
   begin
      for Argument of Container loop
         exit when not Argument.Valid;
         if Petal.Formal_Arguments.Long_Name (Argument.Formal) = Long_Name then
            return Argument;
         end if;
      end loop;
      return Argument : Actual_Argument do
         Argument.Valid := False;
      end return;
   end Named_Argument;

   --------------------
   -- Named_Argument --
   --------------------

   function Named_Argument
     (Container     : Actual_Argument_Container;
      Short_Name    : Character;
      Default_Value : Petal.Values.Petal_Value :=
        Petal.Values.To_Value (False))
      return Petal.Values.Petal_Value
   is
   begin
      for Argument of Container loop
         exit when not Argument.Valid;
         if Petal.Formal_Arguments.Is_Named (Argument.Formal)
           and then Petal.Formal_Arguments.Short_Name (Argument.Formal)
           = Short_Name
         then
            return Argument.Value;
         end if;
      end loop;
      return Default_Value;
   end Named_Argument;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (Argument : Actual_Argument) return Boolean is
   begin
      return Petal.Values.To_Boolean (Argument.Value);
   end To_Boolean;

   ----------------------
   -- To_Stream_Reader --
   ----------------------

   function To_Stream_Reader
     (Argument : Actual_Argument) return Rose.Capabilities.Capability
   is
   begin
      return Petal.Values.To_Capability (Argument.Value);
   end To_Stream_Reader;

   ---------------
   -- To_String --
   ---------------

   function To_String (Argument : Actual_Argument) return String is
   begin
      return Petal.Values.To_String (Argument.Value);
   end To_String;

end Petal.Actual_Arguments;
