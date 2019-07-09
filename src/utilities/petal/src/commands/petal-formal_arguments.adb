package body Petal.Formal_Arguments is

   Max_Formal_Arguments : constant := 1000;
   Max_Name_Length      : constant := 20;

   subtype Formal_Argument_Name is String (1 .. Max_Name_Length);

   type Formal_Argument_Record is
      record
         Has_Name      : Boolean;
         Long_Name     : Formal_Argument_Name;
         Short_Name    : Character;
         Value_Type    : Petal.Values.Value_Type;
         Expected_Type : Argument_Type;
         Default_Value : Petal.Values.Petal_Value;
      end record;

   Formal_Argument_Container :
     array (1 .. Max_Formal_Arguments) of aliased Formal_Argument_Record;
   Formal_Argument_Count : Natural := 0;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Argument : Formal_Argument) return Boolean is
   begin
      return Petal.Values.To_Boolean (Argument.Default_Value);
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Argument : Formal_Argument) return Integer is
   begin
      return Petal.Values.To_Integer (Argument.Default_Value);
   end Default_Value;

   -------------------
   -- Default_Value --
   -------------------

   function Default_Value (Argument : Formal_Argument) return String is
   begin
      return Petal.Values.To_String (Argument.Default_Value);
   end Default_Value;

   -------------------
   -- Expected_Type --
   -------------------

   function Expected_Type (Argument : Formal_Argument) return Argument_Type is
   begin
      return Argument.Expected_Type;
   end Expected_Type;

   --------------
   -- Is_Named --
   --------------

   function Is_Named (Argument : Formal_Argument) return Boolean is
   begin
      return Argument.Has_Name;
   end Is_Named;

   -------------------
   -- Is_Positional --
   -------------------

   function Is_Positional (Argument : Formal_Argument) return Boolean is
   begin
      return not Argument.Has_Name;
   end Is_Positional;

   ---------------
   -- Long_Name --
   ---------------

   function Long_Name (Argument : Formal_Argument) return String is
      Last : Natural := 0;
   begin
      for I in reverse Argument.Long_Name'Range loop
         if Argument.Long_Name (I) /= ' ' then
            Last := I;
            exit;
         end if;
      end loop;
      return Argument.Long_Name (1 .. Last);
   end Long_Name;

   --------------------
   -- Named_Argument --
   --------------------

   function Named_Argument
     (Long_Name  : String; Short_Name : Character;
      Value_Type : Petal.Values.Value_Type; Expected_Type : Argument_Type)
      return Formal_Argument
   is
      Key : Formal_Argument_Name := (others => ' ');
   begin
      Key (1 .. Long_Name'Length) := Long_Name;

      Formal_Argument_Count :=
        Formal_Argument_Count + 1;

      Formal_Argument_Container (Formal_Argument_Count) :=
           Formal_Argument_Record'
             (Has_Name      => True,
              Long_Name     => Key,
              Short_Name    => Short_Name,
              Value_Type    => Value_Type,
              Expected_Type => Expected_Type,
              Default_Value => Petal.Values.To_Value (False));
      return Formal_Argument_Container (Formal_Argument_Count)'Access;
   end Named_Argument;

   ----------------------------
   -- Named_Boolean_Argument --
   ----------------------------

   function Named_Boolean_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Default_Value : Boolean := False)
      return Formal_Argument
   is
      pragma Unreferenced (Default_Value);
   begin
      return Named_Argument
        (Long_Name     => Long_Name,
         Short_Name    => Short_Name,
         Value_Type    => Petal.Values.Boolean_Value,
         Expected_Type => Value_Argument);
   end Named_Boolean_Argument;

   ----------------------------
   -- Named_Integer_Argument --
   ----------------------------

   function Named_Integer_Argument
     (Long_Name     : String; Short_Name : Character;
      Default_Value : Integer := 0)
      return Formal_Argument
   is
      pragma Unreferenced (Default_Value);
   begin
      return Named_Argument
        (Long_Name     => Long_Name,
         Short_Name    => Short_Name,
         Value_Type    => Petal.Values.Integer_Value,
         Expected_Type => Value_Argument);
   end Named_Integer_Argument;

   --------------------------------
   -- Named_Read_Stream_Argument --
   --------------------------------

   function Named_Read_Stream_Argument
     (Long_Name : String; Short_Name : Character) return Formal_Argument
   is
   begin
      return Named_Argument
        (Long_Name     => Long_Name,
         Short_Name    => Short_Name,
         Value_Type    => Petal.Values.Boolean_Value,
         Expected_Type => Read_Stream_Argument);
   end Named_Read_Stream_Argument;

   ---------------------------
   -- Named_String_Argument --
   ---------------------------

   function Named_String_Argument
     (Long_Name     : String;
      Short_Name    : Character;
      Default_Value : String := "")
      return Formal_Argument
   is
      pragma Unreferenced (Default_Value);
   begin
      return Named_Argument
        (Long_Name     => Long_Name,
         Short_Name    => Short_Name,
         Value_Type    => Petal.Values.String_Value,
         Expected_Type => Value_Argument);
   end Named_String_Argument;

   ---------------------------------
   -- Named_Write_Stream_Argument --
   ---------------------------------

   function Named_Write_Stream_Argument
     (Long_Name : String; Short_Name : Character) return Formal_Argument
   is
   begin
      return Named_Argument
        (Long_Name     => Long_Name,
         Short_Name    => Short_Name,
         Value_Type    => Petal.Values.Boolean_Value,
         Expected_Type => Write_Stream_Argument);
   end Named_Write_Stream_Argument;

   -------------------------
   -- Positional_Argument --
   -------------------------

   function Positional_Argument
     (Key           : String;
      Required      : Boolean;
      Multiple      : Boolean;
      Value_Type    : Petal.Values.Value_Type;
      Expected_Type : Argument_Type)
      return Formal_Argument
   is
      pragma Unreferenced (Required, Multiple);
      Internal_Key : Formal_Argument_Name := (others => ' ');
   begin
      Internal_Key (1 .. Key'Length) := Key;

      Formal_Argument_Count :=
        Formal_Argument_Count + 1;

      Formal_Argument_Container (Formal_Argument_Count) :=
        Formal_Argument_Record'
          (Has_Name      => False,
           Long_Name     => Internal_Key,
           Short_Name    => ' ',
           Value_Type    => Value_Type,
           Expected_Type => Expected_Type,
           Default_Value => Petal.Values.To_Value (False));
      return Formal_Argument_Container (Formal_Argument_Count)'Access;
   end Positional_Argument;

   ---------------------------------
   -- Positional_Integer_Argument --
   ---------------------------------

   function Positional_Integer_Argument
     (Key : String; Required, Multiple : Boolean; Default_Value : Integer := 0)
      return Formal_Argument
   is
      pragma Unreferenced (Default_Value);
   begin
      return Positional_Argument
        (Key           => Key,
         Required      => Required,
         Multiple      => Multiple,
         Value_Type    => Petal.Values.Integer_Value,
         Expected_Type => Value_Argument);
   end Positional_Integer_Argument;

   -------------------------------------
   -- Positional_Read_Stream_Argument --
   -------------------------------------

   function Positional_Read_Stream_Argument
     (Key : String; Required, Multiple : Boolean) return Formal_Argument
   is
   begin
      return Positional_Argument
        (Key           => Key,
         Required      => Required,
         Multiple      => Multiple,
         Value_Type    => Petal.Values.Boolean_Value,
         Expected_Type => Read_Stream_Argument);
   end Positional_Read_Stream_Argument;

   --------------------------------
   -- Positional_String_Argument --
   --------------------------------

   function Positional_String_Argument
     (Key : String; Required, Multiple : Boolean; Default_Value : String := "")
      return Formal_Argument
   is
      pragma Unreferenced (Default_Value);
   begin
      return Positional_Argument
        (Key           => Key,
         Required      => Required,
         Multiple      => Multiple,
         Value_Type    => Petal.Values.String_Value,
         Expected_Type => Value_Argument);
   end Positional_String_Argument;

   --------------------------------------
   -- Positional_Write_Stream_Argument --
   --------------------------------------

   function Positional_Write_Stream_Argument
     (Key : String; Required, Multiple : Boolean) return Formal_Argument
   is
   begin
      return Positional_Argument
        (Key           => Key,
         Required      => Required,
         Multiple      => Multiple,
         Value_Type    => Petal.Values.Boolean_Value,
         Expected_Type => Write_Stream_Argument);
   end Positional_Write_Stream_Argument;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name (Argument : Formal_Argument) return Character is
   begin
      return Argument.Short_Name;
   end Short_Name;

end Petal.Formal_Arguments;
