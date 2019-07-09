with Ada.Text_IO;

package body Petal.Commands is

   Max_Named_Arguments   : constant := 20;
   Max_Indexed_Arguments : constant := 5;

   Max_Commands : constant := 100;

   type Petal_Command_Record is
      record
         Built_In             : Boolean;
         Execute              : Built_In_Handler;
         Launch               : Rose.Capabilities.Capability;
         Named_Formal_Count   : Natural;
         Indexed_Formal_Count : Natural;
         Has_Extra_Formals    : Boolean;
         Named_Formals        : Petal.Formal_Arguments.Formal_Argument_Array
           (1 .. Max_Named_Arguments);
         Indexed_Formals      : Petal.Formal_Arguments.Formal_Argument_Array
           (1 .. Max_Indexed_Arguments);
         Extra_Formals        : Petal.Formal_Arguments.Formal_Argument;
      end record;

   Command_Array : array (1 .. Max_Commands) of aliased Petal_Command_Record;
   Command_Count : Natural := 0;

   ----------------------------
   -- Default_Launch_Context --
   ----------------------------

   function Default_Launch_Context return Launch_Context is
   begin
      return Launch_Context'
        (Standard_Caps => (others => 0),
         Extra_Caps    => (others => 0),
         Wait          => True);
   end Default_Launch_Context;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Command : Petal_Command) return Boolean is
   begin
      return Command.Built_In;
   end Is_Built_In;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Command   : Petal_Command;
      Context   : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
   is
      Result : constant Petal.Values.Petal_Value :=
                 Execute (Command, Context, Arguments);
   begin
      pragma Unreferenced (Result);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command   : Petal_Command; Context : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
      return Petal.Values.Petal_Value
   is
   begin
      if Command.Built_In then
         return Command.Execute (Context, Arguments);
      else
         Ada.Text_IO.Put_Line
           ("launching:" & Command.Launch'Image);
         return Petal.Values.To_Value (True);
      end if;
   end Execute;

   ---------------------
   -- Formal_Argument --
   ---------------------

   function Formal_Argument
     (Command : Petal_Command; Long_Name : String)
      return Petal.Formal_Arguments.Formal_Argument
   is
   begin
      for Argument of Command.Named_Formals loop
         if Petal.Formal_Arguments.Long_Name (Argument) = Long_Name then
            return Argument;
         end if;
      end loop;
      return Petal.Formal_Arguments.No_Formal_Argument;
   end Formal_Argument;

   ---------------------
   -- Formal_Argument --
   ---------------------

   function Formal_Argument
     (Command : Petal_Command; Short_Name : Character)
      return Petal.Formal_Arguments.Formal_Argument
   is
   begin
      for Argument of Command.Named_Formals loop
         if Petal.Formal_Arguments.Short_Name (Argument) = Short_Name then
            return Argument;
         end if;
      end loop;
      return Petal.Formal_Arguments.No_Formal_Argument;
   end Formal_Argument;

   ----------------------------
   -- Indexed_Argument_Count --
   ----------------------------

   function Indexed_Argument_Count (Command : Petal_Command) return Natural is
   begin
      return Command.Indexed_Formal_Count;
   end Indexed_Argument_Count;

   ----------------------
   -- Indexed_Argument --
   ----------------------

   function Indexed_Argument
     (Command : Petal_Command;
      Index   : Positive)
      return Petal.Formal_Arguments.Formal_Argument
   is
   begin
      return Command.Indexed_Formals (Index);
   end Indexed_Argument;

   -------------------------
   -- Has_Extra_Arguments --
   -------------------------

   function Has_Extra_Arguments (Command : Petal_Command) return Boolean is
   begin
      return Command.Has_Extra_Formals;
   end Has_Extra_Arguments;

   ---------------------
   -- Extra_Arguments --
   ---------------------

   function Extra_Arguments
     (Command : Petal_Command) return Petal.Formal_Arguments.Formal_Argument
   is
   begin
      return Command.Extra_Formals;
   end Extra_Arguments;

   ---------------------
   -- Create_Built_In --
   ---------------------

   function Create_Built_In
     (Handler           : Built_In_Handler;
      Formal_Arguments  : Petal.Formal_Arguments.Formal_Argument_Array;
      Indexed_Arguments : Petal.Formal_Arguments.Formal_Argument_Array;
      Extra_Arguments   : Petal.Formal_Arguments.Formal_Argument)
      return Petal_Command
   is
      use Petal.Formal_Arguments;
      Rec : Petal_Command_Record renames Command_Array (Command_Count + 1);
   begin
      Rec := Petal_Command_Record'
        (Built_In             => True,
         Execute              => Handler,
         Launch               => Rose.Capabilities.Null_Capability,
         Named_Formal_Count   => Formal_Arguments'Length,
         Indexed_Formal_Count => Indexed_Arguments'Length,
         Has_Extra_Formals    => Extra_Arguments /= No_Formal_Argument,
         Named_Formals        => (others => No_Formal_Argument),
         Indexed_Formals      => (others => No_Formal_Argument),
         Extra_Formals        => Extra_Arguments);
      Rec.Named_Formals (1 .. Formal_Arguments'Length) := Formal_Arguments;
      Rec.Indexed_Formals (1 .. Indexed_Arguments'Length) := Indexed_Arguments;

      Command_Count := Command_Count + 1;
      return Command_Array (Command_Count)'Access;
   end Create_Built_In;

   ----------------------------
   -- Create_With_Launch_Cap --
   ----------------------------

   function Create_With_Launch_Cap
     (Launch_Cap        : Rose.Capabilities.Capability;
      Formal_Arguments  : Petal.Formal_Arguments.Formal_Argument_Array;
      Indexed_Arguments : Petal.Formal_Arguments.Formal_Argument_Array;
      Extra_Arguments   : Petal.Formal_Arguments.Formal_Argument)
      return Petal_Command
   is
      use Petal.Formal_Arguments;
      Rec : Petal_Command_Record renames Command_Array (Command_Count + 1);
   begin
      Rec := Petal_Command_Record'
        (Built_In             => True,
         Execute              => null,
         Launch               => Launch_Cap,
         Named_Formal_Count   => Formal_Arguments'Length,
         Indexed_Formal_Count => Indexed_Arguments'Length,
         Has_Extra_Formals    => Extra_Arguments /= No_Formal_Argument,
         Named_Formals        => (others => No_Formal_Argument),
         Indexed_Formals      => (others => No_Formal_Argument),
         Extra_Formals        => Extra_Arguments);
      Rec.Named_Formals (1 .. Formal_Arguments'Length) := Formal_Arguments;
      Rec.Indexed_Formals (1 .. Indexed_Arguments'Length) := Indexed_Arguments;

      Command_Count := Command_Count + 1;
      return Command_Array (Command_Count)'Access;
   end Create_With_Launch_Cap;

end Petal.Commands;
