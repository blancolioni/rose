with Rose.Capabilities;

with Petal.Actual_Arguments;
with Petal.Formal_Arguments;
with Petal.Values;

package Petal.Commands is

   type Launch_Context is private;

   function Default_Launch_Context return Launch_Context;

   type Petal_Command is private;

   function Is_Built_In
     (Command : Petal_Command)
      return Boolean;

   procedure Execute
     (Command   : Petal_Command;
      Context   : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container);

   function Execute
     (Command   : Petal_Command;
      Context   : Launch_Context;
      Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
     return Petal.Values.Petal_Value;

   function Formal_Argument
     (Command   : Petal_Command;
      Long_Name : String)
      return Petal.Formal_Arguments.Formal_Argument;

   function Formal_Argument
     (Command    : Petal_Command;
      Short_Name : Character)
      return Petal.Formal_Arguments.Formal_Argument;

   function Indexed_Argument_Count
     (Command : Petal_Command)
      return Natural;

   function Indexed_Argument
     (Command : Petal_Command;
      Index   : Positive)
      return Petal.Formal_Arguments.Formal_Argument;

   function Has_Extra_Arguments
     (Command : Petal_Command)
      return Boolean;

   function Extra_Arguments
     (Command : Petal_Command)
      return Petal.Formal_Arguments.Formal_Argument;

private

   type Standard_Launch_Caps is
     (Cap_Exit,
      Cap_Create_Endpoint,
      Cap_Memory,
      Cap_Standard_Input,
      Cap_Standard_Output,
      Cap_Standard_Error,
      Cap_Current_Directory,
      Cap_Clock,
      Cap_Construct_Cap_Set);

   type Standard_Launch_Cap_Array is
     array (Standard_Launch_Caps) of Rose.Capabilities.Capability;

   Max_Extra_Launch_Caps : constant := 10;
   type Extra_Launch_Cap_Array is
     array (1 .. Max_Extra_Launch_Caps) of Rose.Capabilities.Capability;

   type Launch_Context is
      record
         Standard_Caps : Standard_Launch_Cap_Array;
         Extra_Caps    : Extra_Launch_Cap_Array;
         Wait : Boolean;
      end record;

   type Petal_Command_Record;

   type Petal_Command is access constant Petal_Command_Record;

   type Built_In_Handler is access
     function (Context   : Launch_Context;
               Arguments : Petal.Actual_Arguments.Actual_Argument_Container)
               return Petal.Values.Petal_Value;

   function Create_Built_In
     (Handler           : Built_In_Handler;
      Formal_Arguments  : Petal.Formal_Arguments.Formal_Argument_Array;
      Indexed_Arguments : Petal.Formal_Arguments.Formal_Argument_Array;
      Extra_Arguments   : Petal.Formal_Arguments.Formal_Argument)
      return Petal_Command;

   function Create_With_Launch_Cap
     (Launch_Cap        : Rose.Capabilities.Capability;
      Formal_Arguments  : Petal.Formal_Arguments.Formal_Argument_Array;
      Indexed_Arguments : Petal.Formal_Arguments.Formal_Argument_Array;
      Extra_Arguments   : Petal.Formal_Arguments.Formal_Argument)
      return Petal_Command;

end Petal.Commands;
