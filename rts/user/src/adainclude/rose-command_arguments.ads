package Rose.Command_Arguments is

   pragma Preelaborate;

   --  The command name and the arguments of the program, as
   --  Ada.Command_Line reports them, and its exit status.
   --
   --  Rose has no argv. A command (ADR 0002) receives its arguments in its
   --  launch invocation, after the program has been elaborated, so these
   --  are empty until Rose.Command_Line.Launch.Set_Command_Line has been
   --  called, before the command runs. A program that does not declare an
   --  argument list has no arguments.

   Max_Name_Length : constant := 128;
   --  A longer command name is cut off.

   Max_Text_Length : constant := 4096;
   --  The total length of all the arguments, which is that of a launch
   --  buffer (Rose.Command_Line.Launch.Max_Buffer_Length).

   Max_Arguments : constant := 1024;

   function Argument_Count return Natural;

   function Argument (Index : Positive) return String;
   --  The argument at Index, or "" if Index is greater than
   --  Argument_Count. (The run-time system has no assertions, so there is
   --  no precondition.)

   function Command_Name return String;

   function Exit_Status return Integer;
   --  What the program has asked for with Set_Exit_Status, which is zero
   --  until it does.

   procedure Add_Argument (Item : String);
   --  Add Item after the arguments there are. Raises Storage_Error if
   --  there are too many arguments, or if they are too long.

   procedure Set_Command_Name (Name : String);

   procedure Set_Exit_Status (Code : Integer);

end Rose.Command_Arguments;
