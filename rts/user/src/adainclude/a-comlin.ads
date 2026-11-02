package Ada.Command_Line is

   pragma Preelaborate;

   --  Rose has no argv, so this is not GNAT's package (#498, ADR 0023).
   --  The arguments are those of the command's argument list, the one
   --  String_List in its signature, if it has one. The shell binds named
   --  arguments and options by the signature, so they are not among them.
   --  The command's arguments arrive after the program has been
   --  elaborated, so Argument_Count is 0 until the command's main starts.

   function Argument_Count return Natural;

   function Argument (Number : Positive) return String;
   --  Raises Constraint_Error if Number is greater than Argument_Count.

   function Command_Name return String;
   --  The full name of the command in its IDL declaration, such as
   --  Rose.Core.Echo.

   type Exit_Status is new Integer;

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;

   procedure Set_Exit_Status (Code : Exit_Status);
   --  A command that finishes with a status other than Success fails, and
   --  the shell discards what it has written (ADR 0012), with the message
   --  "exit status N".

end Ada.Command_Line;
