with Rose.Capabilities;

package Rose.Standard_Streams is

   --  The caps of the program's standard streams, as Ada.Text_IO uses
   --  them: the Read cap of standard input, and the Write caps of standard
   --  output and standard error. A cap is null if the program has no such
   --  stream.
   --
   --  A program launched with an argument cap set (see
   --  Rose.Capabilities.Standard) finds its streams there, the first time
   --  one is asked for. A command (ADR 0002) has no argument cap set: its
   --  launch invocation gives it the streams it declares, and
   --  Rose.Command_Line.Launch passes them on with Set and the Set_
   --  procedures below, before the command runs.

   function Standard_Input return Rose.Capabilities.Capability;
   function Standard_Output return Rose.Capabilities.Capability;
   function Standard_Error return Rose.Capabilities.Capability;

   procedure Set
     (Standard_Input  : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
      Standard_Output : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
      Standard_Error  : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability);
   --  Use these streams, and never look in the argument cap set.

   procedure Set_Standard_Input (Cap : Rose.Capabilities.Capability);
   procedure Set_Standard_Output (Cap : Rose.Capabilities.Capability);
   procedure Set_Standard_Error (Cap : Rose.Capabilities.Capability);
   --  Use Cap for one stream, and never look in the argument cap set.

end Rose.Standard_Streams;
