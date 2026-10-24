package Rose.Capabilities.Standard is

   --  The caps that a process started through exec finds in its cap
   --  table, by convention between exec and the process. Processes that
   --  init starts have their own layouts.

   Process_Interface_Cap : constant Capability := 1;
   --  The process's own Process interface (interfaces/process.idl).

   Argument_Cap_Set      : constant Capability := 2;
   --  A Cap_Set (interfaces/cap_set.idl) holding the process's arguments.
   --  A command (ADR 0002) has none: see Command_Endpoint_Cap.

   Command_Endpoint_Cap  : constant Capability := 2;
   --  For a command, the receive cap of its command endpoint, which the
   --  kernel creates when the process is created. The shell calls the
   --  endpoint once, with the command's arguments (see
   --  Rose.Command_Line.Launch).

end Rose.Capabilities.Standard;
