package Rose.Capabilities.Standard is

   --  The caps that a process started through exec finds in its cap
   --  table, by convention between exec and the process. Processes that
   --  init starts have their own layouts. The launch protocol of ADR 0002,
   --  in which a process is handed its caps, replaces this convention.

   Process_Interface_Cap : constant Capability := 1;
   --  The process's own Process interface (interfaces/process.idl).

   Argument_Cap_Set      : constant Capability := 2;
   --  A Cap_Set (interfaces/cap_set.idl) holding the process's arguments.

end Rose.Capabilities.Standard;
