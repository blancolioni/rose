with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 3;
   Console_Cap          : constant Rose.Capabilities.Capability := 4;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 5;
   Region_Cap           : constant Rose.Capabilities.Capability := 6;
   Bank_Cap             : constant Rose.Capabilities.Capability := 7;
   --  The bank that exec launches a program against when its caller
   --  gives none (ADR 0014, #449; ADR 0015, #750): the root bank, until
   --  #450 gives it the system bank.

   Launch_Command_Cap   : constant Rose.Capabilities.Capability := 8;
   --  The ELF server's Launch_Command (interfaces/executable.idl).

   Trace_Cap            : constant Rose.Capabilities.Capability := 9;
   --  For trace events (Rose.Trace).

end Exec;
