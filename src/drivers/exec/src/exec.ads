with Rose.Capabilities;

package Exec is

   pragma Pure (Exec);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Delete_Endpoint_Cap  : constant Rose.Capabilities.Capability := 3;
   Rescind_Endpoint_Cap : constant Rose.Capabilities.Capability := 4;
   Console_Cap          : constant Rose.Capabilities.Capability := 5;

   Create_Process_Cap   : constant Rose.Capabilities.Capability := 6;
   Region_Cap           : constant Rose.Capabilities.Capability := 7;
   Bank_Cap             : constant Rose.Capabilities.Capability := 8;
   --  The bank that the programs exec launches are launched against
   --  (ADR 0014, #449): the root bank, until #450 gives it the system
   --  bank.

   Launch_Command_Cap   : constant Rose.Capabilities.Capability := 9;
   --  The ELF server's Launch_Command (interfaces/executable.idl).

   Trace_Cap            : constant Rose.Capabilities.Capability := 10;
   --  For trace events (Rose.Trace).

end Exec;
