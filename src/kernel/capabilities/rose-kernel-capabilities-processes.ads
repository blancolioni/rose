package Rose.Kernel.Capabilities.Processes is

   Resume_Process_Endpoint : constant Rose.Objects.Endpoint_Index := 1;
   Faulted_Process_Endpoint : constant Rose.Objects.Endpoint_Index := 2;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Processes;
