package Rose.Kernel.Capabilities.Kernel_Caps is

   Enable_Paging_Endpoint : constant Rose.Objects.Endpoint_Index := 1;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Kernel_Caps;
