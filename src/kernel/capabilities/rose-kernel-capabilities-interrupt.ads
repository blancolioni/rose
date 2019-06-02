package Rose.Kernel.Capabilities.Interrupt is

   Reserve_Interrupt_Endpoint : constant := 1;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Interrupt;
