package Rose.Kernel.Capabilities.Pages is

   Identity_Map_Page  : constant Rose.Objects.Endpoint_Index := 1;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Pages;
