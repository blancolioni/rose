package Rose.Kernel.Capabilities.Page_Table is

   Map_Page : constant Rose.Objects.Endpoint_Index := 1;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Page_Table;
