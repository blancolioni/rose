package Rose.Kernel.Capabilities.Page_Table is

   Map_Page   : constant Rose.Objects.Endpoint_Index := 1;
   Unmap_Page : constant Rose.Objects.Endpoint_Index := 2;
   Set_Page   : constant Rose.Objects.Endpoint_Index := 3;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Page_Table;
