package Rose.Kernel.Capabilities.Physical_Memory is

   Reserve_Device_Memory : constant Rose.Objects.Endpoint_Index := 1;
   Get_Region_Count      : constant Rose.Objects.Endpoint_Index := 2;
   Get_Region            : constant Rose.Objects.Endpoint_Index := 3;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Physical_Memory;
