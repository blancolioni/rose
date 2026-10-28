with Rose.Init_Capabilities;

package Rose.Kernel.Capabilities.Physical_Memory is

   Reserve_Device_Memory : constant Rose.Objects.Endpoint_Index := 1;
   Get_Region_Count      : constant Rose.Objects.Endpoint_Index := 2;
   Get_Region            : constant Rose.Objects.Endpoint_Index := 3;

   Device_Page           : constant Rose.Objects.Endpoint_Index :=
                             Rose.Init_Capabilities.Device_Page_Endpoint;
   --  Only init's Device_Pages_Cap has this endpoint
   --  (Rose.Init_Capabilities).  It replies with a cap that maps the
   --  physical page in data word 0 at the same address, unless the page
   --  is RAM.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Physical_Memory;
