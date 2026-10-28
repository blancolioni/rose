with Rose.Capabilities.Layout;
with Rose.Init_Capabilities;

package Rose.Kernel.Capabilities.Arch is

   Port_Out_Endpoint       : constant Rose.Objects.Endpoint_Id :=
                               Rose.Init_Capabilities.Port_Out_Endpoint;
   Port_In_Endpoint        : constant Rose.Objects.Endpoint_Id :=
                               Rose.Init_Capabilities.Port_In_Endpoint;
   Port_Out_Range_Endpoint : constant Rose.Objects.Endpoint_Id :=
                               Rose.Init_Capabilities.Port_Out_Range_Endpoint;
   Port_In_Range_Endpoint  : constant Rose.Objects.Endpoint_Id :=
                               Rose.Init_Capabilities.Port_In_Range_Endpoint;

   Derive_Endpoint         : constant Rose.Objects.Endpoint_Id :=
                               Rose.Init_Capabilities.Derive_Ports_Endpoint;
   --  Only init's Ports_Cap has this endpoint (Rose.Init_Capabilities).
   --  It replies with a port cap for a range of ports inside its own.

   type Port_IO_Size is (Data_8, Data_16, Data_32, Data_64);

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Arch;
