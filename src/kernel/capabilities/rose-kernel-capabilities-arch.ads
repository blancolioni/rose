with Rose.Capabilities.Layout;

package Rose.Kernel.Capabilities.Arch is

   Port_Out_Endpoint       : constant Rose.Objects.Endpoint_Id := 1;
   Port_In_Endpoint        : constant Rose.Objects.Endpoint_Id := 2;
   Port_Out_Range_Endpoint : constant Rose.Objects.Endpoint_Id := 3;
   Port_In_Range_Endpoint  : constant Rose.Objects.Endpoint_Id := 4;

   type Port_IO_Size is (Data_8, Data_16, Data_32, Data_64);

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Arch;
