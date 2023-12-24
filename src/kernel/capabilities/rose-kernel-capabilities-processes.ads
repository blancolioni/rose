package Rose.Kernel.Capabilities.Processes is

   Process_Interface_Endpoint : constant Rose.Objects.Endpoint_Index := 1;
   Resume_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 2;
   Faulted_Process_Endpoint   : constant Rose.Objects.Endpoint_Index := 3;
   Notify_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 4;
   Start_Process_Endpoint     : constant Rose.Objects.Endpoint_Index := 5;
   Initial_Cap_Endpoint       : constant Rose.Objects.Endpoint_Index := 6;
   Kill_Process_Endpoint      : constant Rose.Objects.Endpoint_Index := 7;
   Get_Name_Endpoint          : constant Rose.Objects.Endpoint_Index := 8;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Processes;
