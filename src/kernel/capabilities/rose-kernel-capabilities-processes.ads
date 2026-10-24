package Rose.Kernel.Capabilities.Processes is

   Process_Interface_Endpoint : constant Rose.Objects.Endpoint_Index := 1;
   Resume_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 2;
   Faulted_Process_Endpoint   : constant Rose.Objects.Endpoint_Index := 3;
   Notify_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 4;
   Start_Process_Endpoint     : constant Rose.Objects.Endpoint_Index := 5;
   Initial_Cap_Endpoint       : constant Rose.Objects.Endpoint_Index := 6;
   Kill_Process_Endpoint      : constant Rose.Objects.Endpoint_Index := 7;
   Get_Name_Endpoint          : constant Rose.Objects.Endpoint_Index := 8;
   Command_Endpoint_Endpoint  : constant Rose.Objects.Endpoint_Index := 9;
   --  Create the command endpoint of a new process (ADR 0002): the
   --  endpoint id is in data words 0 and 1. The receive cap goes in the
   --  process's next free cap, and the reply is the endpoint cap.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Processes;
