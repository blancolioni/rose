package Rose.Kernel.Capabilities.Kernel_Caps is

   Enable_Paging_Endpoint     : constant Rose.Objects.Endpoint_Index := 1;
   Start_Checkpoint_Endpoint  : constant Rose.Objects.Endpoint_Index := 2;
   Start_Image_Write_Endpoint : constant Rose.Objects.Endpoint_Index := 3;
   Create_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 4;
   Add_Heap_Memory            : constant Rose.Objects.Endpoint_Index := 5;
   Set_Timeout_Endpoint       : constant Rose.Objects.Endpoint_Index := 6;
   Get_Current_Ticks_Endpoint : constant Rose.Objects.Endpoint_Index := 7;

   Enter_Checkpoint_Endpoint  : constant Rose.Objects.Endpoint_Index := 8;
   Leave_Checkpoint_Endpoint  : constant Rose.Objects.Endpoint_Index := 9;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Kernel_Caps;
