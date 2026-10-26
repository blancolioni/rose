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

   Start_Log_Endpoint         : constant Rose.Objects.Endpoint_Index := 10;

   Trace_Endpoint             : constant Rose.Objects.Endpoint_Index := 11;
   --  Write the text in the data words, four characters to a word, to the
   --  log as a trace event of the calling process, if tracing is on
   --  (Trace_Launch) and there is text. The reply's data word is the
   --  current tick count. See Rose.Trace.

   --  Restoring a checkpoint (Rose.Kernel.Processes.Snapshot). Only the
   --  restore server has these caps.

   Restore_Process_Endpoint   : constant Rose.Objects.Endpoint_Index := 12;
   --  The sent buffer holds a process record page and its cap pages.
   --  Recreate the process, not yet runnable, and reply with the same
   --  four caps as Create_Process, and its object id in the data words.

   Resume_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 13;
   --  Let the restored process whose object id is in data words 0 and 1
   --  run.

   Restore_Interrupt_Endpoint : constant Rose.Objects.Endpoint_Index := 14;
   --  Send interrupt data word 0 to the endpoint cap data word 3 of the
   --  restored process whose object id is in data words 1 and 2.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Kernel_Caps;
