package Rose.Kernel.Capabilities.Kernel_Caps is

   Enable_Paging_Endpoint     : constant Rose.Objects.Endpoint_Index := 1;
   Start_Checkpoint_Endpoint  : constant Rose.Objects.Endpoint_Index := 2;
   Start_Image_Write_Endpoint : constant Rose.Objects.Endpoint_Index := 3;
   Create_Process_Endpoint    : constant Rose.Objects.Endpoint_Index := 4;
   Add_Heap_Memory            : constant Rose.Objects.Endpoint_Index := 5;
   Set_Timeout_Endpoint       : constant Rose.Objects.Endpoint_Index := 6;
   Get_Current_Ticks_64_Endpoint : constant Rose.Objects.Endpoint_Index := 7;
   --  Reply with the tick count in two data words: the low 32 bits, then
   --  the high 32 bits. The caller has to ask for two words.

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

   Restart_Endpoint           : constant Rose.Objects.Endpoint_Index := 15;
   --  Restart the machine (#413). Only init has this cap: it restarts
   --  after an install, once the first checkpoint is in the log.

   Boot_Tags_Endpoint         : constant Rose.Objects.Endpoint_Index := 16;
   --  Data word 0 is a multiboot2 tag type, from 1 to 65535.  Reply with
   --  a cap that reads that tag (Read_Boot_Tag_Endpoint) and no other.
   --  Only init has this cap (#582).

   Read_Boot_Tag_Endpoint     : constant Rose.Objects.Endpoint_Index := 17;
   --  The cap's payload is a tag type.  Copy the first tag of that type
   --  from the boot loader's information, header included, into the sent
   --  buffer, which has to be writable, as far as it fits.  The reply's
   --  data word is the tag's whole size, or 0 if there is no such tag.

   Restore_Derivations_Endpoint : constant Rose.Objects.Endpoint_Index := 18;
   --  The sent buffer holds the pages of derivation records that follow
   --  the kernel's header page, and data word 0 is the number of records
   --  (ADR 0001).  Only the restore server has this cap, and calls it
   --  before it restores any process.

   Restored_Endpoint_Endpoint : constant Rose.Objects.Endpoint_Index := 19;
   --  Data words 0 and 1 are the object id of a restored process, words 2
   --  and 3 an endpoint id, and word 4 a capability identifier.  Reply
   --  with an endpoint cap to that endpoint of the process, if it has
   --  one, so that the update boot can call the restored exec and command
   --  registry (#829, ADR 0031).  Only restore-install has this cap.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Kernel_Caps;
