with System.Storage_Elements;

with Rose.Arch.Interrupts;

package Rose.Kernel.Processes.Snapshot is

   procedure Write_Tables
     (Storage    : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean);
   --  Write the kernel's tables for every persistent process into
   --  Storage, in the format described in Rose.Checkpoints, and set
   --  Page_Count to the number of pages used.  Success is False, and
   --  nothing is written, if Storage is too small; it is also False if
   --  more processes are waiting to send to one process than its record
   --  can hold, or persistent processes handle more interrupts than the
   --  header page can hold.

   --  Restoring a checkpoint: each saved process is recreated with
   --  Restore_Process, while it can't run, then the memory server is
   --  given its segments and pages, and then it is started with
   --  Resume_Restored_Process.

   procedure Restore_Process
     (Pages   : System.Storage_Elements.Storage_Array;
      Pid     : out Process_Id;
      Success : out Boolean);
   --  Recreate the process whose record page and capability pages are
   --  Pages, with the object id, caps, endpoints and registers it had.
   --  It is not runnable until Resume_Restored_Process.  A process that
   --  was blocked in a call makes the call again when it runs, since
   --  whatever it was waiting for was lost in the restart.  Success is
   --  False if the record is not valid, or the process's slot is in
   --  use.

   procedure Resume_Restored_Process
     (Pid     : Process_Id;
      Success : out Boolean);
   --  Let the restored process Pid run.

   procedure Restore_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Object    : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability;
      Success   : out Boolean);
   --  Send Interrupt to the endpoint cap Cap of the restored process
   --  Object again, as before the restart.

end Rose.Kernel.Processes.Snapshot;
