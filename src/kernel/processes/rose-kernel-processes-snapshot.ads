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
     (Pages          : System.Storage_Elements.Storage_Array;
      Next_Object_Id : Rose.Objects.Object_Id;
      Pid            : out Process_Id;
      Success        : out Boolean);
   --  Recreate the process whose record page and capability pages are
   --  Pages, with the object id, caps, endpoints and registers it had,
   --  in any free entry.  It is not runnable until
   --  Resume_Restored_Process, which decides what happens to a call
   --  that it was blocked in.  Next_Object_Id is the next object id
   --  from the checkpoint's header, and new processes have ids from
   --  there on.  Success is False if the record is not valid, its object
   --  id is a boot module's or a live process's, or there is no free
   --  entry.

   procedure Resume_Restored_Process
     (Pid     : Process_Id;
      Success : out Boolean);
   --  Let the restored process Pid run.  If it was blocked in a call,
   --  and a restored server holds the call, it goes on waiting for the
   --  reply; otherwise it makes the call again, since the server has no
   --  record of it (ADR 0004, section 4).  Every process must have been
   --  restored before any is resumed.

   procedure Restore_Derivations
     (Pages        : System.Storage_Elements.Storage_Array;
      Record_Count : Natural;
      Success      : out Boolean);
   --  Put the derivation records in Pages back, which are those of
   --  Rose.Checkpoints.Saved_Record, numbered 1 to Record_Count.  It must
   --  come before the first Restore_Process, since the capabilities that
   --  are restored name the records.  Success is False, and nothing is
   --  restored, if the records are not a consistent table, or there are
   --  records already.  Each record has a slot of its own until
   --  Release_Restore_Holds, which counts the capabilities that name it.

   procedure Release_Restore_Holds;
   --  Every process has been restored, and so every capability that names
   --  a record has counted it: drop the slot that Restore_Derivations
   --  gave each record, which frees the records that nothing names.  It
   --  does nothing if it has done it, or if there was nothing to restore.

   procedure Restore_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Object    : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability;
      Success   : out Boolean);
   --  Send Interrupt to the endpoint cap Cap of the restored process
   --  Object again, as before the restart.

end Rose.Kernel.Processes.Snapshot;
