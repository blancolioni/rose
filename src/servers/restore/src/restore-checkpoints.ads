with Rose.Capabilities;
with Rose.Words;

package Restore.Checkpoints is

   procedure Restore
     (Reader_Cap : Rose.Capabilities.Capability;
      Checkpoint : Rose.Words.Word_64;
      Success    : out Boolean);
   --  Bring back the persistent processes from the checkpoint at
   --  Checkpoint in the log, read through the Log_Reader interface at
   --  Reader_Cap:
   --
   --  * the storage server's regions;
   --  * each process in the kernel, not yet runnable;
   --  * each process in the memory server, with its segments;
   --  * the newest saved contents of each page they changed, from any
   --    entry up to and including Checkpoint;
   --  * the interrupts they handle.
   --
   --  The backing store must have been added first.  The processes can't
   --  run until Resume_Processes.

   procedure Resume_Processes;
   --  Let the processes Restore brought back run.  This waits until init
   --  has started the boot modules it starts after a restore: the kernel
   --  tells the memory server about a new boot module only if it is
   --  waiting for a message, and running processes keep it busy with
   --  their page faults.

end Restore.Checkpoints;
