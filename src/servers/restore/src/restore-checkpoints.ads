with Rose.Capabilities;
with Rose.Objects;
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

   function Restored_Process_Count return Natural;
   --  The number of processes that Restore brought back.

   procedure Get_Restored_Name
     (Index : Positive;
      Name  : out String;
      Last  : out Natural);
   --  The name of restored process Index, from the checkpoint, in
   --  Name (Name'First .. Last).

   function Find_Endpoint
     (Index        : Positive;
      Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;
   --  A cap to the interface endpoint Interface_Id, with identifier 0, of
   --  restored process Index, or null if it has none. Only
   --  restore-install can do this, for the update boot (#829, #830).

   function Find_Server
     (Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;
   --  A cap to the interface endpoint Interface_Id, with identifier 0,
   --  of the first restored process that has it, or null if none has.
   --  Calling it returns the caps of the interface of the server's first
   --  object, as a published interface does.  Only restore-install can
   --  do this, for the update boot (#829).

end Restore.Checkpoints;
