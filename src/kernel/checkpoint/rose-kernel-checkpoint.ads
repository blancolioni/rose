with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;

with Rose.Kernel.Processes;

package Rose.Kernel.Checkpoint is

   procedure Enter_Checkpoint
     (Tables     : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean);
   --  Write a snapshot of the kernel's tables for the persistent
   --  processes into Tables (see Rose.Checkpoints), set Page_Count to
   --  the number of pages it takes, and enter checkpoint mode.  If the
   --  snapshot doesn't fit, Success is False and nothing else happens.

   procedure Leave_Checkpoint;

   procedure Set_Log_Handlers
     (Create, Append, Commit : Rose.Capabilities.Capability);

   --  The log is told when a checkpoint starts (Create) and ends (Commit).
   --  A notice is sent only while the log is waiting to receive it.  If
   --  the log is busy, in a call of its own for example, the notice waits
   --  until the log next receives, and Create goes before Commit (#1013).

   function Has_Log_Notice
     (Process  : Rose.Kernel.Processes.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean;
   --  True if Process is the log, and a notice waits for it that a
   --  receive on Endpoint takes (0 takes any).

   procedure Send_Log_Notice;
   --  Send the log the first notice that waits for it.  The log must be
   --  blocked in its receive.

end Rose.Kernel.Checkpoint;
