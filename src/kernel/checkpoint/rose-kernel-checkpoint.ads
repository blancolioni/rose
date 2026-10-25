with System.Storage_Elements;

with Rose.Capabilities;

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

end Rose.Kernel.Checkpoint;
