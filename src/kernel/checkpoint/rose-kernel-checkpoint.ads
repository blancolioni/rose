with Rose.Capabilities;

package Rose.Kernel.Checkpoint is

   procedure Enter_Checkpoint;
   procedure Leave_Checkpoint;

   procedure Set_Log_Handlers
     (Create, Append, Commit : Rose.Capabilities.Capability);

end Rose.Kernel.Checkpoint;
