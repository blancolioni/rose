with Rose.Interfaces.Block_Device.Client;

package Rose.Devices.Checkpoints is

   Checkpoint_Begin_Magic : constant := 16#9d40b6e66b255ba1#;
   Checkpoint_End_Magic   : constant := 16#242762f947ce1070#;

   function Has_Checkpoint
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
     return Boolean;

end Rose.Devices.Checkpoints;
