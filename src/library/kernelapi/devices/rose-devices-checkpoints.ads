with System.Storage_Elements;

with Rose.Interfaces.Block_Device.Client;

package Rose.Devices.Checkpoints is

   Checkpoint_Begin_Magic : constant := 16#9d40b6e66b255ba1#;
   Checkpoint_End_Magic   : constant := 16#242762f947ce1070#;

   --  Checkpoints are in the checkpoint log, not on the storage device;
   --  the restore server finds them with the log server's Log_Reader
   --  interface.

   procedure Start_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   procedure Write_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Image  : System.Storage_Elements.Storage_Array);

   procedure Finish_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client);


end Rose.Devices.Checkpoints;
