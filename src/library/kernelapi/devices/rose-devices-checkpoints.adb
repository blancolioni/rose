with System.Storage_Elements;
with Rose.Words;

package body Rose.Devices.Checkpoints is

   type Checkpoint_Page_Data is
     array (1 .. 4080) of Rose.Words.Word_8;

   type Checkpoint_Page is
      record
         Begin_Magic : Rose.Words.Word_64;
         Data        : Checkpoint_Page_Data;
         End_Magic   : Rose.Words.Word_64;
      end record
     with Size => 8 * 4096;

   --------------------
   -- Has_Checkpoint --
   --------------------

   function Has_Checkpoint
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Boolean
   is
      use Rose.Words;
      use Rose.Interfaces.Block_Device.Client;
      Storage : System.Storage_Elements.Storage_Array (1 .. 4096);
      Header  : Checkpoint_Page;
      pragma Import (Ada, Header);
      for Header'Address use Storage'Address;
   begin
      Read_Blocks (Device, 0, 1, Storage);

      return Header.Begin_Magic = Checkpoint_Begin_Magic
        and then Header.End_Magic = Checkpoint_End_Magic;
   end Has_Checkpoint;

end Rose.Devices.Checkpoints;
