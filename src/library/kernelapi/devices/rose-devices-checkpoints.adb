with Rose.Console_IO;
with Rose.Words;

package body Rose.Devices.Checkpoints is

   use System.Storage_Elements;

   subtype Checkpoint_Page_Data is Storage_Array (1 .. 4080);

   type Checkpoint_Page is
      record
         Begin_Magic : Rose.Words.Word_64;
         Data        : Checkpoint_Page_Data;
         End_Magic   : Rose.Words.Word_64;
      end record
     with Size => 8 * 4096;

--     Current_Page_Last    : Storage_Offset;
   Current_Page_Address : Rose.Interfaces.Block_Device.Block_Address_Type;
   Current_Page         : Checkpoint_Page :=
                            Checkpoint_Page'
                              (Begin_Magic => Checkpoint_Begin_Magic,
                               Data        => (others => 0),
                               End_Magic   => Checkpoint_End_Magic);

   -------------------------
   -- Finish_System_Image --
   -------------------------

   procedure Finish_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Page : Storage_Array (1 .. 4096);
      pragma Import (Ada, Page);
      for Page'Address use Current_Page'Address;
   begin
      Rose.Console_IO.Put_Line ("committing system image");
      Rose.Interfaces.Block_Device.Client.Write_Blocks
        (Item   => Device,
         Start  => Current_Page_Address,
         Count  => 1,
         Blocks => Page);
   end Finish_System_Image;

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

   ------------------------
   -- Start_System_Image --
   ------------------------

   procedure Start_System_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      pragma Unreferenced (Device);
   begin
      Current_Page_Address := 0;
--        Current_Page_Last := 0;
      Current_Page :=
        Checkpoint_Page'
          (Begin_Magic => Checkpoint_Begin_Magic,
           Data        => (others => 0),
           End_Magic   => Checkpoint_End_Magic);
   end Start_System_Image;

   procedure Write_Image
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Image  : System.Storage_Elements.Storage_Array)
   is null;

end Rose.Devices.Checkpoints;
