with Rose.Devices.Block.Client;

package IsoFS.Directories is

   ISO_Sector_Size         : constant := 2048;
   ISO_First_Volume_Sector : constant := 16;

   Descriptor_Type_Offset  : constant := 0;
   Boot_Record_Descriptor  : constant := 0;
   Primary_Volume_Descriptor : constant := 1;

   procedure Read_Root_Directory
     (Device : Rose.Devices.Block.Client.Block_Device_Type);

end IsoFS.Directories;
