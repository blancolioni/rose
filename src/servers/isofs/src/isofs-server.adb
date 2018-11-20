with Rose.Devices.Block.Client;

with IsoFS.Directories;

package body IsoFS.Server is

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Device : Rose.Devices.Block.Client.Block_Device_Type;
   begin
      Rose.Devices.Block.Client.Open
        (Device, Device_Parameters_Cap, Device_Read_Cap, 0);

      IsoFS.Directories.Read_Root_Directory (Device);

      loop
         null;
      end loop;
   end Start_Server;

end IsoFS.Server;
