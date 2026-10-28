with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Directories;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.File_Reader.Client;

package body Restore.Boot_Disk is

   use System.Storage_Elements;
   use Rose.Interfaces.Block_Device;
   use Rose.Interfaces.Block_Device.Client;

   Boot_Directory : constant String := "/rose/install/boot/";

   MBR_Code_Bytes : constant := 440;
   --  The part of block 0 that is boot code.  The rest holds the disk
   --  signature and the protective MBR, which the GPT needs.

   Buffer : Storage_Array (1 .. Storage_Offset (Max_Block_Size))
     with Alignment => 4096;

   procedure Write_File
     (Name   : String;
      Device : Rose.Capabilities.Capability);
   --  Write the file Name in Boot_Directory to Device, from its first
   --  block, if it fits

   procedure Write_MBR;

   -----------------------
   -- Write_Boot_Loader --
   -----------------------

   procedure Write_Boot_Loader is
   begin
      Rose.Console_IO.Put_Line ("restore: writing boot loader");
      Write_MBR;
      Write_File ("core.img", BIOS_Boot_Cap);
      Write_File ("boot.fat", Boot_Partition_Cap);
   end Write_Boot_Loader;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File
     (Name   : String;
      Device : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Directories.File_Size;
      Path        : constant String := Boot_Directory & Name;
      Client      : Block_Device_Client;
      Block_Count : Block_Address_Type;
      Block_Size  : Block_Size_Type;
   begin
      if Device = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line
           ("restore: no partition for " & Name & "; not written");
         return;
      end if;

      if not Rose.Directories.Exists (Path) then
         Rose.Console_IO.Put_Line ("restore: cannot find " & Path);
         return;
      end if;

      Open (Client, Device);
      Get_Parameters (Client, Block_Count, Block_Size);

      if Block_Size = 0
        or else Rose.Directories.Size (Path)
          > Rose.Directories.File_Size (Block_Count)
            * Rose.Directories.File_Size (Block_Size)
      then
         Rose.Console_IO.Put_Line
           ("restore: " & Name & " does not fit its partition;"
            & " not written");
         return;
      end if;

      declare
         use Rose.Interfaces.File_Reader.Client;
         Reader  : File_Reader_Client;
         Block   : Storage_Array renames
                     Buffer (1 .. Storage_Offset (Block_Size));
         Filled  : Storage_Count := 0;
         Address : Block_Address_Type := 0;
      begin
         Rose.Directories.Open (Reader, Path);

         loop
            declare
               Count : Storage_Count;
               --  Read gives the number of bytes it read
            begin
               Read (Reader, Block (Filled + 1 .. Block'Last), Count);
               exit when Rose.Interfaces.File_Reader.Client.Has_Error
                 or else Count = 0;
               Filled := Filled + Count;
            end;

            if Filled = Block'Last then
               Write_Blocks (Client, Address, 1, Block);
               Address := Address + 1;
               Filled := 0;
            end if;
         end loop;

         if Filled > 0 then
            Block (Filled + 1 .. Block'Last) := (others => 0);
            Write_Blocks (Client, Address, 1, Block);
            Address := Address + 1;
         end if;

         Release (Reader);
         Rose.Console_IO.Put ("restore: wrote " & Name & ": ");
         Rose.Console_IO.Put (Integer (Address));
         Rose.Console_IO.Put_Line (" blocks");
      end;
   end Write_File;

   ---------------
   -- Write_MBR --
   ---------------

   procedure Write_MBR is
      use type Rose.Capabilities.Capability;
      Path   : constant String := Boot_Directory & "boot.img";
      Client : Block_Device_Client;
      Block  : Storage_Array renames Buffer (1 .. 512);
   begin
      if MBR_Cap = Rose.Capabilities.Null_Capability
        or else not Rose.Directories.Exists (Path)
      then
         Rose.Console_IO.Put_Line ("restore: cannot write the MBR");
         return;
      end if;

      Open (Client, MBR_Cap);
      Read_Blocks (Client, 0, 1, Block);

      declare
         use Rose.Interfaces.File_Reader.Client;
         Reader : File_Reader_Client;
         Code   : Storage_Array (1 .. 512);
         Last   : Storage_Count := 0;
      begin
         Rose.Directories.Open (Reader, Path);
         while Last < Code'Last loop
            declare
               Count : Storage_Count;
            begin
               Read (Reader, Code (Last + 1 .. Code'Last), Count);
               exit when Rose.Interfaces.File_Reader.Client.Has_Error
                 or else Count = 0;
               Last := Last + Count;
            end;
         end loop;
         Release (Reader);

         if Last < MBR_Code_Bytes then
            Rose.Console_IO.Put_Line ("restore: boot.img is too short");
            return;
         end if;

         Block (1 .. MBR_Code_Bytes) := Code (1 .. MBR_Code_Bytes);
      end;

      Write_Blocks (Client, 0, 1, Block);
      Rose.Console_IO.Put_Line ("restore: wrote the MBR");
   end Write_MBR;

end Restore.Boot_Disk;
