with System.Storage_Elements;

with Rose.Words;

with Rose.Console_IO;

package body IsoFS.Directories is

   use Rose.Words;

   subtype ISO_Sector is
     System.Storage_Elements.Storage_Array
       (1 .. ISO_Sector_Size);

   type Directory_Date_Time is array (1 .. 7) of Word_8;

   type Directory_Entry is
      record
         Length                           : Word_8;
         Extended_Attribute_Record_Length : Word_8;
         Extent_Location_LSB              : Word_32;
         Extent_Location_MSB              : Word_32;
         Extent_Size_LSB                  : Word_32;
         Extent_Size_MSB                  : Word_32;
         Recording_Date_Time              : Directory_Date_Time;
         File_Flags                       : Word_8;
         Interleaved_File_Unit_Size       : Word_8;
         Interleaved_Gap_Size             : Word_8;
         Volume_Sequence_LSB              : Word_16;
         Volume_Sequence_MSB              : Word_16;
         File_Identifier_Length           : Word_8;
      end record
   with Pack, Size => 33 * 8;

   type Primary_Volume_Sector is
      record
         Sector_Type                   : Word_8;
         Standard_Identifier           : String (1 .. 5);
         Version                       : Word_8;
         Unused_1                      : Word_8;
         System_Identifier             : String (1 .. 32);
         Volume_Identifier             : String (1 .. 32);
         Unused_2                      : Word_64;
         Volume_Space_Size_LSB         : Word_32;
         Volume_Space_Size_MSB         : Word_32;
         Unused_3                      : String (1 .. 32);
         Volume_Set_Size_LSB           : Word_16;
         Volume_Set_Size_MSB           : Word_16;
         Volume_Seq_Nr_LSB             : Word_16;
         Volume_Seq_Nr_MSB             : Word_16;
         Logical_Block_Size_LSB        : Word_16;
         Logical_Block_Size_MSB        : Word_16;
         Path_Table_Size_LSB           : Word_32;
         Path_Table_Size_MSB           : Word_32;
         L_Path_Table_Address          : Word_32;
         Optional_L_Path_Table_Address : Word_32;
         M_Path_Table_Address          : Word_32;
         Optional_M_Path_Table_Address : Word_32;
         Root_Directory_Entry          : Directory_Entry;
         Empty_Root_Directory_Name     : Word_8;
         Volume_Set_Identifier         : String (1 .. 128);
         Publisher_Identifier          : String (1 .. 128);
         Data_Preparer_Identifier      : String (1 .. 128);
         Application_Identifier        : String (1 .. 128);
         Copyright_File_Identifier     : String (1 .. 38);
         Abstract_File_Identifier      : String (1 .. 36);
         Bibliographic_File_Identifier : String (1 .. 37);
         Create_Date_Time              : String (1 .. 17);
         Modification_Date_Time        : String (1 .. 17);
         Expiration_Date_Time          : String (1 .. 17);
         Effective_Date_Time           : String (1 .. 17);
         File_Structure_Version        : Word_8;
         Unused_4                      : Word_8;
         Available                     : String (1 .. 512);
         Reserved                      : String (1 .. 653);
      end record
     with Pack, Size => 2048 * 8;

   -------------------------
   -- Read_Root_Directory --
   -------------------------

   procedure Read_Root_Directory
     (Device : Rose.Devices.Block.Client.Block_Device_Type)
   is
      use System.Storage_Elements;
      use Rose.Devices.Block;
      use Rose.Devices.Block.Client;

      Volume_Index : Block_Address_Type := ISO_First_Volume_Sector;
      Sector_Count : constant Block_Address_Type :=
                       Get_Block_Count (Device);
      Found        : Boolean := False;
      Buffer       : ISO_Sector;
   begin
      while Volume_Index < Sector_Count
        and then not Found
      loop

         Read_Block (Device, Volume_Index, Buffer);

         if Buffer (Descriptor_Type_Offset + 1)
           = Primary_Volume_Descriptor
         then
            Found := True;
         else
            Volume_Index := Volume_Index + 1;
         end if;
      end loop;

      if not Found then
         Rose.Console_IO.Put_Line
           ("isofs: cannot find primary volume descriptor");
         return;
      end if;

      declare
         use Rose.Console_IO;
         Volume : Primary_Volume_Sector;
         pragma Import (Ada, Volume);
         for Volume'Address use Buffer'Address;
      begin
         Rose.Console_IO.Put
           ("found volume: ");
         Rose.Console_IO.Put
           (Volume.Volume_Identifier);
         Rose.Console_IO.New_Line;
         Put ("root directory timestamp: ");
         declare
            D : Directory_Date_Time renames
                  Volume.Root_Directory_Entry.Recording_Date_Time;
         begin
            Put (Natural (D (1)) + 1900);
            Put ("-");
            Put (Natural (D (2)), 2, '0');
            Put ("-");
            Put (Natural (D (3)), 2, '0');
            Put (" ");
            Put (Natural (D (4)), 2, '0');
            Put (":");
            Put (Natural (D (5)), 2, '0');
            Put (":");
            Put (Natural (D (6)), 2, '0');
            New_Line;
         end;
      end;

   end Read_Root_Directory;

end IsoFS.Directories;