with Rose.Objects;
with Rose.Words;
with Rose.Interfaces.Storage.Block_Array;

package Rose.Devices.Block is

   Max_Block_Size : constant := 4096;
   --  The largest block that this library handles, and the size of its
   --  one-block buffer. Block_Array allows blocks of up to 64 KiB (ADR
   --  0034), but Rose's own block arrays serve 512 or 2048 bytes.

   subtype Block_Size_Type is
     Rose.Interfaces.Storage.Block_Array.Block_Size_Type;

   subtype Block_Address_Type is
     Rose.Interfaces.Storage.Block_Array.Block_Address_Type;

   type Device_Size_Type is new Rose.Words.Word_64;
   --  A device's size in bytes. Block_Array has no such type (ADR 0034).

   function To_Device_Size
     (Block_Size : Block_Size_Type;
      Block_Count : Block_Address_Type)
      return Device_Size_Type
   is (Device_Size_Type (Block_Size) * Device_Size_Type (Block_Count));

   Get_Device_Parameters_Endpoint : constant Rose.Objects.Endpoint_Id :=
                                      Rose.Interfaces.Storage.Block_Array
                                        .Get_Parameters_Endpoint;

   Read_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
                            Rose.Interfaces.Storage.Block_Array
                              .Read_Blocks_Endpoint;

   Write_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
                             Rose.Interfaces.Storage.Block_Array
                               .Write_Blocks_Endpoint;

end Rose.Devices.Block;
