with Rose.Objects;

package Rose.Devices.Block is

   type Block_Size_Type is range 0 .. 65536;
   type Block_Address_Type is mod 2 ** 48;
   type Device_Size_Type is mod 2 ** 64;

   function To_Device_Size
     (Block_Size : Block_Size_Type;
      Block_Count : Block_Address_Type)
      return Device_Size_Type
   is (Device_Size_Type (Block_Size) * Device_Size_Type (Block_Count));

   Get_Device_Parameters_Endpoint : constant Rose.Objects.Endpoint_Id :=
                             16#8C1F_B232#;

   Read_Block_Endpoint : constant Rose.Objects.Endpoint_Id :=
                             16#82AC_0BDA#;

   Write_Block_Endpoint : constant Rose.Objects.Endpoint_Id :=
                             16#D469_B96E#;

end Rose.Devices.Block;
