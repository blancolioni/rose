with Rose.Objects;
with Rose.Interfaces.Block_Device;

package Rose.Devices.Block is

   Max_Block_Size : constant := Rose.Interfaces.Block_Device.Max_Block_Size;

   subtype Block_Size_Type is
     Rose.Interfaces.Block_Device.Block_Size_Type;

   subtype Block_Address_Type is
     Rose.Interfaces.Block_Device.Block_Address_Type;

   subtype Device_Size_Type is
     Rose.Interfaces.Block_Device.Device_Size_Type;

   function To_Device_Size
     (Block_Size : Block_Size_Type;
      Block_Count : Block_Address_Type)
      return Device_Size_Type
   is (Rose.Interfaces.Block_Device."*"
       (Device_Size_Type (Block_Size), Device_Size_Type (Block_Count)));

   Get_Device_Parameters_Endpoint : constant Rose.Objects.Endpoint_Id :=
                                      Rose.Interfaces.Block_Device
                                        .Get_Parameters_Endpoint;

   Read_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
                            Rose.Interfaces.Block_Device
                              .Read_Blocks_Endpoint;

   Write_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
                             Rose.Interfaces.Block_Device
                               .Write_Blocks_Endpoint;

end Rose.Devices.Block;
