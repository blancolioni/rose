with Rose.Words;
with Rose.Objects;

package Rose.Interfaces.Block_Device is

   Max_Block_Size : constant  := 4096;
   type Block_Size_Type is range  0 ..  4096;
   type Block_Address_Type is new Rose.Words.Word_64;
   type Device_Size_Type is new Rose.Words.Word_64;
   Block_Device_Interface : constant Rose.Objects.Endpoint_Id :=
      16#0241_4E1D_921C#;
   Get_Parameters_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#964A_D0FE_10CD#;
   Read_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#565F_A8C8_AA82#;
   Write_Blocks_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#8314_5AC2_2803#;

end Rose.Interfaces.Block_Device;
