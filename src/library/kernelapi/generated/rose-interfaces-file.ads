with Rose.Objects;

package Rose.Interfaces.File is

   Stream_Reader_Interface : constant Rose.Objects.Endpoint_Id :=
      16#108B_3304_14EF#;
   Stream_Writer_Interface : constant Rose.Objects.Endpoint_Id :=
      16#50CB_136A_60B4#;
   Stream_Interface : constant Rose.Objects.Endpoint_Id :=
      16#06FC_CA7F_A17F#;
   Seekable_Interface : constant Rose.Objects.Endpoint_Id :=
      16#D185_E85F_7269#;
   File_Interface : constant Rose.Objects.Endpoint_Id := 16#308E_297C_64BB#;
   Read_Endpoint : constant Rose.Objects.Endpoint_Id := 16#50DD_8588_27DD#;
   Write_Endpoint : constant Rose.Objects.Endpoint_Id := 16#2844_384F_C1C5#;
   Seek_Endpoint : constant Rose.Objects.Endpoint_Id := 16#6836_7A39_E98C#;

end Rose.Interfaces.File;
