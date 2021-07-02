with Rose.Objects;

package Rose.Interfaces.Terminal is

   Stream_Reader_Interface : constant Rose.Objects.Endpoint_Id :=
      16#108B_3304_14EF#;
   Stream_Writer_Interface : constant Rose.Objects.Endpoint_Id :=
      16#50CB_136A_60B4#;
   Stream_Interface : constant Rose.Objects.Endpoint_Id :=
      16#06FC_CA7F_A17F#;
   Terminal_Interface : constant Rose.Objects.Endpoint_Id :=
      16#8AEA_ADEE_54F7#;
   Read_Endpoint : constant Rose.Objects.Endpoint_Id := 16#50DD_8588_27DD#;
   Write_Endpoint : constant Rose.Objects.Endpoint_Id := 16#2844_384F_C1C5#;

end Rose.Interfaces.Terminal;
