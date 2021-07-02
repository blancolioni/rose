with Rose.Objects;

package Rose.Interfaces.Directory is

   type File_Kind is (Directory, Ordinary_File, Special_File);
   Directory_Interface : constant Rose.Objects.Endpoint_Id :=
      16#73CB_EA92_6209#;
   Directory_Entry_Count_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#653D_E0D0_57C3#;
   Directory_Entry_Name_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#6A71_8E59_E724#;
   Directory_Entry_Kind_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#85EC_1CD6_1D24#;
   Directory_Entry_Size_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#728F_CCA4_905E#;
   Find_Entry_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#0C71_18A7_C791#;
   Get_Ordinary_File_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#594B_E7C6_0A99#;
   Get_Directory_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#17C2_1154_7115#;
   Read_File_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#DCB0_BC08_9761#;
   Create_Directory_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#3583_2795_9762#;
   Create_File_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#AF04_2836_AD88#;

end Rose.Interfaces.Directory;
