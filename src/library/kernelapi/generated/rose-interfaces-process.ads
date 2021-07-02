with Rose.Objects;

package Rose.Interfaces.Process is

   Segment_Interface : constant Rose.Objects.Endpoint_Id :=
      16#25CE_5F3A_F35C#;
   Server_Interface : constant Rose.Objects.Endpoint_Id :=
      16#682A_60F1_9C52#;
   Cap_Interface : constant Rose.Objects.Endpoint_Id := 16#BF0F_45C4_4F4A#;
   Process_Interface : constant Rose.Objects.Endpoint_Id :=
      16#5E43_3170_8583#;
   Add_Segment_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#FFBB_6E9C_85B9#;
   Add_Nonpersistent_Segment_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#7C63_63AC_C85C#;
   Published_Interface_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#8365_663A_FE52#;
   Destroy_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#B057_5569_CE0C#;
   Get_Object_Id_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#955F_93DE_0601#;
   Heap_Interface_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#4E60_08BB_1C02#;
   Exit_Process_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#53B7_D1CB_4ADB#;
   Publish_Interface_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#B34F_9AFA_D2C1#;

end Rose.Interfaces.Process;
