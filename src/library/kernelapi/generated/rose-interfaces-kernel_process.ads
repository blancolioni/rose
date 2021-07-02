with Rose.Objects;

package Rose.Interfaces.Kernel_Process is

   Cap_Interface : constant Rose.Objects.Endpoint_Id := 16#BF0F_45C4_4F4A#;
   Kernel_Process_Interface : constant Rose.Objects.Endpoint_Id :=
      16#D2F3_1FC5_8E22#;
   Destroy_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#B057_5569_CE0C#;
   Get_Object_Id_Endpoint : constant Rose.Objects.Endpoint_Id :=
      16#955F_93DE_0601#;
   Resume_Endpoint : constant Rose.Objects.Endpoint_Id := 16#15CA_54D9_4983#;
   Fault_Endpoint : constant Rose.Objects.Endpoint_Id := 16#79D4_59CD_9D4C#;
   Notify_Endpoint : constant Rose.Objects.Endpoint_Id := 16#A81B_B880_C5A1#;

end Rose.Interfaces.Kernel_Process;
