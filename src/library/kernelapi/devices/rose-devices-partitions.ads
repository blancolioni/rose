package Rose.Devices.Partitions is

   Swap_Id_Low  : constant :=
                    16#9A63_FE8C_C82C_3C47#;
   Swap_Id_High : constant :=
                    16#DD8F_C8B6_D82D_11E8#;

   Log_Id_Low  : constant :=
                    16#E4BA_963D_64FD_CF54#;
   Log_Id_High : constant :=
                    16#4091_DD32_8E25_7A2F#;

   Active_Swap_Flag : constant := 2 ** 22;

end Rose.Devices.Partitions;
