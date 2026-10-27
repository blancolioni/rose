package Rose.Devices.Partitions is

   --  Partition type GUIDs, as the two words of a GPT partition entry:
   --  the first three fields of a GUID are stored little-endian, so
   --  21686148-6449-6E6F-744E-656564454649 is Low 16#6E6F_6449_2168_6148#
   --  and High 16#4946_4564_6565_4E74#.

   Swap_Id_Low  : constant :=
                    16#9A63_FE8C_C82C_3C47#;
   Swap_Id_High : constant :=
                    16#DD8F_C8B6_D82D_11E8#;

   Log_Id_Low  : constant :=
                    16#E4BA_963D_64FD_CF54#;
   Log_Id_High : constant :=
                    16#4091_DD32_8E25_7A2F#;

   BIOS_Boot_Id_Low  : constant :=
                         16#6E6F_6449_2168_6148#;
   BIOS_Boot_Id_High : constant :=
                         16#4946_4564_6565_4E74#;
   --  21686148-6449-6E6F-744E-656564454649: GRUB's core.img (#322)

   EFI_System_Id_Low  : constant :=
                          16#11D2_F81F_C12A_7328#;
   EFI_System_Id_High : constant :=
                          16#3BC9_3EC9_A000_4BBA#;
   --  C12A7328-F81F-11D2-BA4B-00A0C93EC93B: the boot partition, which
   --  holds GRUB's files, the kernel and the boot modules (#322)

end Rose.Devices.Partitions;
