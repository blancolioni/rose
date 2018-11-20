with Rose.Objects;
with Rose.Words;

with Init.Calls;

package body Init.Run is

   NL : constant Character := Character'Val (10);

   --------------
   -- Run_Init --
   --------------

   procedure Run_Init is
      use Rose.Capabilities, Rose.Objects, Rose.Words;
      Boot_Cap             : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (8, 1, 0, 0));
      Reserve_Cap          : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (10, 1, 0, 0));
      Console_Mem_Cap      : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap,
                                  (1, 1, 16#0000_00B8#, 16#FF00_0000#));
      Console_Cursor_Cap   : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap,
                                  (16#0000_000E#, 3, 16#03D4#, 16#03D5#));
      Create_Endpoint_Cap  : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (2, 1, 0, 0));
      Exit_Cap             : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (2, 31, 0, 0));
      Copy_Console_Cap     : Rose.Capabilities.Capability;
      Console_Id           : Rose.Objects.Object_Id;
      Mem_Id               : Rose.Objects.Object_Id;
      pragma Warnings (Off, Mem_Id);
      Console_Write_Cap    : Rose.Capabilities.Capability :=
                               Null_Capability;
      Mem_Region_Count_Cap : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (10, 2, 0, 0));
      Mem_Get_Region_Cap   : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (10, 3, 0, 0));
      Page_On_Cap          : constant Rose.Capabilities.Capability :=
                               Init.Calls.Call
                                 (Create_Cap, (7, 1, 0, 0));
      PCI_Cap              : Rose.Capabilities.Capability;

      Hd0_Parameters_Cap   : Rose.Capabilities.Capability;
      Hd0_Read_Cap         : Rose.Capabilities.Capability;
      Hd0_Write_Cap        : Rose.Capabilities.Capability;

      Hd1_Parameters_Cap   : Rose.Capabilities.Capability;
      Hd1_Read_Cap         : Rose.Capabilities.Capability;

      function Copy_Cap_From_Process
        (Copy_Cap : Rose.Capabilities.Capability;
         Endpoint : Rose.Words.Word)
         return Rose.Capabilities.Capability;

      ---------------------------
      -- Copy_Cap_From_Process --
      ---------------------------

      function Copy_Cap_From_Process
        (Copy_Cap : Rose.Capabilities.Capability;
         Endpoint : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is
         Cap : Capability :=
                 Init.Calls.Call
                   (Copy_Cap, Endpoint);
         Hex_Digits : constant String := "0123456789ABCDEF";
         Retry_Message : String :=
                           "init: failed to copy endpoint; retrying "
                           & "        " & NL;
         Fail_Message  : constant String :=
                           "init: failed to copy endpoint; giving up"
                           & NL;
         It            : Word := Endpoint;
      begin
         for I in 1 .. 8 loop
            Retry_Message (Retry_Message'Last - I) :=
              Hex_Digits (Natural (It mod 16) + 1);
            It := It / 16;
         end loop;

         for I in 1 .. 4 loop
            exit when Cap /= Null_Capability;
            Init.Calls.Send_String
              (Console_Write_Cap, Retry_Message);
            Cap :=
              Init.Calls.Call (Copy_Cap, Endpoint);
         end loop;

         if Cap = 0 then
            if Console_Write_Cap /= Null_Capability then
               Init.Calls.Send_String
                 (Console_Write_Cap, Fail_Message);
            end if;
            Init.Calls.Send (Exit_Cap);
         end if;

         return Cap;
      end Copy_Cap_From_Process;

   begin
      Init.Calls.Send (Reserve_Cap, (16#0000_1000#, 16#0009_F000#));
      Console_Id :=
        Init.Calls.Call (Boot_Cap, 2,
                         (Create_Endpoint_Cap,
                          Console_Mem_Cap,
                          Console_Cursor_Cap));
      Copy_Console_Cap :=
        Init.Calls.Call
          (Create_Cap,
           (9, 1, Word (Console_Id mod 2 ** 32), Word (Console_Id / 2 ** 32)));

      Console_Write_Cap :=
        Copy_Cap_From_Process (Copy_Console_Cap, 16#C025_0130#);

      Mem_Id :=
        Init.Calls.Call (Boot_Cap, 3,
                         (Create_Endpoint_Cap, Console_Write_Cap,
                          Mem_Region_Count_Cap, Mem_Get_Region_Cap,
                          Page_On_Cap));

      declare
         Command_Port_Out_Cap : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (16#0000_002E#, 1, 16#0CF8#, 0));
         Data_Port_Out_Cap    : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (16#0000_002E#, 1, 16#0CFC#, 0));
         Data_Port_In_Cap     : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (16#0000_002E#, 2, 16#0CFC#, 0));
         PCI_Id               : constant Rose.Objects.Object_Id :=
                                  Init.Calls.Call (Boot_Cap, 4,
                                                   (Create_Endpoint_Cap,
                                                    Console_Write_Cap,
                                                    Command_Port_Out_Cap,
                                                    Data_Port_Out_Cap,
                                                    Data_Port_In_Cap));
         Copy_PCI_Cap         : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (9, 1,
                                      Word (PCI_Id mod 2 ** 32),
                                      Word (PCI_Id / 2 ** 32)));

      begin
         PCI_Cap :=
           Copy_Cap_From_Process (Copy_PCI_Cap, 450);
      end;

      declare
         Command_0_Cap : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (16#0000_000E#, 3, 16#01F0#, 16#01F7#));
         Control_0_Cap : constant Rose.Capabilities.Capability :=
                           Init.Calls.Call
                             (Create_Cap,
                              (16#0000_000E#, 1, 16#03F6#, 16#03F6#));
         Data_0_Cap_8  : constant Rose.Capabilities.Capability :=
                           Init.Calls.Call
                             (Create_Cap,
                              (16#0000_000E#, 2, 16#01F0#, 16#01F7#));
         Data_0_Cap_Read_16 : constant Rose.Capabilities.Capability :=
                                Init.Calls.Call
                                  (Create_Cap,
                                   (16#0000_001E#, 2, 16#01F0#, 16#01F7#));
         Data_0_Cap_Write_16   : constant Rose.Capabilities.Capability :=
                                   Init.Calls.Call
                                     (Create_Cap,
                                      (16#0000_001E#, 1, 16#01F0#, 16#01F7#));
         Ata_Id               : constant Rose.Objects.Object_Id :=
                    Init.Calls.Call (Boot_Cap, 5,
                                     (Create_Endpoint_Cap,
                                      Console_Write_Cap,
                                      PCI_Cap,
                                      Command_0_Cap,
                                      Control_0_Cap,
                                      Data_0_Cap_8,
                                      Data_0_Cap_Read_16,
                                      Data_0_Cap_Write_16));
         Copy_Ata_Cap         : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Call
                                    (Create_Cap,
                                     (9, 1,
                                      Word (Ata_Id mod 2 ** 32),
                                      Word (Ata_Id / 2 ** 32)));
         Get_Interface_Cap     : constant Rose.Capabilities.Capability :=
                                   Copy_Cap_From_Process
                                     (Copy_Ata_Cap, 16#7F4D_5635#);
         Hd0                   : Init.Calls.Array_Of_Capabilities (1 .. 3);
         Hd1                   : Init.Calls.Array_Of_Capabilities (1 .. 3);
      begin
         Init.Calls.Get_Interface (Get_Interface_Cap, 0, Hd0);
         Init.Calls.Get_Interface (Get_Interface_Cap, 1, Hd1);

         Hd0_Parameters_Cap := Hd0 (1);
         Hd0_Read_Cap := Hd0 (2);
         Hd0_Write_Cap := Hd0 (3);

         Hd1_Parameters_Cap := Hd1 (1);
         Hd1_Read_Cap := Hd1 (2);

         --           Hd_Parameters_Cap :=
--             Copy_Cap_From_Process (Copy_Ata_Cap, 16#8C1F_B232#);
--           Hd_Read_Cap :=
--             Copy_Cap_From_Process (Copy_Ata_Cap, 16#82AC_0BDA#);
--           Hd_Write_Cap :=
--             Copy_Cap_From_Process (Copy_Ata_Cap, 16#D469_B96E#);
      end;

      if False then
         declare
            IsoFS_Id : constant Rose.Objects.Object_Id :=
                         Init.Calls.Call (Boot_Cap, 6,
                                          (Create_Endpoint_Cap,
                                           Console_Write_Cap,
                                           Hd1_Parameters_Cap,
                                           Hd1_Read_Cap));
         begin
            pragma Unreferenced (IsoFS_Id);
         end;
      end if;

      declare
         Restore_Id : constant Rose.Objects.Object_Id :=
                        Init.Calls.Call (Boot_Cap, 7,
                                         (Create_Endpoint_Cap,
                                          Console_Write_Cap,
                                          Hd0_Parameters_Cap,
                                          Hd0_Read_Cap,
                                          Hd0_Write_Cap,
                                          Hd1_Parameters_Cap,
                                          Hd1_Read_Cap));
      begin
         pragma Unreferenced (Restore_Id);
      end;

      Init.Calls.Send (Exit_Cap);

   end Run_Init;

end Init.Run;
