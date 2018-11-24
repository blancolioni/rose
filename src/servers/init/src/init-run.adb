with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.File_System;
with Rose.Interfaces.Storage;

with Init.Calls;

package body Init.Run is

   NL : constant Character := Character'Val (10);

   Device_Driver_Priority : constant := 4;
   File_System_Priority   : constant := 10;
   Low_Priority           : constant := 12;

   Console_Module : constant := 2;
   Mem_Module     : constant := 3;
   PCI_Module     : constant := 4;
   ATA_Module     : constant := 5;
   Store_Module   : constant := 6;
   ISOFS_Module   : constant := 7;
   Restore_Module : constant := 8;
   Scan_Module    : constant := 9;

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

      Add_Storage_Cap      : Rose.Capabilities.Capability;

      Install_FS           : Rose.Capabilities.Capability;

      function Copy_Cap_From_Process
        (Copy_Cap : Rose.Capabilities.Capability;
         Endpoint : Rose.Objects.Endpoint_Id)
         return Rose.Capabilities.Capability;

      ---------------------------
      -- Copy_Cap_From_Process --
      ---------------------------

      function Copy_Cap_From_Process
        (Copy_Cap : Rose.Capabilities.Capability;
         Endpoint : Rose.Objects.Endpoint_Id)
         return Rose.Capabilities.Capability
      is

         Data : constant Init.Calls.Array_Of_Words :=
                  (Word_32 (Word_64 (Endpoint) mod 2 ** 32),
                   Word_32 (Word_64 (Endpoint) / 2 ** 32));
         Cap  : Capability :=
                  Init.Calls.Call (Copy_Cap, Data);
         Hex_Digits : constant String := "0123456789ABCDEF";
         Retry_Message : String :=
                           "init: failed to copy endpoint; retrying "
                           & "            " & NL;
         Fail_Message  : constant String :=
                           "init: failed to copy endpoint; giving up"
                           & NL;
         It            : Word_64 := Rose.Words.Word_64 (Endpoint);
      begin
         for I in 1 .. 12 loop
            Retry_Message (Retry_Message'Last - I) :=
              Hex_Digits (Natural (It mod 16) + 1);
            It := It / 16;
         end loop;

         for I in 1 .. 4 loop
            exit when Cap /= Null_Capability;
            Init.Calls.Send_String
              (Console_Write_Cap, Retry_Message);
            Cap := Init.Calls.Call (Copy_Cap, Data);
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
        Init.Calls.Launch_Boot_Module
          (Boot_Cap, Console_Module, Low_Priority,
           (Create_Endpoint_Cap,
            Console_Mem_Cap,
            Console_Cursor_Cap));
      Copy_Console_Cap :=
        Init.Calls.Call
          (Create_Cap,
           (9, 1, Word (Console_Id mod 2 ** 32), Word (Console_Id / 2 ** 32)));

      Console_Write_Cap :=
        Copy_Cap_From_Process (Copy_Console_Cap, 16#C025_0130#);

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
                                  Init.Calls.Launch_Boot_Module
                                    (Boot_Cap, PCI_Module, Low_Priority,
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
                                   Init.Calls.Launch_Boot_Module
                                     (Boot_Cap, ATA_Module,
                                      Device_Driver_Priority,
                                      (Create_Endpoint_Cap,
                                       Console_Write_Cap,
                                       PCI_Cap,
                                       Command_0_Cap,
                                       Control_0_Cap,
                                       Data_0_Cap_8,
                                       Data_0_Cap_Read_16,
                                       Data_0_Cap_Write_16));
         Copy_Ata_Cap          : constant Rose.Capabilities.Capability :=
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

      end;

      declare
         Store_Id : constant Rose.Objects.Object_Id :=
                      Init.Calls.Launch_Boot_Module
                        (Boot_Cap, Store_Module, Device_Driver_Priority,
                         (Create_Endpoint_Cap,
                          Console_Write_Cap,
                          Hd0_Parameters_Cap,
                          Hd0_Read_Cap,
                          Hd0_Write_Cap));
         Copy_Store_Cap : constant Rose.Capabilities.Capability :=
                            Init.Calls.Call
                              (Create_Cap,
                               (9, 1,
                                Word (Store_Id mod 2 ** 32),
                                Word (Store_Id / 2 ** 32)));
      begin
         Add_Storage_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage.Add_Backing_Store_Endpoint);
      end;

      declare
         Scan_Id : constant Rose.Objects.Object_Id :=
                     Init.Calls.Launch_Boot_Module
                       (Boot_Cap, Scan_Module, File_System_Priority,
                        (Create_Endpoint_Cap,
                         Console_Write_Cap,
                         Hd0_Parameters_Cap,
                         Hd0_Read_Cap,
                         Hd0_Write_Cap,
                         Add_Storage_Cap));
      begin
         pragma Unreferenced (Scan_Id);
      end;

      Mem_Id :=
        Init.Calls.Launch_Boot_Module
          (Boot_Cap, Mem_Module, Device_Driver_Priority,
           (Create_Endpoint_Cap, Console_Write_Cap,
            Mem_Region_Count_Cap, Mem_Get_Region_Cap,
            Page_On_Cap));

      declare
         IsoFS_Id : constant Rose.Objects.Object_Id :=
                      Init.Calls.Launch_Boot_Module
                        (Boot_Cap, ISOFS_Module, File_System_Priority,
                         (Create_Endpoint_Cap,
                          Console_Write_Cap,
                          Hd1_Parameters_Cap,
                          Hd1_Read_Cap));
         Copy_IsoFS_Cap : constant Rose.Capabilities.Capability :=
                            Init.Calls.Call
                              (Create_Cap,
                               (9, 1,
                                Word (IsoFS_Id mod 2 ** 32),
                                Word (IsoFS_Id / 2 ** 32)));
      begin
         Install_FS :=
           Copy_Cap_From_Process
             (Copy_IsoFS_Cap,
              Rose.Interfaces.File_System.Root_Directory_Endpoint);
      end;

      declare
         Restore_Id : constant Rose.Objects.Object_Id :=
                        Init.Calls.Launch_Boot_Module
                          (Boot_Cap, Restore_Module, File_System_Priority,
                           (Create_Endpoint_Cap,
                            Console_Write_Cap,
                            Hd0_Parameters_Cap,
                            Hd0_Read_Cap,
                            Hd0_Write_Cap,
                            Add_Storage_Cap,
                            Install_FS));
      begin
         pragma Unreferenced (Restore_Id);
      end;

      Init.Calls.Send_String
        (Console_Write_Cap, "init: exiting" & NL);
      Init.Calls.Send (Exit_Cap);

   end Run_Init;

end Init.Run;
