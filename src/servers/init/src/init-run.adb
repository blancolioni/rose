with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Init_Capabilities;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Devices.Partitions;

with Rose.Interfaces.Block_Device;
with Rose.Interfaces.Constructor;
with Rose.Interfaces.Exec;
with Rose.Interfaces.File_System;
with Rose.Interfaces.Kernel_Log;
with Rose.Interfaces.Log_Reader;
with Rose.Interfaces.Output_Redirect;
with Rose.Interfaces.Storage;
with Rose.Interfaces.Storage_Checkpoint;
with Rose.Interfaces.Stream_Writer;
with Rose.Interfaces.Timer;
with Rose.Interfaces.Timeout;

with Rose.Interfaces.Partitions.Client;

with Rose.Interfaces.Executable;
with Rose.Interfaces.Memory;
with Rose.Interfaces.Memory_Restore;
with Rose.Interfaces.State;

with Rose.Invocation;
with Rose.System_Calls;

with Init.Calls;
with Init.Grants;
with Init.Installer;
with Init.Users;

package body Init.Run is

   NL : constant Character := Character'Val (10);

   Memory_Priority        : constant := 2;
   Log_Priority           : constant := 13;
   Checkpoint_Priority    : constant := 14;
   Device_Driver_Priority : constant := 5;
   File_System_Priority   : constant := 10;
   Low_Priority           : constant := 12;

   --  The boot modules' numbers, which are their positions in the GRUB
   --  configuration that loads them.  Module 1 is init.

   type Module_Table is
      record
         Console, Mem, PCI, ATA, Store, ISOFS, Restore, Scan,
         Partition, Elf, Timer, Cap_Set, Checkpoint, Log : Rose.Words.Word;
      end record;

   Install_Modules : constant Module_Table :=
                       (Console    => 2,
                        Mem        => 3,
                        PCI        => 4,
                        ATA        => 5,
                        Store      => 6,
                        ISOFS      => 7,
                        Restore    => 8,
                        Scan       => 9,
                        Partition  => 10,
                        Elf        => 11,
                        Timer      => 12,
                        Cap_Set    => 13,
                        Checkpoint => 14,
                        Log        => 15);
   --  The install media's images/iso/boot/grub/grub.cfg, which loads
   --  init-install and restore-install.

   Disk_Modules    : constant Module_Table :=
                       (Console    => 2,
                        Mem        => 3,
                        PCI        => 4,
                        ATA        => 5,
                        Store      => 6,
                        ISOFS      => 0,
                        Restore    => 7,
                        Scan       => 8,
                        Partition  => 9,
                        Elf        => 10,
                        Timer      => 11,
                        Cap_Set    => 12,
                        Checkpoint => 13,
                        Log        => 14);
   --  The boot disk's configuration (#322), which loads init and
   --  restore, and not isofs.

   --  The object ids of the boot servers.  A server has the same id on
   --  every boot, whichever servers are launched, so that the caps that
   --  restored processes hold still name it (#372).  The kernel gives
   --  idle and init ids 1 and 2.

   Console_Oid        : constant := 3;
   Mem_Oid            : constant := 4;
   PCI_Oid            : constant := 5;
   ATA_Oid            : constant := 6;
   Store_Oid          : constant := 7;
   ISOFS_Oid          : constant := 8;
   Restore_Oid        : constant := 9;
   Scan_Oid           : constant := 10;
   Swap_Partition_Oid : constant := 11;
   Elf_Oid            : constant := 12;
   Timer_Oid          : constant := 13;
   Cap_Set_Oid        : constant := 14;
   Checkpoint_Oid     : constant := 15;
   Log_Oid            : constant := 16;
   Log_Partition_Oid  : constant := 17;

   --  The install media's partition servers for writing the boot loader
   --  (#322): block 0, which holds the MBR, and the BIOS boot and boot
   --  partitions.

   MBR_Oid            : constant := 18;
   BIOS_Boot_Oid      : constant := 19;
   Boot_Partition_Oid : constant := 20;

   Is_Driver_Flag     : constant := 32;
   --  Set in the flags of an install message for a program from the
   --  drivers directory (Restore.Installer.Is_Driver).

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;

   --------------
   -- Run_Init --
   --------------

   procedure Run_Init (Install : Boolean) is
      use Rose.Capabilities, Rose.Objects, Rose.Words;
      package Init_Caps renames Rose.Init_Capabilities;

      Modules : constant Module_Table :=
                  (if Install then Install_Modules else Disk_Modules);

      --  The caps the kernel gives init

      Boot_Cap             : constant Capability := Init_Caps.Boot_Cap;
      Reserve_Cap          : constant Capability :=
                               Init_Caps.Reserve_Memory_Cap;
      Create_Endpoint_Cap  : constant Capability :=
                               Init_Caps.Create_Endpoint_Cap;
      Delete_Cap           : constant Capability := Init_Caps.Delete_Cap;
      Rescind_Cap          : constant Capability := Init_Caps.Rescind_Cap;
      Exit_Cap             : constant Capability := Init_Caps.Exit_Cap;
      Mem_Region_Count_Cap : constant Capability :=
                               Init_Caps.Region_Count_Cap;
      Mem_Get_Region_Cap   : constant Capability :=
                               Init_Caps.Get_Region_Cap;
      Page_On_Cap          : constant Capability := Init_Caps.Page_On_Cap;
      Start_Log_Cap        : constant Capability := Init_Caps.Start_Log_Cap;
      Trace_Cap            : constant Capability := Init_Caps.Trace_Cap;
      --  For trace events (Rose.Trace), given to the processes that
      --  launch commands.

      --  The boot console's screen and cursor.  They aren't recorded as
      --  granted (Init.Grants), because the persistent console takes the
      --  screen over from the boot console (#304).

      Console_Mem_Cap      : constant Capability :=
                               Init.Grants.Device_Page
                                 (Init.Grants.No_Owner, 16#B8#);
      Console_Cursor_Cap   : constant Capability :=
                               Init.Grants.Ports
                                 (Init.Grants.No_Owner,
                                  Init_Caps.Port_Out_Range_Endpoint,
                                  Init_Caps.Port_Size_8,
                                  16#03D4#, 16#03D5#);
      Copy_Console_Cap      : Rose.Capabilities.Capability;
      Console_Interface_Cap : Rose.Capabilities.Capability :=
                                Null_Capability;
      Console_Stream_Cap    : Rose.Capabilities.Capability :=
                                Null_Capability;
      Console_Redirect_Cap  : Rose.Capabilities.Capability :=
                                Null_Capability;
      Keyboard_Interface_Cap : Rose.Capabilities.Capability :=
                                 Null_Capability
                                   with Unreferenced;
      Console_Input_Stream_Cap : Rose.Capabilities.Capability :=
                                   Null_Capability;
      Mem_Cap                  : Rose.Capabilities.Capability;
      Memory_Checkpoint_Cap    : Rose.Capabilities.Capability;
      Timer_Cap                : Rose.Capabilities.Capability :=
                                   Null_Capability;
      PCI_Cap                  : Rose.Capabilities.Capability;

      Cap_Set_Cap          : Rose.Capabilities.Capability;

      Hd0_Cap              : Rose.Capabilities.Capability;
      Hd1_Cap              : Rose.Capabilities.Capability :=
                               Null_Capability;

      Storage_Cap          : Rose.Capabilities.Capability;
      Add_Storage_Cap      : Rose.Capabilities.Capability;
      Reserve_Storage_Cap  : Rose.Capabilities.Capability;
      Store_Checkpoint_Cap : Rose.Capabilities.Capability;
      --  For the checkpoint server, to add the region table to each
      --  checkpoint.
      Store_Restore_Cap    : Rose.Capabilities.Capability;
      Memory_Restore_Cap   : Rose.Capabilities.Capability;
      Log_Reset_Cap        : Rose.Capabilities.Capability;
      --  For the restore server, to recreate the regions, the memory
      --  server's processes, and to start a new log on a fresh install.
      Restored             : Boolean := False;
      --  Set if the restore server restored a checkpoint, in which case
      --  the programs it would have installed are running already.
      Restore_Reply_Cap    : Rose.Capabilities.Capability := 0;
      --  The reply cap of the restore server's last message.

      Launch_Elf_Cap       : Rose.Capabilities.Capability;
      Launch_Command_Elf_Cap : Rose.Capabilities.Capability;
      Install_Media_FS     : Rose.Capabilities.Capability :=
                               Null_Capability;
      Install_Receiver     : Rose.Capabilities.Capability;
      Install_Endpoint     : Rose.Capabilities.Capability;

      Swap_Cap          : Rose.Capabilities.Capability := 0;
      Log_Cap           : Rose.Capabilities.Capability := 0;

      MBR_Cap            : Rose.Capabilities.Capability := 0;
      BIOS_Boot_Cap      : Rose.Capabilities.Capability := 0;
      Boot_Partition_Cap : Rose.Capabilities.Capability := 0;
      --  Block devices for block 0 and the two boot partitions, which
      --  restore-install writes the boot loader to (#322)

      Add_Log_Device_Cap : Rose.Capabilities.Capability;
      Append_To_Log_Cap  : Rose.Capabilities.Capability;
      Log_Reader_Cap     : Rose.Capabilities.Capability;
      Receive_Timeout    : Rose.Capabilities.Capability;
      Send_Timeout       : Rose.Capabilities.Capability;

      Find_Cap    : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;

      First_User : constant String := "fraser";
      User_Cap   : Rose.Capabilities.Capability;
      Home_Cap   : Rose.Capabilities.Capability;
      --  The first user, and its home directory, created on first boot.

      function Copy_Cap_From_Process
        (Copy_Cap   : Rose.Capabilities.Capability;
         Endpoint   : Rose.Objects.Endpoint_Id;
         Identifier : Rose.Objects.Capability_Identifier := 0)
         return Rose.Capabilities.Capability;

      function Get_Public_Interface_From_Process
        (Process_Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability;

      function Launch_Partition_Server
        (Oid        : Rose.Objects.Object_Id;
         Device_Cap : Rose.Capabilities.Capability;
         Start      : Rose.Words.Word_64;
         Bound      : Rose.Words.Word_64;
         Block_Size : Rose.Words.Word)
         return Rose.Capabilities.Capability;
      --  Launch a partition server for the device's blocks Start ..
      --  Bound - 1, in 512-byte blocks, and return its block device
      --  interface, which has blocks of Block_Size bytes.

      procedure Load_Partition
        (Client            : Rose.Interfaces.Partitions
         .Client.Partitions_Client;
         Device_Cap        : Rose.Capabilities.Capability;
         Index             : Positive);

      function Is_Swap_Partition (Low, High : Word_64) return Boolean
      is (Low = Rose.Devices.Partitions.Swap_Id_Low
          and then High = Rose.Devices.Partitions.Swap_Id_High);

      function Is_Log_Partition (Low, High : Word_64) return Boolean
      is (Low = Rose.Devices.Partitions.Log_Id_Low
          and then High = Rose.Devices.Partitions.Log_Id_High);

      procedure Wait (Milliseconds : Natural);

      ---------------------------
      -- Copy_Cap_From_Process --
      ---------------------------

      function Copy_Cap_From_Process
        (Copy_Cap   : Rose.Capabilities.Capability;
         Endpoint   : Rose.Objects.Endpoint_Id;
         Identifier : Rose.Objects.Capability_Identifier := 0)
         return Rose.Capabilities.Capability
      is
         Data : constant Init.Calls.Array_Of_Words :=
                  (Word_32 (Word_64 (Endpoint) mod 2 ** 32),
                   Word_32 (Word_64 (Endpoint) / 2 ** 32),
                   Word_32 (Identifier));
         Cap  : Capability :=
                  Init.Calls.Call (Copy_Cap, Data);
         Fail_Message  : constant String :=
                           "init: failed to copy endpoint; giving up"
                           & NL;
      begin

         if Timer_Cap = Null_Capability then
            for I in 1 .. 16 loop
               exit when Cap /= Null_Capability;
               Cap := Init.Calls.Call (Copy_Cap, Data);
            end loop;
         else
            for I in 1 .. 4 loop
               exit when Cap /= Null_Capability;
               Wait (500);
               Cap := Init.Calls.Call (Copy_Cap, Data);
            end loop;
         end if;

         if Cap = Null_Capability then
            if Console_Interface_Cap /= Null_Capability then
               Init.Calls.Send_String
                 (Console_Interface_Cap, Fail_Message);
            end if;
            Init.Calls.Send (Exit_Cap);
         end if;

         return Cap;
      end Copy_Cap_From_Process;

      ---------------------------------------
      -- Get_Public_Interface_From_Process --
      ---------------------------------------

      function Get_Public_Interface_From_Process
        (Process_Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is
         Caps          : Init.Calls.Array_Of_Capabilities (1 .. 7);
         Public_Cap    : Rose.Capabilities.Capability;
         Interface_Cap : Rose.Capabilities.Capability := Null_Capability;
      begin
         --  get process interface caps
         Init.Calls.Get_Interface (Process_Cap, Caps);

         --  public interface cap is in Caps (3)
         Public_Cap := Caps (3);

         if Public_Cap /= Null_Capability then
            for I in 1 .. 4 loop
               Init.Calls.Get_Interface (Public_Cap, Caps);
               Interface_Cap := Caps (1);
               exit when Interface_Cap /= Null_Capability;
               Wait (500);
            end loop;
         end if;

         return Interface_Cap;
      end Get_Public_Interface_From_Process;

      -----------------------------
      -- Launch_Partition_Server --
      -----------------------------

      function Launch_Partition_Server
        (Oid        : Rose.Objects.Object_Id;
         Device_Cap : Rose.Capabilities.Capability;
         Start      : Rose.Words.Word_64;
         Bound      : Rose.Words.Word_64;
         Block_Size : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is
         Copy_Id_Cap : constant Rose.Capabilities.Capability :=
                         Init.Calls.Launch_Boot_Module
                           (Boot_Cap, Modules.Partition, Oid,
                            Device_Driver_Priority,
                            Create_Endpoint_Cap, Cap_Set_Cap,
                            (Console_Interface_Cap,
                             Device_Cap),
                            (Rose.Words.Word (Start),
                             Rose.Words.Word (Bound),
                             Block_Size));
      begin
         return Copy_Cap_From_Process
           (Copy_Id_Cap,
            Rose.Interfaces.Block_Device.Block_Device_Interface);
      end Launch_Partition_Server;

      --------------------
      -- Load_Partition --
      --------------------

      procedure Load_Partition
        (Client            : Rose.Interfaces.Partitions
         .Client.Partitions_Client;
         Device_Cap        : Rose.Capabilities.Capability;
         Index             : Positive)
      is
         use Rose.Devices.Partitions;
         Partition_Type_Low  : Rose.Words.Word_64;
         Partition_Type_High : Rose.Words.Word_64;
         Partition_Flags     : Rose.Words.Word_64;
         Start_Address       : Rose.Words.Word_64;
         Length              : Rose.Words.Word_64;

         function Is_Type (Low, High : Rose.Words.Word_64) return Boolean
         is (Partition_Type_Low = Low and then Partition_Type_High = High);

         function Launch
           (Oid        : Rose.Objects.Object_Id;
            Block_Size : Rose.Words.Word)
            return Rose.Capabilities.Capability
         is (Launch_Partition_Server
               (Oid, Device_Cap, Start_Address, Start_Address + Length,
                Block_Size));

      begin
         Rose.Interfaces.Partitions.Client.Get_Partition
           (Client, Index, Partition_Type_Low,
            Partition_Type_High, Partition_Flags,
            Start_Address, Length);

         --  Only the first partition of each type is used.  The install
         --  media's init also uses GRUB's, to write the boot loader
         --  (#322).

         if Is_Swap_Partition (Partition_Type_Low, Partition_Type_High) then
            if Swap_Cap = 0 then
               Swap_Cap := Launch (Swap_Partition_Oid, 4096);
            end if;
         elsif Is_Log_Partition (Partition_Type_Low, Partition_Type_High) then
            if Log_Cap = 0 then
               Log_Cap := Launch (Log_Partition_Oid, 512);
            end if;
         elsif not Install then
            null;
         elsif Is_Type (BIOS_Boot_Id_Low, BIOS_Boot_Id_High) then
            if BIOS_Boot_Cap = 0 then
               BIOS_Boot_Cap := Launch (BIOS_Boot_Oid, 512);
            end if;
         elsif Is_Type (EFI_System_Id_Low, EFI_System_Id_High) then
            if Boot_Partition_Cap = 0 then
               Boot_Partition_Cap := Launch (Boot_Partition_Oid, 4096);
            end if;
         end if;
      end Load_Partition;

      ----------
      -- Wait --
      ----------

      procedure Wait (Milliseconds : Natural) is
         Control_Cap : constant Rose.Capabilities.Capability :=
                         Init.Calls.Call
                           (Timer_Cap, Send_Timeout,
                            (1 => Rose.Words.Word (Milliseconds)));
      begin
         pragma Unreferenced (Control_Cap);
         Init.Calls.Receive (Receive_Timeout);
      end Wait;

   begin

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Init.Calls.Send (Reserve_Cap, (16#0000_1000#, 16#0009_F000#));
      Copy_Console_Cap :=
        Init.Calls.Launch_Boot_Module
          (Boot_Cap, Modules.Console, Console_Oid,
           Low_Priority,
           Create_Endpoint_Cap,
           Rose.Capabilities.Null_Capability,
           (Console_Mem_Cap,
            Console_Cursor_Cap));
      Console_Interface_Cap :=
        Copy_Cap_From_Process
          (Copy_Console_Cap,
           Rose.Interfaces.Stream_Writer.Stream_Writer_Interface);

      --  Where Init.Calls.Send_String reports, as when the installer
      --  refuses a cap request.

      Init.Calls.Set_Console (Console_Interface_Cap);

      Console_Stream_Cap :=
        Copy_Cap_From_Process
          (Copy_Console_Cap,
           Rose.Interfaces.Stream_Writer.Write_Endpoint);

      Console_Redirect_Cap :=
        Copy_Cap_From_Process
          (Copy_Console_Cap,
           Rose.Interfaces.Output_Redirect.Redirect_Endpoint);

      declare
         Copy_Cap_Set_Cap : constant Rose.Capabilities.Capability :=
                        Init.Calls.Launch_Boot_Module
                          (Boot_Cap, Modules.Cap_Set, Cap_Set_Oid,
                           Device_Driver_Priority,
                           Create_Endpoint_Cap,
                           Rose.Capabilities.Null_Capability,
                           (1 => Console_Interface_Cap));
      begin
         Cap_Set_Cap :=
           Copy_Cap_From_Process
             (Copy_Cap_Set_Cap,
              Rose.Interfaces.Constructor.Create_Endpoint);
      end;

      declare
         Copy_Mem_Cap : constant Rose.Capabilities.Capability :=
                    Init.Calls.Launch_Boot_Module
                      (Boot_Cap, Modules.Mem, Mem_Oid,
                       Memory_Priority,
                       Create_Endpoint_Cap,
                       Cap_Set_Cap,
                       (Console_Interface_Cap,
                        Mem_Region_Count_Cap, Mem_Get_Region_Cap,
                        Page_On_Cap, Delete_Cap, Trace_Cap));
      begin
         Mem_Cap :=
           Copy_Cap_From_Process
             (Copy_Mem_Cap, Rose.Interfaces.Memory.Memory_Interface);
         Memory_Checkpoint_Cap :=
           Copy_Cap_From_Process
             (Copy_Mem_Cap, Rose.Interfaces.Memory.Checkpoint_Endpoint);
         Memory_Restore_Cap :=
           Copy_Cap_From_Process
             (Copy_Mem_Cap,
              Rose.Interfaces.Memory_Restore.Memory_Restore_Interface);
      end;

      declare
         Set_Timeout_Cap : constant Rose.Capabilities.Capability :=
                             Init_Caps.Set_Timeout_Cap;
         Get_Ticks_Cap   : constant Rose.Capabilities.Capability :=
                             Init_Caps.Get_Ticks_Cap;
         Copy_Timeout_Cap : constant Rose.Capabilities.Capability :=
                            Init.Calls.Launch_Boot_Module
                               (Boot_Cap, Modules.Timer, Timer_Oid,
                                Device_Driver_Priority,
                                Create_Endpoint_Cap, Cap_Set_Cap,
                               (Console_Interface_Cap,
                                Set_Timeout_Cap, Get_Ticks_Cap));
         Timer_Caps       : Init.Calls.Array_Of_Capabilities (1 .. 2);
         Control_Cap      : Rose.Capabilities.Capability with Unreferenced;

      begin
         Timer_Cap :=
           Copy_Cap_From_Process
             (Copy_Timeout_Cap, Rose.Interfaces.Timer.Set_Timer_Endpoint);

         Init.Calls.Call
           (Cap         => Create_Endpoint_Cap,
            Data        =>
              (Rose.Words.Word_32
                   (Rose.Interfaces.Timeout.On_Timeout_Endpoint
                    mod 2 ** 32),
               Rose.Words.Word_32
                 (Rose.Interfaces.Timeout.On_Timeout_Endpoint / 2 ** 32)),
            Result_Caps => Timer_Caps);

         Receive_Timeout := Timer_Caps (1);
         Send_Timeout := Timer_Caps (2);

      end;

      declare
         Command_Port_Out_Cap : constant Rose.Capabilities.Capability :=
                                  Init.Grants.Ports
                                    (PCI_Oid, Init_Caps.Port_Out_Endpoint,
                                     Init_Caps.Port_Size_32,
                                     16#0CF8#, 16#0CF8#);
         Data_Port_Out_Cap    : constant Rose.Capabilities.Capability :=
                                  Init.Grants.Ports
                                    (PCI_Oid, Init_Caps.Port_Out_Endpoint,
                                     Init_Caps.Port_Size_32,
                                     16#0CFC#, 16#0CFC#);
         Data_Port_In_Cap     : constant Rose.Capabilities.Capability :=
                                  Init.Grants.Ports
                                    (PCI_Oid, Init_Caps.Port_In_Endpoint,
                                     Init_Caps.Port_Size_32,
                                     16#0CFC#, 16#0CFC#);
         Copy_PCI_Cap         : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Launch_Boot_Module
                                    (Boot_Cap, Modules.PCI, PCI_Oid,
                                     Low_Priority,
                                     Create_Endpoint_Cap, Cap_Set_Cap,
                                     (Console_Interface_Cap,
                                      Command_Port_Out_Cap,
                                      Data_Port_Out_Cap,
                                      Data_Port_In_Cap));

      begin
         PCI_Cap :=
           Copy_Cap_From_Process (Copy_PCI_Cap, 450);
      end;

      declare
         Command_0_Cap : constant Rose.Capabilities.Capability :=
                           Init.Grants.Ports
                             (ATA_Oid, Init_Caps.Port_Out_Range_Endpoint,
                              Init_Caps.Port_Size_8, 16#01F0#, 16#01F7#);
         Control_0_Cap : constant Rose.Capabilities.Capability :=
                           Init.Grants.Ports
                             (ATA_Oid, Init_Caps.Port_Out_Endpoint,
                              Init_Caps.Port_Size_8, 16#03F6#, 16#03F6#);
         Data_0_Cap_8  : constant Rose.Capabilities.Capability :=
                           Init.Grants.Ports
                             (ATA_Oid, Init_Caps.Port_In_Endpoint,
                              Init_Caps.Port_Size_8, 16#01F0#, 16#01F7#);
         Data_0_Cap_Read_16 : constant Rose.Capabilities.Capability :=
                                Init.Grants.Ports
                                  (ATA_Oid, Init_Caps.Port_In_Endpoint,
                                   Init_Caps.Port_Size_16, 16#01F0#, 16#01F7#);
         Data_0_Cap_Write_16   : constant Rose.Capabilities.Capability :=
                                   Init.Grants.Ports
                                     (ATA_Oid, Init_Caps.Port_Out_Endpoint,
                                      Init_Caps.Port_Size_16,
                                      16#01F0#, 16#01F7#);
         Reserve_Primary_IRQ   : constant Rose.Capabilities.Capability :=
                                   Init.Grants.Interrupt (ATA_Oid, 46);
         Reserve_Secondary_IRQ : constant Rose.Capabilities.Capability :=
                                   Init.Grants.Interrupt (ATA_Oid, 47);
         Copy_Ata_Cap          : constant Rose.Capabilities.Capability :=
                                   Init.Calls.Launch_Boot_Module
                                     (Boot_Cap, Modules.ATA, ATA_Oid,
                                      Device_Driver_Priority,
                                      Create_Endpoint_Cap, Cap_Set_Cap,
                                      (Console_Interface_Cap,
                                       PCI_Cap,
                                       Reserve_Primary_IRQ,
                                       Reserve_Secondary_IRQ,
                                       Command_0_Cap,
                                       Control_0_Cap,
                                       Data_0_Cap_8,
                                       Data_0_Cap_Read_16,
                                       Data_0_Cap_Write_16,
                                       Timer_Cap));
      begin

         Hd0_Cap :=
           Copy_Cap_From_Process
             (Copy_Ata_Cap,
              Rose.Interfaces.Get_Interface_Endpoint,
              0);

         --  The install media is hd1, which only the install init uses.

         if Install then
            Hd1_Cap :=
              Copy_Cap_From_Process
                (Copy_Ata_Cap,
                 Rose.Interfaces.Get_Interface_Endpoint,
                 1);
         end if;

      end;

      declare
         Copy_Store_Cap : constant Rose.Capabilities.Capability :=
                      Init.Calls.Launch_Boot_Module
                        (Boot_Cap, Modules.Store, Store_Oid,
                         Device_Driver_Priority,
                         Create_Endpoint_Cap, Cap_Set_Cap,
                         (Delete_Cap,
                          Console_Interface_Cap,
                          Trace_Cap));
      begin
         Storage_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage.Storage_Interface);
         Add_Storage_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage.Add_Backing_Store_Endpoint);
         Reserve_Storage_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage.Reserve_Storage_Endpoint);
         Store_Checkpoint_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage_Checkpoint.Checkpoint_Endpoint);
         Store_Restore_Cap :=
           Copy_Cap_From_Process
             (Copy_Store_Cap,
              Rose.Interfaces.Storage_Checkpoint
              .Storage_Checkpoint_Interface);
      end;

      declare
         Create_Process_Cap : constant Rose.Capabilities.Capability :=
                                Init_Caps.Create_Process_Cap;
         Copy_Elf_Cap : constant Rose.Capabilities.Capability :=
                            Init.Calls.Launch_Boot_Module
                              (Boot_Cap, Modules.Elf, Elf_Oid,
                               Device_Driver_Priority,
                               Create_Endpoint_Cap, Cap_Set_Cap,
                               (Delete_Cap, Rescind_Cap,
                                Console_Interface_Cap,
                                Mem_Cap, Create_Process_Cap,
                                Trace_Cap));
      begin
         Launch_Elf_Cap :=
           Copy_Cap_From_Process
             (Copy_Elf_Cap,
              Rose.Interfaces.Executable.Launch_Endpoint);
         Launch_Command_Elf_Cap :=
           Copy_Cap_From_Process
             (Copy_Elf_Cap,
              Rose.Interfaces.Executable.Launch_Command_Endpoint);
      end;

      declare
         use Rose.Interfaces.Partitions;
         Copy_Scan_Cap : constant Rose.Capabilities.Capability :=
                     Init.Calls.Launch_Boot_Module
                       (Boot_Cap, Modules.Scan, Scan_Oid,
                        File_System_Priority,
                        Create_Endpoint_Cap, Cap_Set_Cap,
                        (Console_Interface_Cap,
                         Hd0_Cap));
         Count_Cap     : constant Rose.Capabilities.Capability :=
                           Copy_Cap_From_Process
                             (Copy_Scan_Cap, Partition_Count_Endpoint);
         Part_Cap      : constant Rose.Capabilities.Capability :=
                           Copy_Cap_From_Process
                             (Copy_Scan_Cap, Get_Partition_Endpoint);
         Parts         : Client.Partitions_Client;
      begin
         Client.Open_Cap_Set (Parts, Count_Cap, Part_Cap);

         for I in 1 .. Client.Partition_Count (Parts) loop
            Load_Partition
              (Client     => Parts,
               Device_Cap => Hd0_Cap,
               Index      => I);
         end loop;

      end;

      --  Block 0 isn't a partition, but restore-install writes GRUB's
      --  boot.img to it (#322).

      if Install then
         MBR_Cap := Launch_Partition_Server (MBR_Oid, Hd0_Cap, 0, 1, 512);
      end if;

      declare
         Install_Caps : Init.Calls.Array_Of_Capabilities (1 .. 2);
      begin
         Init.Calls.Call
           (Create_Endpoint_Cap,
            (Rose.Words.Word
                 (Rose.Interfaces.Exec.Install_Endpoint mod 2 ** 32),
             Rose.Words.Word
               (Rose.Interfaces.Exec.Install_Endpoint / 2 ** 32)),
            Install_Caps);
         Install_Receiver := Install_Caps (1);
         Install_Endpoint := Install_Caps (2);
      end;

      if Install then
         declare
            Copy_IsoFS_Cap : constant Rose.Capabilities.Capability :=
                               Init.Calls.Launch_Boot_Module
                                 (Boot_Cap, Modules.ISOFS, ISOFS_Oid,
                                  File_System_Priority,
                                  Create_Endpoint_Cap, Cap_Set_Cap,
                                  (Console_Interface_Cap,
                                   Hd1_Cap,
                                   Delete_Cap));
            --  isofs deletes the caps of the file readers that are
            --  released (ADR 0009).
         begin
            Install_Media_FS :=
              Copy_Cap_From_Process
                (Copy_IsoFS_Cap,
                 Rose.Interfaces.File_System.Root_Directory_Endpoint);
         end;
      end if;

      declare
         use Rose.Interfaces.Kernel_Log;
         Copy_Cap : constant Rose.Capabilities.Capability :=
           Init.Calls.Launch_Boot_Module
             (Boot_Cap, Modules.Log, Log_Oid,
              Log_Priority,
              Create_Endpoint_Cap, Cap_Set_Cap,
              (Console_Interface_Cap, Start_Log_Cap));
      begin
         Add_Log_Device_Cap :=
           Copy_Cap_From_Process
             (Copy_Cap, Add_Log_Device_Endpoint);
         Append_To_Log_Cap :=
           Copy_Cap_From_Process
             (Copy_Cap, Append_Endpoint);
         Log_Reader_Cap :=
           Copy_Cap_From_Process
             (Copy_Cap, Rose.Interfaces.Log_Reader.Log_Reader_Interface);
         Log_Reset_Cap :=
           Copy_Cap_From_Process
             (Copy_Cap, Rose.Interfaces.State.Reset_Endpoint);
      end;

      declare
         --  The kernel's restore endpoints (Rose.Kernel.Capabilities
         --  .Kernel_Caps): restore a process, let it run, and restore an
         --  interrupt handler.
         Restore_Process_Cap    : constant Rose.Capabilities.Capability :=
                                    Init_Caps.Restore_Process_Cap;
         Resume_Process_Cap     : constant Rose.Capabilities.Capability :=
                                    Init_Caps.Resume_Process_Cap;
         Restore_Interrupt_Cap  : constant Rose.Capabilities.Capability :=
                                    Init_Caps.Restore_Interrupt_Cap;

         --  The restore server's caps, in the order Restore.Start takes
         --  them.  Only restore-install has the last five, to install
         --  from the install media, start a new log, and write the boot
         --  loader (#322).

         Restore_Caps     : constant Init.Calls.Array_Of_Capabilities :=
                              (Console_Interface_Cap,
                               Swap_Cap,
                               Log_Cap,
                               Add_Storage_Cap,
                               Install_Endpoint,
                               Add_Log_Device_Cap,
                               Log_Reader_Cap,
                               Restore_Process_Cap,
                               Resume_Process_Cap,
                               Restore_Interrupt_Cap,
                               Store_Restore_Cap,
                               Memory_Restore_Cap,
                               Install_Media_FS,
                               Log_Reset_Cap,
                               MBR_Cap,
                               BIOS_Boot_Cap,
                               Boot_Partition_Cap);
         Last_Restore_Cap : constant Positive :=
                              (if Install
                               then Restore_Caps'Last
                               else Restore_Caps'Last - 5);

         Copy_Restore_Cap : constant Rose.Capabilities.Capability :=
                              Init.Calls.Launch_Boot_Module
                                (Boot_Cap, Modules.Restore, Restore_Oid,
                                 File_System_Priority,
                                 Create_Endpoint_Cap, Cap_Set_Cap,
                                 Restore_Caps (1 .. Last_Restore_Cap));

         pragma Unreferenced (Copy_Restore_Cap);
         Params : aliased Rose.Invocation.Invocation_Record;
         Reply  : aliased Rose.Invocation.Invocation_Record;
         Install_Cap : Rose.Capabilities.Capability :=
                         Rose.Capabilities.Null_Capability;
         Add_Cap     : Rose.Capabilities.Capability :=
                         Rose.Capabilities.Null_Capability;
         Do_Exec     : Boolean := True;
         Do_Command  : Boolean := True;
         Set_Output_Cap : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;

         function Installs (Name : String) return Boolean;
         --  True if Params asks to install the program called Name.

         --------------
         -- Installs --
         --------------

         function Installs (Name : String) return Boolean is
            use System.Storage_Elements;
            Buffer : Storage_Array (1 .. Params.Buffer_Length);
            pragma Import (Ada, Buffer);
            for Buffer'Address use Params.Buffer_Address;
         begin
            if not Params.Control.Flags (Rose.Invocation.Send_Buffer)
              or else Buffer'Length /= Name'Length
            then
               return False;
            end if;

            for I in Name'Range loop
               if Character'Val
                 (Buffer (Storage_Offset (I - Name'First + 1)))
                 /= Name (I)
               then
                  return False;
               end if;
            end loop;

            return True;
         end Installs;

      begin
         loop
            Rose.System_Calls.Initialize_Receive (Params, Install_Receiver);
            Rose.System_Calls.Receive_Caps (Params, 3);
            Rose.System_Calls.Receive_Words (Params, 3);
            Rose.System_Calls.Receive_Buffer (Params, 4096);
            Rose.System_Calls.Invoke_Capability (Params);
            --  The restore server's last message has no caps.  Its first
            --  data word is 1 if it restored a checkpoint.  The restore
            --  init installs nothing, so that is the only message it
            --  takes.

            if not Install
              or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
            then
               Restored := Params.Control.Flags (Rose.Invocation.Send_Words)
                 and then Params.Data (0) = 1;
               Restore_Reply_Cap := Params.Reply_Cap;
               exit;
            end if;

            Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

            if Do_Exec then
               Do_Exec := False;

               declare
                  Exec_Cap : constant Rose.Capabilities.Capability :=
                               Init.Installer.Install_Exec_Library
                                 (Storage_Cap     => Storage_Cap,
                                  Reserve_Cap     => Reserve_Storage_Cap,
                                  Launch_Cap      => Launch_Elf_Cap,
                                  Launch_Command_Cap =>
                                    Launch_Command_Elf_Cap,
                                  Trace_Cap       => Trace_Cap,
                                  Cap_Stream      => Params.Caps (0),
                                  Standard_Output => Console_Interface_Cap,
                                  Binary_Stream   => Params.Caps (1),
                                  Binary_Length   => Params.Data (0));
                  Caps     : Init.Calls.Array_Of_Capabilities
                    (1 .. Rose.Interfaces.Exec.Subprogram_Count);
                  Public_Cap : constant Rose.Capabilities.Capability :=
                                 Get_Public_Interface_From_Process (Exec_Cap);
               begin
                  Install_Cap := Null_Capability;

                  if Public_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "unable to find exec interface" & NL);
                  else
                     Init.Calls.Get_Interface (Public_Cap, Caps);
                     Install_Cap :=
                       Caps (Rose.Interfaces.Exec.Install_Index + 1);
                     Set_Output_Cap :=
                       Caps
                         (Rose.Interfaces.Exec.Set_Standard_Output_Index + 1);
                  end if;
               end;

            elsif Do_Command and then Installs ("command") then
               Do_Command := False;

               declare
                  Command_Cap : constant Rose.Capabilities.Capability :=
                                  Init.Installer.Install_Command_Library
                                    (Storage_Cap     => Storage_Cap,
                                     Reserve_Cap     => Reserve_Storage_Cap,
                                     Launch_Cap      => Launch_Elf_Cap,
                                     Cap_Stream      => Params.Caps (0),
                                     Standard_Output => Console_Interface_Cap,
                                     Binary_Stream   => Params.Caps (1),
                                     Binary_Length   => Params.Data (0));
                  Caps     : Init.Calls.Array_Of_Capabilities (1 .. 3);
                  Public_Cap : constant Rose.Capabilities.Capability :=
                                  Get_Public_Interface_From_Process
                                    (Command_Cap);
               begin
                  Add_Cap := Null_Capability;
                  for I in 1 .. 4 loop
                     Init.Calls.Get_Interface (Public_Cap, Caps);
                     Add_Cap := Caps (1);
                     Find_Cap := Caps (3);
                     exit when Add_Cap /= Null_Capability;
                     Wait (500);
                  end loop;

                  if Add_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "unable to find command interface" & NL);
                  end if;
                  if Find_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "find cap was null" & NL);
                  end if;
               end;
            elsif Params.Data (1) = 99 then
               Keyboard_Interface_Cap := Params.Caps (0);
               Console_Input_Stream_Cap := Params.Caps (1);
               if Console_Input_Stream_Cap = Null_Capability then
                  Init.Calls.Send_String
                    (Console_Stream_Cap,
                     "did not receive input stream cap" & NL);
               else
                  Init.Calls.Send_String
                    (Console_Stream_Cap, "received input stream cap" & NL);
               end if;
            elsif Params.Data (1) = 98 then

               --  The persistent console is running.  The boot console
               --  sends what it is given on to it, and it is the
               --  standard output of everything installed from now on.

               Console_Interface_Cap := Params.Caps (0);

               declare
                  Caps : Init.Calls.Array_Of_Capabilities (1 .. 1);

                  procedure Send_Console (To : Rose.Capabilities.Capability);

                  ------------------
                  -- Send_Console --
                  ------------------

                  procedure Send_Console
                    (To : Rose.Capabilities.Capability)
                  is
                     Call_Params : aliased Rose.Invocation.Invocation_Record;
                  begin
                     Rose.System_Calls.Initialize_Send (Call_Params, To);
                     Rose.System_Calls.Send_Cap
                       (Call_Params, Console_Interface_Cap);
                     Rose.System_Calls.Invoke_Capability (Call_Params);
                  end Send_Console;

               begin
                  Init.Calls.Get_Interface (Console_Interface_Cap, Caps);
                  Console_Stream_Cap := Caps (1);
                  Init.Calls.Set_Console (Console_Stream_Cap);
                  Send_Console (Console_Redirect_Cap);
                  Send_Console (Set_Output_Cap);
               end;
            else

               declare
                  Extra_Caps : constant
                    Init.Calls.Array_Of_Capabilities (1 .. 1)
                    := (1 => Params.Caps (2));
                  Last_Cap   : constant Natural
                    := (if Params.Caps (2)
                        = Null_Capability
                        then 0 else 1);
                  Name       : String (1 .. 64);
                  Name_Last  : Natural := 0;
                  Launch     : Capability;
                  Buffer     : System.Storage_Elements.Storage_Array
                    (1 .. 4096);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
               begin

                  for I in 1 .. Params.Buffer_Length loop
                     Name_Last := Name_Last + 1;
                     Name (Name_Last) :=
                       Character'Val (Buffer (I));
                  end loop;

                  Launch :=
                    Init.Installer.Install_Executable
                      (Install_Cap   => Install_Cap,
                       Cap_Stream    => Params.Caps (0),
                       Binary_Stream => Params.Caps (1),
                       Binary_Length => Params.Data (0),
                       Name          => Name (1 .. Name_Last),
                       Is_Driver     =>
                         (Params.Data (2) / Is_Driver_Flag) mod 2 = 1,
                       Extra_Caps    => Extra_Caps (1 .. Last_Cap));

                  Wait (1000);

                  --  Launch is the program's Launch interface. Restore
                  --  launches drivers itself, through the interface's
                  --  Launch cap, and saves commands with the interface.

                  if Params.Data (1) = 0 then
                     declare
                        Launch_Caps : Init.Calls.Array_Of_Capabilities
                          (1 .. 3);
                     begin
                        Init.Calls.Get_Interface (Launch, Launch_Caps);
                        Rose.System_Calls.Send_Cap (Reply, Launch_Caps (1));
                     end;
                  elsif Params.Data (1) = 1 then
                     Rose.System_Calls.Send_Cap (Reply, Add_Cap);
                     Rose.System_Calls.Send_Cap (Reply, Launch);
                  else
                     Init.Calls.Send_String
                       (Console_Stream_Cap, "init: bad action");
                  end if;
               end;
            end if;

            Rose.System_Calls.Invoke_Capability (Reply);
         end loop;
      end;

      --  The restore init doesn't install.  If there was no checkpoint to
      --  restore, the system stays as it is, with this message on the
      --  screen (#373).

      if not Install and then not Restored then
         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: no system on this disk; boot the install media" & NL);
         Init.Calls.Send (Exit_Cap);
         return;
      end if;

      declare
         Enter_Checkpoint_Cap : constant Rose.Capabilities.Capability :=
                                  Init_Caps.Enter_Checkpoint_Cap;
         Leave_Checkpoint_Cap : constant Rose.Capabilities.Capability :=
                                  Init_Caps.Leave_Checkpoint_Cap;
         Copy_Checkpoint_Cap  : constant Rose.Capabilities.Capability :=
                                  Init.Calls.Launch_Boot_Module
                                    (Boot_Cap, Modules.Checkpoint,
                                     Checkpoint_Oid,
                                     Checkpoint_Priority,
                                     Create_Endpoint_Cap, Cap_Set_Cap,
                                     (Console_Interface_Cap,
                                      Timer_Cap,
                                      Enter_Checkpoint_Cap,
                                      Leave_Checkpoint_Cap,
                                      Delete_Cap,
                                      Memory_Checkpoint_Cap,
                                      Append_To_Log_Cap,
                                      Store_Checkpoint_Cap));
         pragma Unreferenced (Copy_Checkpoint_Cap);
      begin
         null;
      end;

      --  After a restore, the programs that would be launched below are
      --  running already, as they were at the checkpoint.

      if Restored then

         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: restored from a checkpoint; exiting" & NL);

         --  The restored console redraws the screen as it was at the
         --  checkpoint, so the boot console stops writing to it.

         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Console_Redirect_Cap);
            Rose.System_Calls.Invoke_Capability (Params);
         end;

         --  The restore server lets the restored processes run when
         --  this reply arrives, now that the boot modules are started.

         declare
            Reply : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Reply (Reply, Restore_Reply_Cap);
            Rose.System_Calls.Invoke_Capability (Reply);
         end;

         Init.Calls.Send (Exit_Cap);
         return;
      end if;

      Init.Calls.Send_String
        (Console_Stream_Cap,
         "completed initial install" & NL);

      if Find_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to find 'find'" & NL);
      end if;

      if Cap_Set_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to find 'cap_set'" & NL);
      end if;

      declare
         Launch_Tests_Cap : constant Rose.Capabilities.Capability :=
                              Init.Calls.Find_In_Map
                                (Find_Cap => Find_Cap,
                                 Key      => "tests");
         Standard_Caps    : constant Rose.Capabilities.Capability :=
                              Init.Calls.Create_Cap_Set_With
                                (Create_Cap_Set => Cap_Set_Cap,
                                 Caps           =>
                                 --  standard_input
                                   (Console_Input_Stream_Cap,

                                    --  standard_output
                                    Console_Stream_Cap,

                                    --  standard_error
                                    Console_Stream_Cap,

                                    --  current_directory
                                    Null_Capability,

                                    --  clock
                                    Null_Capability,

                                    --  delete_cap
                                    Delete_Cap,

                                    --  rescind_cap
                                    Rescind_Cap,

                                    --  create_endpoint
                                    Create_Endpoint_Cap,

                                    --  create_cap_set
                                    Cap_Set_Cap,

                                    --  the command registry's Find, for
                                    --  launching a file server
                                    Find_Cap
                                   ));
         Tests_Object     : Rose.Objects.Object_Id with Unreferenced;
      begin
         if Launch_Tests_Cap = Rose.Capabilities.Null_Capability then
            Init.Calls.Send_String
              (Console_Stream_Cap, "init: unable to find 'tests'" & NL);
         else
            Init.Calls.Send_String
              (Console_Stream_Cap,
               "launching tests" & NL);
            Tests_Object :=
              Init.Calls.Launch
                (Launch_Tests_Cap, Standard_Caps, "tests");
         end if;
      end;

      Wait (2000);

      --  The first user, with a home directory (ADR 0003). Init runs
      --  only on a fresh boot, so it is created once.

      Init.Calls.Send_String
        (Console_Stream_Cap, "creating user " & First_User & NL);
      Init.Users.Create_First_User
        (Name            => First_User,
         Find_Cap        => Find_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         User            => User_Cap,
         Home            => Home_Cap);
      if User_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: unable to create user " & First_User & NL);
      end if;

      declare
         Launch_Petal_Cap : constant Rose.Capabilities.Capability :=
                              Init.Calls.Find_In_Map
                                (Find_Cap => Find_Cap,
                                 Key      => "petal");
         Standard_Caps    : constant Rose.Capabilities.Capability :=
                              Init.Calls.Create_Cap_Set_With
                                (Create_Cap_Set => Cap_Set_Cap,
                                 Caps           =>
                                 --  standard_input
                                   (Console_Input_Stream_Cap,

                                    --  standard_output
                                    Console_Stream_Cap,

                                    --  standard_error
                                    Console_Stream_Cap,

                                    --  current_directory: the
                                    --  user's home directory
                                    Home_Cap,

                                    --  clock
                                    Null_Capability,

                                    --  delete_cap
                                    Delete_Cap,

                                    --  rescind_cap
                                    Rescind_Cap,

                                    --  create_endpoint
                                    Create_Endpoint_Cap,

                                    --  create_cap_set
                                    Cap_Set_Cap,

                                    --  the command registry's Find, for
                                    --  running commands (ADR 0002)
                                    Find_Cap,

                                    --  trace events (Rose.Trace)
                                    Trace_Cap,

                                    --  the user (interfaces/user.idl)
                                    User_Cap
                                   ));

         Petal_Object     : Rose.Objects.Object_Id with Unreferenced;
      begin
         if Launch_Petal_Cap = Rose.Capabilities.Null_Capability then
            Init.Calls.Send_String
              (Console_Stream_Cap, "init: unable to find 'petal'" & NL);
         else
            Init.Calls.Send_String
              (Console_Stream_Cap,
               "launching petal" & NL);
            Petal_Object :=
              Init.Calls.Launch
                (Launch_Petal_Cap, Standard_Caps, "petal");
         end if;
      end;

      Init.Calls.Send_String
        (Console_Stream_Cap, "init: exiting" & NL);
      Init.Calls.Send (Exit_Cap);

   end Run_Init;

end Init.Run;
