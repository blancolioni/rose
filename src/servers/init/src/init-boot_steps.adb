with Rose.Init_Capabilities;
with Rose.Words;

with Rose.Devices.Partitions;

with Rose.Interfaces.Block_Device;
with Rose.Interfaces.Partitions.Client;

with Init.Boot;
with Init.Calls;

package body Init.Boot_Steps is

   NL : constant Character := Character'Val (10);

   function Launch_Partition_Server
     (Oid        : Rose.Objects.Object_Id;
      Start      : Rose.Words.Word_64;
      Bound      : Rose.Words.Word_64;
      Block_Size : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  Launch a partition server for hd0's blocks Start .. Bound - 1, in
   --  512-byte blocks, and return its block device interface, which has
   --  blocks of Block_Size bytes.

   ----------
   -- Copy --
   ----------

   function Copy
     (Copy_Cap   : Rose.Capabilities.Capability;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities, Rose.Words;
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

      if Init.Boot.Timer_Cap = Null_Capability then
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
         if Init.Boot.Console_Interface_Cap /= Null_Capability then
            Init.Calls.Send_String
              (Init.Boot.Console_Interface_Cap, Fail_Message);
         end if;
         Init.Calls.Send (Rose.Init_Capabilities.Exit_Cap);
      end if;

      return Cap;
   end Copy;

   -----------------------------
   -- Launch_Partition_Server --
   -----------------------------

   function Launch_Partition_Server
     (Oid        : Rose.Objects.Object_Id;
      Start      : Rose.Words.Word_64;
      Bound      : Rose.Words.Word_64;
      Block_Size : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      Copy_Id_Cap : constant Rose.Capabilities.Capability :=
                      Init.Calls.Launch_Boot_Module
                        (Rose.Init_Capabilities.Boot_Cap,
                         Init.Boot.Modules.Partition, Oid,
                         Init.Boot.Device_Driver_Priority,
                         Rose.Init_Capabilities.Create_Endpoint_Cap,
                         Init.Boot.Cap_Set_Cap,
                         (Init.Boot.Console_Interface_Cap,
                          Init.Boot.Hd0_Cap),
                         (Rose.Words.Word (Start),
                          Rose.Words.Word (Bound),
                          Block_Size));
   begin
      return Copy
        (Copy_Id_Cap,
         Rose.Interfaces.Block_Device.Block_Device_Interface);
   end Launch_Partition_Server;

   ---------------------
   -- Load_Partitions --
   ---------------------

   procedure Load_Partitions is
      use Rose.Interfaces.Partitions;
      Parts : Client.Partitions_Client;

      procedure Load_Partition (Index : Positive);

      --------------------
      -- Load_Partition --
      --------------------

      procedure Load_Partition (Index : Positive) is
         use Rose.Devices.Partitions;
         use type Rose.Capabilities.Capability;
         use type Rose.Words.Word_64;
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
               (Oid, Start_Address, Start_Address + Length, Block_Size));

      begin
         Client.Get_Partition
           (Parts, Index, Partition_Type_Low,
            Partition_Type_High, Partition_Flags,
            Start_Address, Length);

         --  Only the first partition of each type is used.  The install
         --  media's init also uses GRUB's, to write the boot loader
         --  (#322).

         if Is_Type (Swap_Id_Low, Swap_Id_High) then
            if Init.Boot.Swap_Cap = 0 then
               Init.Boot.Swap_Cap :=
                 Launch (Init.Boot.Swap_Partition_Oid, 4096);
            end if;
         elsif Is_Type (Log_Id_Low, Log_Id_High) then
            if Init.Boot.Log_Cap = 0 then
               Init.Boot.Log_Cap :=
                 Launch (Init.Boot.Log_Partition_Oid, 512);
            end if;
         elsif not Init.Boot.Installing then
            null;
         elsif Is_Type (BIOS_Boot_Id_Low, BIOS_Boot_Id_High) then
            if Init.Boot.BIOS_Boot_Cap = 0 then
               Init.Boot.BIOS_Boot_Cap :=
                 Launch (Init.Boot.BIOS_Boot_Oid, 512);
            end if;
         elsif Is_Type (EFI_System_Id_Low, EFI_System_Id_High) then
            if Init.Boot.Boot_Partition_Cap = 0 then
               Init.Boot.Boot_Partition_Cap :=
                 Launch (Init.Boot.Boot_Partition_Oid, 4096);
            end if;
         end if;
      end Load_Partition;

   begin
      Client.Open_Cap_Set
        (Parts, Init.Boot.Partition_Count_Cap, Init.Boot.Get_Partition_Cap);

      for I in 1 .. Client.Partition_Count (Parts) loop
         Load_Partition (I);
      end loop;

      --  Block 0 isn't a partition, but restore-install writes GRUB's
      --  boot.img to it (#322).

      if Init.Boot.Installing then
         Init.Boot.MBR_Cap :=
           Launch_Partition_Server (Init.Boot.MBR_Oid, 0, 1, 512);
      end if;
   end Load_Partitions;

   -----------------
   -- Set_Console --
   -----------------

   procedure Set_Console is
   begin
      Init.Calls.Set_Console (Init.Boot.Console_Interface_Cap);
   end Set_Console;

   ----------
   -- Wait --
   ----------

   procedure Wait (Milliseconds : Natural) is
      Control_Cap : constant Rose.Capabilities.Capability :=
                      Init.Calls.Call
                        (Init.Boot.Timer_Cap, Init.Boot.Send_Timeout_Cap,
                         (1 => Rose.Words.Word (Milliseconds)));
   begin
      pragma Unreferenced (Control_Cap);
      Init.Calls.Receive (Init.Boot.Receive_Timeout_Cap);
   end Wait;

end Init.Boot_Steps;
