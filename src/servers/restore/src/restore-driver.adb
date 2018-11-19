with Rose.Devices.Block.Client;
with Rose.Devices.GPT;
with Rose.Devices.Partitions;

with Rose.Console_IO;

with Rose.Words;

with Rose.Invocation;
with Rose.System_Calls.Server;

procedure Restore.Driver is
   use Rose.Devices.Block.Client;
   Device : Block_Device_Type;
begin
   Rose.Console_IO.Open (Console_Cap);

   Rose.Devices.Block.Client.Open
     (Device         => Device,
      Parameters_Cap => Block_Device_Parameters_Cap,
      Read_Cap       => Block_Device_Read_Cap,
      Write_Cap      => Block_Device_Write_Cap);

   declare
      use Rose.Words;
      Block_Size   : constant Rose.Devices.Block.Block_Size_Type :=
                       Get_Block_Size (Device);
      Block_Count  : constant Rose.Devices.Block.Block_Address_Type :=
                       Get_Block_Count (Device);
      Storage_Size : constant Rose.Devices.Block.Device_Size_Type :=
                       Rose.Devices.Block.To_Device_Size
                         (Block_Size, Block_Count);
   begin
      Rose.Console_IO.Put ("device: size ");
      Rose.Console_IO.Put (Natural (Word_64 (Storage_Size) / 2 ** 20));
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;

      if Rose.Devices.GPT.Has_GPT (Device) then
         Rose.Console_IO.Put_Line ("Found GPT partition header");
      else

         Rose.Console_IO.Put_Line ("Intitialising device");

         Rose.Devices.GPT.Initialize_GPT (Device);
      end if;

      if not Rose.Devices.GPT.Has_GPT (Device) then
         Rose.Console_IO.Put_Line ("Device initialization failed");
         return;
      end if;

      if Rose.Devices.GPT.Partition_Count (Device) > 0 then
         Rose.Console_IO.Put_Line ("Scanning partitions");
         Rose.Devices.GPT.Report_Partition_Table (Device);
--
--           for I in 1 .. Rose.Devices.GPT.Partition_Count (Device) loop
--              declare
--                 First_Block         : Rose.Devices.Block.Block_Address_Type;
--                 Last_Block          : Rose.Devices.Block.Block_Address_Type;
--                 Partition_Type_Low  : Rose.Words.Word_64;
--                 Partition_Type_High : Rose.Words.Word_64;
--                 Partition_Flags     : Rose.Words.Word_64;
--                 Partition_Name      : String (1 .. 40);
--                 Partition_Name_Last : Natural;
--              begin
--                 Rose.Devices.GPT.Get_Partition_Details
--                   (Device, I,
--                    First_Block, Last_Block,
--                    Partition_Type_Low, Partition_Type_High,
--                    Partition_Flags,
--                    Partition_Name, Partition_Name_Last);
--
--
--           end loop;

      else
         declare
            use Rose.Devices.Block;
            use Rose.Devices.Partitions;
            Available_Blocks : constant Block_Address_Type :=
                                 Block_Count - 4;
            Partition_Size   : constant Block_Address_Type :=
                                 Available_Blocks / 4;
            Current_Start    : Block_Address_Type := 3;
            Name             : String  := "rose-swap-#";
         begin

            Rose.Console_IO.Put_Line ("creating partitions");

            for I in 1 .. 4 loop
               Name (Name'Last) :=
                 Character'Val (48 + I);

               Rose.Devices.GPT.Add_Partition
                 (Block_Device        => Device,
                  First_Block         => Current_Start,
                  Last_Block          => Current_Start + Partition_Size - 1,
                  Partition_Type_Low  => Swap_Id_Low,
                  Partition_Type_High => Swap_Id_High,
                  Partition_Flags     => 0,
                  Partition_Name      => Name);

               Current_Start := Current_Start + Partition_Size;

            end loop;

            Rose.Devices.GPT.Flush (Device);

         end;

         Rose.Devices.GPT.Report_Partition_Table (Device);
      end if;
   end;

   Rose.Console_IO.Put_Line ("restore: done");

   declare
      use Rose.Invocation;
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Cap   => Create_Endpoint_Cap);
   begin
      Params :=
        Invocation_Record'
          (Control       =>
             Control_Word'
               (Flags          =>
                  (Receive     => True,
                   Block       => True,
                   Recv_Words  => True,
                   Recv_Caps   => True,
                   others      => False),
                others         => <>),
           Cap           => Receive_Cap,
           others        => <>);

      Invoke_Capability (Params);
   end;

end Restore.Driver;
