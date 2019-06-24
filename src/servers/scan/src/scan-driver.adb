with Rose.Interfaces.Block_Device.Client;
with Rose.Devices.GPT;
with Rose.Devices.Partitions;

with Rose.Console_IO;

with Rose.Words;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Rose.Interfaces.Partitions;

procedure Scan.Driver is

   use Rose.Interfaces.Block_Device;
   use Rose.Interfaces.Block_Device.Client;

   Device : Block_Device_Client;
   Block_Size   : Rose.Interfaces.Block_Device.Block_Size_Type;
   Block_Count  : Rose.Interfaces.Block_Device.Block_Address_Type;

begin

   Console_Cap := Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
   Block_Device_Cap := Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

   Rose.Console_IO.Open (Console_Cap);

   Rose.Interfaces.Block_Device.Client.Open
     (Client         => Device,
      Interface_Cap  => Block_Device_Cap);

   Get_Parameters (Device, Block_Count, Block_Size);

   declare
      use Rose.Words;
      Storage_Size : constant Device_Size_Type :=
                       Device_Size_Type (Block_Size)
                       * Device_Size_Type (Block_Count);
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
--      First_Block         : Rose.Interfaces.Block_Device.Block_Address_Type;
--      Last_Block          : Rose.Interfaces.Block_Device.Block_Address_Type;
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
            use Rose.Devices.Partitions;
            Available_Blocks : constant Block_Address_Type :=
                                 Block_Count - 4;
            Partition_Size   : constant Block_Address_Type :=
                                 Available_Blocks / 4;
            Current_Start    : Block_Address_Type := 3;
         begin


            Rose.Console_IO.Put_Line ("creating partitions");

            Rose.Devices.GPT.Add_Partition
              (Block_Device        => Device,
               First_Block         => Current_Start,
               Last_Block          => Current_Start + Partition_Size - 1,
               Partition_Type_Low  => Swap_Id_Low,
               Partition_Type_High => Swap_Id_High,
               Partition_Flags     => Active_Swap_Flag,
               Partition_Name      => "rose-storage-1");

            Current_Start := Current_Start + Partition_Size;

            Rose.Devices.GPT.Add_Partition
              (Block_Device        => Device,
               First_Block         => Current_Start,
               Last_Block          => Current_Start + Partition_Size - 1,
               Partition_Type_Low  => Swap_Id_Low,
               Partition_Type_High => Swap_Id_High,
               Partition_Flags     => 0,
               Partition_Name      => "rose-storage-2");

            Current_Start := Current_Start + Partition_Size;

            Rose.Devices.GPT.Add_Partition
              (Block_Device        => Device,
               First_Block         => Current_Start,
               Last_Block          => Available_Blocks,
               Partition_Type_Low  => Log_Id_Low,
               Partition_Type_High => Log_Id_High,
               Partition_Flags     => 0,
               Partition_Name      => "rose-log");

            Rose.Devices.GPT.Flush (Device);

         end;

         Rose.Devices.GPT.Report_Partition_Table (Device);
      end if;
   end;

   Rose.Console_IO.Put_Line ("scan: done");

   declare
      use Rose.Invocation;
      use Rose.System_Calls;
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Cap   => Create_Endpoint_Cap);
      Partition_Count : constant Natural :=
                          Rose.Devices.GPT.Partition_Count (Device);
   begin
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Partitions.Partition_Count_Endpoint);
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Partitions.Get_Partition_Endpoint);

      loop
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

         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Partitions.Partition_Count_Endpoint =>
               Rose.System_Calls.Send_Word
                 (Reply,
                  Rose.Words.Word_32 (Partition_Count));

            when Rose.Interfaces.Partitions.Get_Partition_Endpoint =>
               declare
                  Index : constant Natural :=
                            Natural (Params.Data (0));
               begin
                  if Index in 1 .. Partition_Count then
                     declare
                        First_Block         : Block_Address_Type;
                        Last_Block          : Block_Address_Type;
                        Partition_Type_Low  : Rose.Words.Word_64;
                        Partition_Type_High : Rose.Words.Word_64;
                        Partition_Flags     : Rose.Words.Word_64;
                        Partition_Name      : String (1 .. 32);
                        Partition_Name_Last : Natural;
                     begin
                        Rose.Devices.GPT.Get_Partition_Details
                          (Block_Device        => Device,
                           Partition_Index     => Index,
                           First_Block         => First_Block,
                           Last_Block          => Last_Block,
                           Partition_Type_Low  => Partition_Type_Low,
                           Partition_Type_High => Partition_Type_High,
                           Partition_Flags     => Partition_Flags,
                           Partition_Name      => Partition_Name,
                           Partition_Name_Last => Partition_Name_Last);

                        Rose.System_Calls.Send_Word
                          (Reply, Partition_Type_Low);
                        Rose.System_Calls.Send_Word
                          (Reply, Partition_Type_High);
                        Rose.System_Calls.Send_Word
                          (Reply, Partition_Flags);
                        Rose.System_Calls.Send_Word
                          (Reply, Rose.Words.Word_64 (First_Block));
                        Rose.System_Calls.Send_Word
                          (Reply,
                           Rose.Words.Word_64 (Last_Block - First_Block));
                     end;

                  else
                     Rose.Console_IO.Put_Line ("scan: bad partition index");
                  end if;
               end;
            when others =>
               Rose.Console_IO.Put_Line ("scan: bad endpoint");

         end case;

         Rose.System_Calls.Invoke_Capability (Reply);
      end loop;
   end;

end Scan.Driver;
