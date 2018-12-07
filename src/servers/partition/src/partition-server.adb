with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Command_Line;

with Rose.Interfaces.Block_Device.Client;

package body Partition.Server is

   use Rose.Interfaces.Block_Device;

--     Partition_Base_Address : Block_Address_Type;
   Partition_Block_Count  : Block_Address_Type;
   Partition_Block_Size   : Block_Size_Type;

   Device_Base_Address    : Block_Address_Type;
   Device_Block_Count     : Block_Address_Type;
   Device_Block_Size      : Block_Size_Type;

   Block_Size_Ratio       : Block_Address_Type;

   Device_Total_Block_Count : Block_Address_Type;

   Device_Client      : Client.Block_Device_Client;

   Get_Parameters_Cap   : Rose.Capabilities.Capability;
   Read_Cap, Write_Cap  : Rose.Capabilities.Capability;

--     function To_Device_Block_Count
--       (Count : Block_Address_Type)
--        return Block_Address_Type
--     is (Count * Block_Size_Ratio);
--
--     function To_Partition_Block_Count
--       (Count : Block_Address_Type)
--        return Block_Address_Type
--     is ((Count + Block_Size_Ratio - 1) / Block_Size_Ratio);
--

   function To_Word_32
     (X : String)
      return Rose.Words.Word_32;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Client.Open
        (Device_Client, Block_Device_Cap);

      Client.Get_Parameters
        (Device_Client, Device_Total_Block_Count, Device_Block_Size);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Get_Interface_Endpoint);

      Get_Parameters_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Block_Device.Get_Parameters_Endpoint);

      Read_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Block_Device.Read_Blocks_Endpoint);

      Write_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Block_Device.Write_Blocks_Endpoint);

      declare
         Argument : String (1 .. 20);
         Last     : Natural;
      begin
         Rose.Command_Line.Get_Argument (1, Argument, Last);
         Device_Base_Address :=
           Block_Address_Type (To_Word_32 (Argument (1 .. Last)));

         Rose.Command_Line.Get_Argument (2, Argument, Last);
         Device_Block_Count :=
           Block_Address_Type (To_Word_32 (Argument (1 .. Last)))
           - Device_Base_Address + 1;

         Rose.Command_Line.Get_Argument (3, Argument, Last);
         Partition_Block_Size :=
           Block_Size_Type (To_Word_32 (Argument (1 .. Last)));
      end;

      Block_Size_Ratio :=
        Block_Address_Type (Partition_Block_Size / Device_Block_Size);

--        Partition_Base_Address :=
--          (Device_Base_Address + Block_Size_Ratio - 1) / Block_Size_Ratio;

      declare
         use Rose.Words;
      begin
         Partition_Block_Count :=
           Block_Address_Type
             (Word (Device_Block_Count) / Word (Block_Size_Ratio));
      end;

      Rose.Console_IO.Put
        ("partition: start ");
      Rose.Console_IO.Put
        ((Natural (Device_Base_Address)
         + Natural (Block_Size_Ratio) - 1)
         / Natural (Block_Size_Ratio));

      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Base_Address));
      Rose.Console_IO.Put
        (" blocks ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Count));
      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Block_Count));
      Rose.Console_IO.Put
        (" block size ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Size));
      Rose.Console_IO.Put ("/");
      Rose.Console_IO.Put
        (Natural (Device_Block_Size));

      Rose.Console_IO.Put
        (" total ");
      Rose.Console_IO.Put
        (Natural (Partition_Block_Count)
         * Natural (Partition_Block_Size)
         / 1024 / 1024);
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;

   end Create_Server;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Endpoint_Cap);
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
   begin
      loop
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               Rose.System_Calls.Send_Cap (Reply, Get_Parameters_Cap);
               Rose.System_Calls.Send_Cap (Reply, Read_Cap);
               Rose.System_Calls.Send_Cap (Reply, Write_Cap);

            when Rose.Interfaces.Block_Device.Get_Parameters_Endpoint =>
               Rose.System_Calls.Send_Word
                 (Reply, Rose.Words.Word_64 (Partition_Block_Count));
               Rose.System_Calls.Send_Word
                 (Reply, Rose.Words.Word_32 (Partition_Block_Size));
            when Rose.Interfaces.Block_Device.Read_Blocks_Endpoint =>
               declare
                  use System.Storage_Elements;
                  Part_Address : constant Block_Address_Type :=
                                  Block_Address_Type
                                     (Rose.System_Calls.Get_Word_64
                                        (Params, 0));
                  Part_Count   : constant Natural :=
                                   Natural (Rose.System_Calls.Get_Word_32
                                            (Params, 2));
                  Storage      : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Storage);
                  for Storage'Address use Params.Buffer_Address;
               begin
                  Client.Read_Blocks
                    (Item   => Device_Client,
                     Start  =>
                       Device_Base_Address + Part_Address * Block_Size_Ratio,
                     Count  => Part_Count * Positive (Block_Size_Ratio),
                     Blocks => Storage);
               end;
            when Rose.Interfaces.Block_Device.Write_Blocks_Endpoint =>
               declare
                  use System.Storage_Elements;
                  Part_Address : constant Block_Address_Type :=
                                   Block_Address_Type
                                     (Rose.System_Calls.Get_Word_64
                                        (Params, 0));
                  Part_Count   : constant Natural :=
                                   Natural (Rose.System_Calls.Get_Word_32
                                            (Params, 2));
                  Storage      : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Storage);
                  for Storage'Address use Params.Buffer_Address;
               begin
                  Client.Write_Blocks
                    (Item   => Device_Client,
                     Start  =>
                       Device_Base_Address + Part_Address * Block_Size_Ratio,
                     Count  => Part_Count * Positive (Block_Size_Ratio),
                     Blocks => Storage);
               end;
            when others =>
               Rose.Console_IO.Put ("partition: bad endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
               Rose.Console_IO.New_Line;
         end case;
         Rose.System_Calls.Invoke_Capability (Reply);
      end loop;
   end Start_Server;

   ----------------
   -- To_Word_32 --
   ----------------

   function To_Word_32
     (X : String)
      return Rose.Words.Word_32
   is
      use Rose.Words;
   begin
      return W : Word_32 := 0 do
         for Ch of X loop
            if Ch in '0' .. '9' then
               W := W * 10 + Character'Pos (Ch) - 48;
            end if;
         end loop;
      end return;
   end To_Word_32;

end Partition.Server;
