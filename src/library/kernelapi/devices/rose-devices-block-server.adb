with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Console_IO;

package body Rose.Devices.Block.Server is

   ------------------------
   -- Invoke_Receive_Cap --
   ------------------------

   procedure Run_Server
     (Endpoint_Cap   : Rose.Capabilities.Capability;
      Get_Parameters : Get_Parameters_Handler;
      Read_Handler   : Read_Block_Handler;
      Write_Handler  : Write_Block_Handler;
      Get_Interface  : Get_Interface_Handler)
   is
      use Rose.Invocation;
      use Rose.Words;
      Check_Start : array (Word_8) of Word_8;
      Params      : aliased Rose.Invocation.Invocation_Record;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Endpoint_Cap);
      Check_End      : array (Word_8) of Rose.Words.Word_8;
   begin

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Endpoint_Cap, Rose.Devices.Get_Interface_Endpoint);

      for I in Check_Start'Range loop
         Check_Start (I) := I;
         Check_End (I) := I;
      end loop;

      loop
         Params.Control.Flags :=
           (Rose.Invocation.Receive     => True,
            Rose.Invocation.Block       => True,
            Rose.Invocation.Recv_Words  => True,
            Rose.Invocation.Recv_Buffer => True,
            others                      => False);
         Params.Control.Last_Recv_Word := Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         for I in Check_Start'Range loop
            if Check_Start (I) /= I then
               Rose.Console_IO.Put ("start of stack clobbered at position ");
               Rose.Console_IO.Put (I);
               Rose.Console_IO.Put (": expected ");
               Rose.Console_IO.Put (I);
               Rose.Console_IO.Put (" but found ");
               Rose.Console_IO.Put (Check_Start (I));
               Rose.Console_IO.New_Line;
               exit;
            end if;
         end loop;

         for I in Check_End'Range loop
            if Check_End (I) /= I then
               Rose.Console_IO.Put ("end of stack clobbered at position ");
               Rose.Console_IO.Put (I);
               Rose.Console_IO.Put (": expected ");
               Rose.Console_IO.Put (I);
               Rose.Console_IO.Put (" but found ");
               Rose.Console_IO.Put (Check_End (I));
               Rose.Console_IO.New_Line;
               exit;
            end if;
         end loop;

         if Params.Control.Flags (Error) then
            Rose.Console_IO.Put ("error: ");
            Rose.Console_IO.Put (Params.Data (0));
            Rose.Console_IO.New_Line;
            return;
         end if;

         case Params.Endpoint is
            when Get_Device_Parameters_Endpoint =>
               declare
                  Block_Size : Block_Size_Type;
                  Block_Count : Block_Address_Type;
               begin
                  Get_Parameters (Params.Identifier, Block_Size, Block_Count);
                  Rose.System_Calls.Server.Send_Reply
                    (Params.Reply_Cap,
                     (Rose.Words.Word (Block_Size),
                      Rose.Words.Word (Block_Count)));
               end;

            when Read_Block_Endpoint =>
               declare
                  use System.Storage_Elements;
                  Count   : constant Storage_Count := Params.Buffer_Length;
                  Address : constant System.Address :=
                              Params.Buffer_Address;
                  Buffer  : Storage_Array (1 .. Count);
                  Block_Address : constant Block_Address_Type :=
                                    Block_Address_Type
                                      (Params.Data (0));
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Address;
               begin
--                    if Count /= Storage_Count (Block_Size) then
--                       Rose.Console_IO.Put
--                         ("read-block: received buffer size ");
--                       Rose.Console_IO.Put
--                         (Natural (Count));
--                       Rose.Console_IO.Put
--                         (" but block size is ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Size));
--                       Rose.Console_IO.New_Line;
--                    elsif Block_Address >= Block_Count then
--                       Rose.Console_IO.Put
--                         ("read-block: received request for block ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Address));
--                       Rose.Console_IO.Put
--                         (" but block count is ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Count));
--                       Rose.Console_IO.New_Line;
--                    else
                  Read_Handler (Params.Identifier, Block_Address, Buffer);
                  Rose.System_Calls.Server.Send_Reply (Params.Reply_Cap);
--                    end if;
               end;

            when Write_Block_Endpoint =>
               declare
                  use System.Storage_Elements;
                  Count         : constant Storage_Count :=
                                    Params.Buffer_Length;
                  Address       : constant System.Address :=
                                    Params.Buffer_Address;
                  Buffer        : Storage_Array (1 .. Count);
                  Block_Address : constant Block_Address_Type :=
                                    Block_Address_Type
                                      (Params.Data (0));
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Address;
               begin
--                    if Count /= Storage_Count (Block_Size) then
--                       Rose.Console_IO.Put
--                         ("write-block: received buffer size ");
--                       Rose.Console_IO.Put
--                         (Natural (Count));
--                       Rose.Console_IO.Put
--                         (" but block size is ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Size));
--                       Rose.Console_IO.New_Line;
--                    elsif Block_Address >= Block_Count then
--                       Rose.Console_IO.Put
--                         ("write-block: received request for block ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Address));
--                       Rose.Console_IO.Put
--                         (" but block count is ");
--                       Rose.Console_IO.Put
--                         (Natural (Block_Count));
--                       Rose.Console_IO.New_Line;
--                    else
                  Write_Handler (Params.Identifier, Block_Address, Buffer);
                  Rose.System_Calls.Server.Send_Reply (Params.Reply_Cap);
--                    end if;
               end;

            when Get_Interface_Endpoint =>
               declare
                  Get, Read, Write : Rose.Capabilities.Capability;
               begin
                  Get_Interface
                    (Rose.Objects.Capability_Identifier (Params.Data (0)),
                     Get, Read, Write);
                  Rose.System_Calls.Server.Send_Reply_Caps
                    (Params.Reply_Cap,
                     (Get, Read, Write));
               end;

            when others =>
               Rose.Console_IO.Put
                 ("block-device: unrecognised endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word (Params.Cap));
               Rose.Console_IO.New_Line;

         end case;
      end loop;
   end Run_Server;

end Rose.Devices.Block.Server;
