with Rose.Devices.Block.Client;
with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.File_System;
with Rose.Console_IO;

with IsoFS.Directories;

package body IsoFS.Server is

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Device : Rose.Devices.Block.Client.Block_Device_Type;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Endpoint_Cap);
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.Devices.Block.Client.Open
        (Device, Device_Parameters_Cap, Device_Read_Cap, 0);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.File_System.Root_Directory_Endpoint);

      Rose.Console_IO.Put_Line ("isofs: starting server");

      loop
         Params := (others => <>);
         Params.Control.Flags (Rose.Invocation.Receive) := True;
         Params.Control.Flags (Rose.Invocation.Block) := True;
         Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
         Params.Control.Flags (Rose.Invocation.Recv_Buffer) := True;
         Params.Control.Last_Recv_Word :=
           Rose.Invocation.Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         case Params.Endpoint is
            when Rose.Interfaces.File_System.Root_Directory_Endpoint =>
               declare
                  Root : constant IsoFS.Directories.Directory_Type :=
                           IsoFS.Directories.Get_Root_Directory
                             (Device);
               begin
                  Reply := (others => <>);
                  Reply.Control.Flags (Rose.Invocation.Reply) := True;
                  Reply.Cap := Params.Reply_Cap;
                  IsoFS.Directories.Send_Directory_Caps (Root, Reply);
                  Rose.System_Calls.Invoke_Capability (Reply);
               end;
            when others =>
               Rose.Console_IO.Put
                 ("unknown endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_32 (Params.Endpoint));
               Rose.Console_IO.New_Line;
         end case;
      end loop;
   end Start_Server;

end IsoFS.Server;
