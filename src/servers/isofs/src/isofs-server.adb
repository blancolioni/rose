with System.Storage_Elements;

with Rose.Devices.Block.Client;
with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Directory;
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

            when Rose.Interfaces.Directory.Directory_Entry_Count_Endpoint =>
               declare
                  use IsoFS.Directories;
                  Directory : constant Directory_Type :=
                                Get_Identified_Directory
                                  (Params.Identifier);
               begin
                  if Directory = No_Directory then
                     Rose.Console_IO.Put_Line
                       ("cap does not resolve to a directory");
                  else
                     Reply := (others => <>);
                     Reply.Control.Flags (Rose.Invocation.Reply) := True;
                     Reply.Control.Flags (Rose.Invocation.Send_Words) := True;
                     Reply.Control.Last_Sent_Word := 0;
                     Reply.Cap := Params.Reply_Cap;
                     Reply.Data (0) :=
                       Rose.Words.Word (Get_Entry_Count (Directory));
                     Rose.System_Calls.Invoke_Capability (Reply);
                  end if;
               end;

            when Rose.Interfaces.Directory.Directory_Entry_Name_Endpoint =>
               declare
                  use IsoFS.Directories;
                  use System.Storage_Elements;
                  Directory : constant Directory_Type :=
                                Get_Identified_Directory
                                  (Params.Identifier);
                  Index     : constant Natural := Natural (Params.Data (0));
                  Name      : String (1 .. 255);
                  Last      : Natural;
                  Addr      : constant System.Address := Params.Buffer_Address;
                  Buffer    : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Addr;
               begin
                  if Directory = No_Directory then
                     Rose.Console_IO.Put_Line
                       ("cap does not resolve to a directory");
                  elsif Index = 0 then
                     Rose.Console_IO.Put_Line
                       ("invalid directory index");
                  else
                     Get_Entry_Name (Directory, Index, Name, Last);

                     declare
                        use Rose.Invocation;
                        Index  : Storage_Offset := 0;
                     begin
                        for Ch of Name (1 .. Last) loop
                           exit when Index >= Params.Buffer_Length;
                           Index := Index + 1;
                           Buffer (Index) := Character'Pos (Ch);
                        end loop;

                        Reply := (others => <>);
                        Reply.Control.Flags (Rose.Invocation.Reply) := True;
                        Reply.Control.Flags (Send_Words) := True;
                        Reply.Control.Last_Sent_Word := 0;
                        Reply.Cap := Params.Reply_Cap;
                        Reply.Data (0) := Rose.Words.Word (Index);
                        Rose.System_Calls.Invoke_Capability (Reply);
                     end;
                  end if;
               end;

            when Rose.Interfaces.Directory.Directory_Entry_Kind_Endpoint =>
               declare
                  use IsoFS.Directories;
                  Directory : constant Directory_Type :=
                                Get_Identified_Directory
                                  (Params.Identifier);
                  Index     : constant Natural := Natural (Params.Data (0));
               begin
                  if Directory = No_Directory then
                     Rose.Console_IO.Put_Line
                       ("cap does not resolve to a directory");
                  elsif Index not in 1 .. Get_Entry_Count (Directory) then
                     Rose.Console_IO.Put_Line
                       ("invalid directory index");
                  else
                     declare
                        use Rose.Interfaces.Directory;
                        Kind : constant File_Kind :=
                                 Get_Entry_Kind (Directory, Index);
                     begin
                        Reply := (others => <>);
                        Reply.Control.Flags (Rose.Invocation.Reply) := True;
                        Reply.Control.Flags (Rose.Invocation.Send_Words) :=
                          True;
                        Reply.Control.Last_Sent_Word := 0;
                        Reply.Cap := Params.Reply_Cap;
                        Reply.Data (0) := File_Kind'Pos (Kind);
                        Rose.System_Calls.Invoke_Capability (Reply);
                     end;
                  end if;
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
