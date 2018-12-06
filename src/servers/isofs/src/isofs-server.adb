with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Directory;
with Rose.Interfaces.File_System;
with Rose.Interfaces.Stream_Reader;
with Rose.Console_IO;

with IsoFS.Directories;

package body IsoFS.Server is

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Device      : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Endpoint_Cap);
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.Interfaces.Block_Device.Client.Open
        (Device, Device_Cap);

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
         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.File_System.Root_Directory_Endpoint =>
               declare
                  Root : constant IsoFS.Directories.Directory_Type :=
                           IsoFS.Directories.Get_Root_Directory
                             (Device);
               begin
                  IsoFS.Directories.Send_Directory_Caps (Root, Reply);
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
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     Rose.System_Calls.Send_Word
                       (Reply, Rose.Words.Word (Get_Entry_Count (Directory)));
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
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  elsif Index = 0 then
                     Rose.Console_IO.Put_Line
                       ("invalid directory index");
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     Get_Entry_Name (Directory, Index, Name, Last);

                     declare
                        Index  : Storage_Offset := 0;
                     begin
                        for Ch of Name (1 .. Last) loop
                           exit when Index >= Params.Buffer_Length;
                           Index := Index + 1;
                           Buffer (Index) := Character'Pos (Ch);
                        end loop;

                        Rose.System_Calls.Send_Word
                          (Reply, Rose.Words.Word (Index));
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
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  elsif Index not in 1 .. Get_Entry_Count (Directory) then
                     Rose.Console_IO.Put_Line
                       ("invalid directory index");
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     declare
                        use Rose.Interfaces.Directory;
                        Kind : constant File_Kind :=
                                 Get_Entry_Kind (Directory, Index);
                     begin
                        Rose.System_Calls.Send_Word
                          (Reply, Rose.Words.Word'(File_Kind'Pos (Kind)));
                     end;
                  end if;
               end;

            when Rose.Interfaces.Directory.Get_Directory_Endpoint =>
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
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  elsif Index not in 1 .. Get_Entry_Count (Directory) then
                     Rose.Console_IO.Put
                       ("invalid directory index: ");
                     Rose.Console_IO.Put (Natural (Params.Identifier));
                     Rose.Console_IO.Put ("/");
                     Rose.Console_IO.Put (Index);
                     Rose.Console_IO.New_Line;
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     IsoFS.Directories.Send_Directory_Caps
                       (Get_Child_Directory (Directory, Index), Reply);
                  end if;
               end;

            when Rose.Interfaces.Directory.Find_Entry_Endpoint =>
               declare
                  use IsoFS.Directories;
                  Directory : constant Directory_Type :=
                                Get_Identified_Directory
                                  (Params.Identifier);
                  Name      : String (1 .. Natural (Params.Buffer_Length));
                  pragma Import (Ada, Name);
                  for Name'Address use Params.Buffer_Address;
               begin

                  if Directory = No_Directory then
                     Rose.Console_IO.Put_Line
                       ("cap does not resolve to a directory");
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  elsif not Params.Control.Flags
                    (Rose.Invocation.Send_Buffer)
                  then
                     Rose.Console_IO.Put_Line
                       ("expected a name in call to find_entry");
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     declare
                        Index : constant Natural :=
                                  Get_Index_By_Name (Directory, Name);
                     begin
                        Rose.System_Calls.Send_Word
                          (Reply, Rose.Words.Word (Index));
                     end;
                  end if;
               end;

            when Rose.Interfaces.Directory.Read_File_Endpoint =>
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
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  elsif Index not in 1 .. Get_Entry_Count (Directory) then
                     Rose.Console_IO.Put
                       ("invalid directory index: ");
                     Rose.Console_IO.Put (Natural (Params.Identifier));
                     Rose.Console_IO.Put ("/");
                     Rose.Console_IO.Put (Index);
                     Rose.Console_IO.New_Line;
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Invalid_Operation);
                  else
                     Rose.System_Calls.Send_Cap
                       (Reply,
                        IsoFS.Directories.Read_File (Directory, Index));
                  end if;
               end;

            when Rose.Interfaces.Stream_Reader.Read_Endpoint =>

               declare
                  use System.Storage_Elements;
                  Buffer : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
                  Last   : Storage_Count;
               begin
                  IsoFS.Directories.Read
                    (Positive (Params.Identifier), Buffer, Last);
                  Rose.System_Calls.Send_Word (Reply, Natural (Last));
               end;

            when others =>
               Rose.Console_IO.Put
                 ("unknown endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
               Rose.Console_IO.New_Line;
               Rose.System_Calls.Send_Error
                 (Reply, Rose.Invocation.Invalid_Endpoint);

         end case;

         Rose.System_Calls.Invoke_Capability (Reply);

      end loop;
   end Start_Server;

end IsoFS.Server;
