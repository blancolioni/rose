with System.Storage_Elements;

with Rose.Words;

with Rose.Invocation;

with Rose.Interfaces.Event_Listener;
with Rose.Interfaces.Stream_Reader;

with Rose.System_Calls.Server;

with Rose.Console_IO;

package body Event_Input_Stream.Server is

   --  Cooked_Mode  : constant Boolean := False;

   Input_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   Input_Last   : System.Storage_Elements.Storage_Count := 0;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Receive_Cap     : constant Rose.Capabilities.Capability :=
                          Rose.System_Calls.Server.Create_Receive_Cap
                            (Create_Endpoint_Cap);
      Read_Cap        : constant Rose.Capabilities.Capability :=
                          Rose.System_Calls.Server.Create_Endpoint
                            (Create_Endpoint_Cap,
                             Rose.Interfaces.Stream_Reader.Read_Endpoint);
      Params          : aliased Rose.Invocation.Invocation_Record;
      Reply           : aliased Rose.Invocation.Invocation_Record;
      Blocked_Request : Boolean := False;
      Waiting_Request : Rose.Capabilities.Capability := 0;
   begin

      Rose.Console_IO.Open (Console_Cap);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Get_Interface_Endpoint);

      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Event_Listener.On_Event_Endpoint);

      loop
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Receive_Words (Params, 1);

         Rose.System_Calls.Invoke_Capability (Params);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               Rose.System_Calls.Send_Cap (Reply, Read_Cap);

            when Rose.Interfaces.Event_Listener.On_Event_Endpoint =>
               declare
                  use type Rose.Words.Word;
                  use System.Storage_Elements;
                  Code : constant Rose.Words.Word := Params.Data (0);
               begin
                  if Input_Last < Input_Buffer'Last then
                     Input_Last := Input_Last + 1;
                     Input_Buffer (Input_Last) :=
                       Storage_Element (Code mod 2 ** System.Storage_Unit);
                  end if;
               end;

               if Blocked_Request then
                  Rose.System_Calls.Initialize_Reply (Reply, Waiting_Request);
                  Rose.System_Calls.Send_Storage_Array
                    (Params, Input_Buffer (1 .. Input_Last), False);
                  Rose.System_Calls.Invoke_Capability (Params);
                  Input_Last := 0;
                  Blocked_Request := False;
               end if;

            when Rose.Interfaces.Stream_Reader.Read_Endpoint =>
               declare
                  use System.Storage_Elements;
               begin
                  if Input_Last > 0 then
                     Rose.System_Calls.Initialize_Reply
                       (Reply, Params.Reply_Cap);
                     Rose.System_Calls.Send_Storage_Array
                       (Params, Input_Buffer (1 .. Input_Last), False);
                     Rose.System_Calls.Invoke_Capability (Params);
                     Input_Last := 0;
                  else
                     Blocked_Request := True;
                     Waiting_Request := Params.Reply_Cap;
                  end if;
               end;

            when others =>
               Rose.Console_IO.Put
                 ("event-stream: unknown endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
               Rose.Console_IO.New_Line;

         end case;

      end loop;

   end Start_Server;

end Event_Input_Stream.Server;
