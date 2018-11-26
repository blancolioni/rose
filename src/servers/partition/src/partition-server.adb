with Rose.Console_IO;
with Rose.Invocation;
with Rose.System_Calls.Server;

with Rose.Interfaces.Storage;

package body Store.Server is

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Storage.Add_Backing_Store_Endpoint);
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Storage.Reserve_Storage_Endpoint);
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
      Rose.Console_IO.Put_Line ("storage: starting server");
      loop
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);

         case Params.Endpoint is
            when Rose.Interfaces.Storage.Add_Backing_Store_Endpoint =>
               null;
            when Rose.Interfaces.Storage.Reserve_Storage_Endpoint =>
               null;
            when others =>
               Rose.Console_IO.Put_Line
                 ("storage: bad endpoint");
         end case;
         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);
         Rose.System_Calls.Invoke_Capability (Reply);
      end loop;
   end Start_Server;

end Store.Server;
