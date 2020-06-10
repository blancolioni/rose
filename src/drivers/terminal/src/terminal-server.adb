with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Rose.System_Calls.Server;

with Rose.Interfaces.Stream_Writer.Client;
with Rose.Invocation;

package body Terminal.Server is

   Out_Stream : Rose.Interfaces.Stream_Writer.Client.Stream_Writer_Client;

   Write_Cap  : Rose.Capabilities.Capability;

   Sink_Interface : Rose.Capabilities.Capability;
   Sink_Cap       : Rose.Capabilities.Capability;

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
     with Unreferenced;

   procedure On_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   procedure On_Source_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   -------------
   -- On_Read --
   -------------

   procedure On_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Id);
   begin
      Buffer := (others => 0);
      Last   := 0;
   end On_Read;

   ---------------------
   -- On_Source_Write --
   ---------------------

   procedure On_Source_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      S : String (1 .. Natural (Buffer'Length));
      pragma Import (Ada, S);
      for S'Address use Buffer'Address;
   begin
      Rose.Console_IO.Put (S);
      Rose.Console_IO.Flush;
   end On_Source_Write;

   --------------
   -- On_Write --
   --------------

   procedure On_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      S : String (1 .. Natural (Buffer'Length));
      pragma Import (Ada, S);
      for S'Address use Buffer'Address;
   begin
      Rose.Console_IO.Put (S);
   end On_Write;

   --------------------
   -- Start_Terminal --
   --------------------

   procedure Start_Terminal is
      use type Rose.Objects.Capability_Identifier;
      Params     : aliased Rose.Invocation.Invocation_Record;
      Reply      : aliased Rose.Invocation.Invocation_Record;
      Send_Reply : Boolean;
      Receive_Cap : constant Rose.Capabilities.Capability :=
        Rose.System_Calls.Server.Create_Receive_Cap
          (Create_Endpoint_Cap);
      Writer_Endpoint : constant Rose.Objects.Endpoint_Id :=
        Rose.Interfaces.Stream_Writer.Stream_Writer_Interface;
   begin
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Create_Endpoint_Cap,
         Rose.Interfaces.Get_Interface_Endpoint);

      Rose.Interfaces.Stream_Writer.Client.Open
        (Client        => Out_Stream,
         Interface_Cap => Output_Device_Cap);

      Write_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap   => Create_Endpoint_Cap,
           Endpoint_Id  => Writer_Endpoint,
           Identifier   => 1);

      Sink_Interface :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap   => Create_Endpoint_Cap,
           Endpoint_Id  =>
             Rose.Interfaces.Get_Interface_Endpoint,
           Identifier   => 2);

      Sink_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Cap   => Create_Endpoint_Cap,
           Endpoint_Id  => Writer_Endpoint,
           Identifier   => 2);

      Params := (others => <>);
      Rose.System_Calls.Initialize_Send (Params, Input_Device_Cap);
      Rose.System_Calls.Send_Cap (Params, Sink_Interface);
      Rose.System_Calls.Invoke_Capability (Params);

      loop
         Send_Reply := True;
         Params := (others => <>);
         Params.Control.Flags (Rose.Invocation.Receive) := True;
         Params.Control.Flags (Rose.Invocation.Block) := True;
         Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
         Params.Control.Last_Recv_Word :=
           Rose.Invocation.Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               if Params.Identifier = 1 then
                  Rose.System_Calls.Send_Cap (Reply, Write_Cap);
               elsif Params.Identifier = 2 then
                  Rose.System_Calls.Send_Cap (Reply, Sink_Cap);
               else
                  Send_Reply := False;
               end if;

            when Writer_Endpoint =>
               declare
                  use System.Storage_Elements;
                  Buffer        : Storage_Array (1 .. Params.Buffer_Length);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
               begin
                  if Params.Identifier = 1 then
                     On_Write (Params.Identifier, Buffer);
                  else
                     On_Source_Write (Params.Identifier, Buffer);
                  end if;
                  Send_Reply := True;
               end;

            when others =>
               Rose.Console_IO.Put
                 ("terminal: unknown endpoint: ");
               Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
               Rose.Console_IO.New_Line;
               Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         end case;

         if Send_Reply then
            Rose.System_Calls.Invoke_Capability (Reply);
         end if;

      end loop;

   end Start_Terminal;

end Terminal.Server;
