with Rose.Words;

with Rose.Kernel.Processes;
with Rose.Kernel.Validation;

with Rose.Kernel.Debug;

with Rose.Boot.Console;
with Rose.Invocation.Trace;

package body Rose.Kernel.Capabilities.Endpoint is

   procedure Send_To_Endpoint
     (Sender     : Rose.Objects.Process_Id;
      Receiver   : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Index;
      Identifier : Rose.Objects.Capability_Identifier;
      Params     : Rose.Invocation.Invocation_Access);

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Kernel.Processes;
      Object_Id : constant Rose.Objects.Object_Id := Cap.Payload;
      Process_Id : constant Rose.Objects.Process_Id :=
                     (if Rose.Objects.Is_Process_Object_Id (Object_Id)
                      then Rose.Objects.To_Process_Id (Object_Id)
                      else Rose.Objects.Null_Process_Id);
      Endpoint_Index : constant Rose.Objects.Endpoint_Index :=
                         Cap.Header.Endpoint;
   begin

      if not Is_Valid_Process_Id (Process_Id) then
         Rose.Boot.Console.Put ("endpoint-cap: bad pid ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Process_Id));
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if not Is_Active_Process_Id (Process_Id) then
         Rose.Boot.Console.Put ("endpoint-cap: inactive pid ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Process_Id));
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if not Is_Valid_Endpoint_Index (Process_Id, Endpoint_Index) then
         Rose.Boot.Console.Put ("endpoint-cap: invalid endpoint ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Endpoint_Index));
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Endpoint);
         return;
      end if;

      if not Params.Control.Flags (Rose.Invocation.Send) then
         Rose.Boot.Console.Put ("endpoint-cap: send flag not set");
         Rose.Boot.Console.New_Line;
         Rose.Kernel.Debug.Put_Call
           ("bad-call", Process_Id, Cap, Params.all);
         Return_Error (Params, Rose.Invocation.Invalid_Operation);
         return;
      end if;

      if Params.Control.Flags (Rose.Invocation.Create_Reply_Cap) then
         Params.Reply_Cap := Create_Cap (Process_Id);

         if Rose.Capabilities.Is_Null (Params.Reply_Cap) then
            Rose.Boot.Console.Put (Rose.Kernel.Processes.Current_Process_Id);
            Rose.Boot.Console.Put (": out of caps while creating reply cap");
            Rose.Boot.Console.New_Line;
            Return_Error (Params, Rose.Invocation.Out_Of_Capabilities);
            return;
         end if;

         declare
            use Rose.Capabilities.Layout;
            Reply_Layout : constant Capability_Layout :=
                             Reply_Capability (Current_Process_Id);
         begin
            Set_Cap (Process_Id, Params.Reply_Cap, Reply_Layout);
            Rose.Kernel.Validation.Create_Cap
              (Process_Id, Params.Reply_Cap, Reply_Cap);
         end;
      end if;

      Send_To_Endpoint
        (Sender     => Rose.Kernel.Processes.Current_Process_Id,
         Receiver   => Process_Id,
         Endpoint   => Endpoint_Index,
         Identifier => Cap.Header.Identifier,
         Params     => Params);

   end Handle;

   ----------------------
   -- Send_To_Endpoint --
   ----------------------

   procedure Send_To_Endpoint
     (Sender     : Rose.Objects.Process_Id;
      Receiver   : Rose.Objects.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Index;
      Identifier : Rose.Objects.Capability_Identifier;
      Params     : Rose.Invocation.Invocation_Access)
   is
      Receiver_Blocked : constant Boolean :=
                           Rose.Kernel.Processes.Is_Blocked_On_Endpoint
                             (Process        => Receiver,
                              Endpoint_Index => Endpoint);
   begin
      if Receiver_Blocked then
         Rose.Kernel.Processes.Send_To_Endpoint
           (From_Process => Sender,
            To_Process   => Receiver,
            Sender_Cap   => Params.Cap,
            Endpoint     => Endpoint,
            Identifier   => Identifier,
            Params       => Params.all);
      else
         if Params.Control.Flags (Rose.Invocation.Block) then
            Rose.Boot.Console.Put (Sender);
            Rose.Boot.Console.Put (" -> ");
            Rose.Boot.Console.Put (Receiver);
            Rose.Boot.Console.Put (": blocked on endpoint ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Endpoint));
            Rose.Boot.Console.New_Line;
            Rose.Invocation.Trace.Put
              (Params.all, True);
            Rose.Kernel.Processes.Wait_For_Receiver
              (Waiting_Process   => Sender,
               Receiving_Process => Receiver,
               Endpoint          => Endpoint,
               Identifier        => Identifier,
               Params            => Params.all);
         else
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Request_Would_Block);
         end if;
      end if;
   end Send_To_Endpoint;

end Rose.Kernel.Capabilities.Endpoint;
