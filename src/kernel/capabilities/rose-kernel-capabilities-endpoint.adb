with Rose.Words;

with Rose.Kernel.Processes;

with Rose.Kernel.Debug;

with Rose.Boot.Console;

with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Endpoint is

   procedure Send_To_Endpoint
     (Sender     : Rose.Kernel.Processes.Process_Id;
      Receiver   : Rose.Kernel.Processes.Process_Id;
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
      Oid : constant Rose.Objects.Object_Id := Cap.Payload;
      Pid : constant Rose.Kernel.Processes.Process_Id :=
              (if Rose.Objects.Is_Process_Object_Id (Oid)
               then Rose.Kernel.Processes.To_Process_Id (Oid)
               else Rose.Kernel.Processes.Null_Process_Id);
      Endpoint_Index : constant Rose.Objects.Endpoint_Index :=
                         Cap.Header.Endpoint;
   begin

      if Pid = Null_Process_Id then
         Rose.Boot.Console.Put ("endpoint-cap: bad object id: ");
         Rose.Boot.Console.Put (Oid);
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if not Is_Valid_Process_Id (Pid) then
         Rose.Boot.Console.Put ("endpoint-cap: bad pid ");
         Rose.Kernel.Processes.Debug.Put (Pid);
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if not Is_Active_Process_Id (Pid) then
         Rose.Boot.Console.Put ("endpoint-cap: inactive pid ");
         Rose.Kernel.Processes.Debug.Put (Pid);
         Rose.Boot.Console.New_Line;
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if not Is_Valid_Endpoint_Index (Pid, Endpoint_Index) then
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
           ("bad-call", Cap, Params.all);
         Return_Error (Params, Rose.Invocation.Invalid_Operation);
         return;
      end if;

      if not Rose.Kernel.Processes.Is_Valid_Entry
        (Source_Cap          => Cap,
         Destination_Process => Pid)
      then
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
         return;
      end if;

      if Params.Control.Flags (Rose.Invocation.Create_Reply_Cap) then
         Params.Reply_Cap := Create_Cap (Pid);

         if Rose.Capabilities.Is_Null (Params.Reply_Cap) then
            Rose.Kernel.Processes.Debug.Put (Pid);
            Rose.Boot.Console.Put (": out of caps while creating reply cap");
            Rose.Boot.Console.New_Line;
            Return_Error (Params, Rose.Invocation.Out_Of_Capabilities);
            return;
         end if;

         declare
            use Rose.Capabilities.Layout;
            Reply_Layout : constant Capability_Layout :=
                             Reply_Capability
                               (To_Object_Id (Current_Process_Id));
         begin
            Set_Cap (Pid, Params.Reply_Cap, Reply_Layout);
         end;
      end if;

      Send_To_Endpoint
        (Sender     => Rose.Kernel.Processes.Current_Process_Id,
         Receiver   => Pid,
         Endpoint   => Endpoint_Index,
         Identifier => Cap.Header.Identifier,
         Params     => Params);

   end Handle;

   ----------------------
   -- Send_To_Endpoint --
   ----------------------

   procedure Send_To_Endpoint
     (Sender     : Rose.Kernel.Processes.Process_Id;
      Receiver   : Rose.Kernel.Processes.Process_Id;
      Endpoint   : Rose.Objects.Endpoint_Index;
      Identifier : Rose.Objects.Capability_Identifier;
      Params     : Rose.Invocation.Invocation_Access)
   is
      Receiver_Blocked : constant Boolean :=
                           Rose.Kernel.Processes.Is_Blocked_On_Endpoint
                             (Pid            => Receiver,
                              Endpoint_Index => Endpoint);
   begin
      if Receiver_Blocked then
         Rose.Kernel.Processes.Send_To_Endpoint
           (From_Process_Id => Sender,
            To_Process_Id   => Receiver,
            Sender_Cap      => Params.Cap,
            Endpoint        => Endpoint,
            Identifier      => Identifier,
            Params          => Params.all);
      else
         if Params.Control.Flags (Rose.Invocation.Block) then
            Rose.Kernel.Processes.Wait_For_Receiver
              (Waiting_Process_Id   => Sender,
               Receiving_Process_Id => Receiver,
               Endpoint             => Endpoint,
               Identifier           => Identifier,
               Params               => Params.all);
         else
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Request_Would_Block);
         end if;
      end if;
   end Send_To_Endpoint;

end Rose.Kernel.Capabilities.Endpoint;
