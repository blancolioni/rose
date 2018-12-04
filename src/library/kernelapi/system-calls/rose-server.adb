with Rose.Words;

with Rose.Console_IO;

with Rose.System_Calls.Server;

package body Rose.Server is

   procedure Receive_Message
     (Context : in out Server_Context)
   is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Context.Receive_Cap = Rose.Capabilities.Null_Capability then
         Context.Receive_Cap :=
           Rose.System_Calls.Server.Create_Receive_Cap (1);
      end if;

      Rose.System_Calls.Initialize_Receive (Params, Context.Receive_Cap);
      Rose.System_Calls.Receive_Words
        (Params, Natural (Rose.Invocation.Parameter_Word_Index'Last) + 1);
      Rose.System_Calls.Receive_Caps
        (Params, Natural (Rose.Invocation.Capability_Index'Last) + 1);
      Params.Control.Flags (Rose.Invocation.Recv_Buffer) := True;
      Rose.System_Calls.Invoke_Capability (Params);

      declare
         Handled : Boolean := False;
      begin
         for I in 1 .. Context.Endpoint_Count loop
            declare
               use type Rose.Objects.Endpoint_Id;
               Rec : Endpoint_Record renames Context.Endpoints (I);
            begin
               if Rec.Endpoint = Params.Endpoint then
                  if Rec.Handler /= null then
                     Rec.Handler (Params);
                  end if;
                  Handled := True;
                  exit;
               end if;
            end;
         end loop;

         if Handled then
            if Params.Control.Flags (Rose.Invocation.Reply) then
               Rose.System_Calls.Invoke_Capability (Params);
            end if;
         else
            Rose.Console_IO.Put ("server: unknown endpoint: ");
            Rose.Console_IO.Put (Rose.Words.Word_64 (Params.Endpoint));
            Rose.Console_IO.New_Line;
            Rose.System_Calls.Initialize_Reply (Params, Params.Reply_Cap);
            Rose.System_Calls.Send_Error
              (Params, Rose.Invocation.Invalid_Endpoint);
         end if;
      end;

   end Receive_Message;

   ----------------------
   -- Register_Handler --
   ----------------------

   procedure Register_Handler
     (Context  : in out Server_Context;
      Endpoint : Rose.Objects.Endpoint_Id;
      Handler  : Invocation_Handler)
   is
   begin
      Context.Endpoint_Count := Context.Endpoint_Count + 1;
      Context.Endpoints (Context.Endpoint_Count) :=
        Endpoint_Record'
          (Endpoint => Endpoint,
           Handler  => Handler);
   end Register_Handler;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server
     (Context : in out Server_Context)
   is
   begin
      loop
         Receive_Message (Context);
      end loop;
   end Start_Server;

end Rose.Server;
