with Rose.Words;

with Rose.Console_IO;

with Rose.System_Calls.Server;

package body Rose.Server is

   ----------------------
   -- Get_Instance_Cap --
   ----------------------

   function Get_Instance_Cap
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      for I in 1 .. Instance.Instance_Count loop
         declare
            use Rose.Objects;
            Item : Instanced_Handler_Record renames Instance.Instances (I);
         begin
            if Item.Endpoint = Endpoint
              and then Item.Identifier = Identifier
            then
               return Item.Capability;
            end if;
         end;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Get_Instance_Cap;

   ------------------
   -- Has_Instance --
   ------------------

   function Has_Instance
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Boolean
   is
   begin
      for I in 1 .. Instance.Instance_Count loop
         declare
            use Rose.Objects;
            Item : Instanced_Handler_Record renames Instance.Instances (I);
         begin
            if Item.Endpoint = Endpoint
              and then Item.Identifier = Identifier
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Has_Instance;

   ---------------------
   -- Receive_Message --
   ---------------------

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
            if Params.Control.Flags (Rose.Invocation.Reply)
              and then Params.Cap /= Rose.Capabilities.Null_Capability
            then
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

   ----------------------
   -- Set_Instance_Cap --
   ----------------------

   procedure Set_Instance_Cap
     (Instance   : in out Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier;
      Cap        : Rose.Capabilities.Capability)
   is
   begin
      if Instance.Instance_Count < Max_Endpoint_Instances then
         Instance.Instance_Count :=
           Instance.Instance_Count + 1;
         Instance.Instances (Instance.Instance_Count) :=
           Instanced_Handler_Record'
             (Endpoint   => Endpoint,
              Identifier => Identifier,
              Capability => Cap);
      end if;
   end Set_Instance_Cap;

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
