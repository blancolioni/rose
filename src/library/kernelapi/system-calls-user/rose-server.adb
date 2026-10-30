with Rose.Capabilities.Standard;
with Rose.Interfaces.Process;
with Rose.System_Calls.Server;

package body Rose.Server is

   ---------------------------
   -- Block_Current_Request --
   ---------------------------

   procedure Block_Current_Request
     (Context : in out Server_Context)
   is
   begin
      Context.Block_Reply := True;
   end Block_Current_Request;

   -------------------------------
   -- Create_Anonymous_Endpoint --
   -------------------------------

   procedure Create_Anonymous_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id)
   is
   begin
      Rose.System_Calls.Server.Create_Anonymous_Endpoint
        (Rose.System_Calls.Create_Endpoint_Capability, Endpoint_Id);
   end Create_Anonymous_Endpoint;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability
   is
   begin
      return Rose.System_Calls.Server.Create_Endpoint
        (Rose.System_Calls.Create_Endpoint_Capability,
         Endpoint_Id, Identifier);
   end Create_Endpoint;

   ----------------------
   -- Get_Instance_Cap --
   ----------------------

   function Get_Instance_Cap
     (Instance   : Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use Instance_Maps;
      use type Rose.Objects.Endpoint_Id;
      Position : constant Cursor := Instance.Instances.Find (Identifier);
   begin
      if Has_Element (Position) then
         for Item of
           Instance.Instances.Constant_Reference (Position).Element.all
         loop
            if Item.Endpoint = Endpoint then
               return Item.Capability;
            end if;
         end loop;
      end if;
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
      use Instance_Maps;
      use type Rose.Objects.Endpoint_Id;
      Position : constant Cursor := Instance.Instances.Find (Identifier);
   begin
      if Has_Element (Position) then
         for Item of
           Instance.Instances.Constant_Reference (Position).Element.all
         loop
            if Item.Endpoint = Endpoint then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Has_Instance;

   ---------------------
   -- Hash_Identifier --
   ---------------------

   function Hash_Identifier
     (Identifier : Rose.Objects.Capability_Identifier)
      return Ada.Containers.Hash_Type
   is
   begin
      return Ada.Containers.Hash_Type (Identifier);
   end Hash_Identifier;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Interface_Cap : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Standard.Process_Interface_Cap);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Process.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);

      declare
         Publish_Cap : constant Rose.Capabilities.Capability :=
                         Params.Caps
                           (Rose.Interfaces.Process.Publish_Interface_Index);
      begin
         Rose.System_Calls.Initialize_Send (Params, Publish_Cap);
         Rose.System_Calls.Send_Cap (Params, Interface_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Publish_Interface;

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
           Rose.System_Calls.Server.Create_Receive_Cap
             (Rose.System_Calls.Create_Endpoint_Capability);
      end if;

      Rose.System_Calls.Initialize_Receive (Params, Context.Receive_Cap);
      Rose.System_Calls.Receive_Words
        (Params, Natural (Rose.Invocation.Parameter_Word_Index'Last) + 1);
      Rose.System_Calls.Receive_Caps
        (Params, Natural (Rose.Invocation.Capability_Index'Last) + 1);

      Rose.System_Calls.Expect_Buffer (Params);
      if Rose.System_Calls.Have_Buffer then
         Rose.System_Calls.Receive_Buffer (Params);
      end if;

      Rose.System_Calls.Invoke_Capability (Params);

      declare
         Handled : Boolean := False;
      begin
         for I in 1 .. Context.Endpoint_Count loop
            declare
               use type Rose.Objects.Endpoint_Id;
               Rec : Endpoint_Record renames Context.Endpoints (I);
               Saved_Params : constant Rose.Invocation.Invocation_Record :=
                                Params;
            begin
               if Rec.Endpoint = Params.Endpoint then
                  if Rec.Handler /= null then
                     Context.Block_Reply := False;
                     Rec.Handler (Params);
                     if Context.Block_Reply then
                        Rec.Blocked := True;
                        Rec.Params := Saved_Params;
                     end if;
                  end if;
                  Handled := True;
                  exit;
               end if;
            end;
         end loop;

         if Handled then
            if not Context.Block_Reply
              and then Params.Control.Flags (Rose.Invocation.Reply)
              and then Params.Cap /= Rose.Capabilities.Null_Capability
            then
               Rose.System_Calls.Invoke_Capability (Params);
            end if;
         else
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
      use type Rose.Capabilities.Capability;
   begin
      --  Register the endpoint with the kernel now, even for an instanced
      --  interface whose caps are made later.  The kernel gives each
      --  endpoint the next free index, so this fixes the indices by the
      --  order in which the server attaches its interfaces, and not by
      --  the order its clients first use them.  Caps hold the index, and
      --  the caps that persistent processes hold are saved in
      --  checkpoints, so after a restore the server's endpoints have to
      --  be where they were.

      if Rose.System_Calls.Create_Endpoint_Capability
        /= Rose.Capabilities.Null_Capability
      then
         Create_Anonymous_Endpoint (Endpoint);
      end if;

      --  Checks are suppressed in the servers, so a full table would
      --  otherwise be written past its end. Servers cannot raise, so the
      --  endpoint is not handled.

      if Context.Endpoint_Count = Max_Handled_Endpoints then
         return;
      end if;

      Context.Endpoint_Count := Context.Endpoint_Count + 1;
      Context.Endpoints (Context.Endpoint_Count) :=
        Endpoint_Record'
          (Endpoint => Endpoint,
           Handler  => Handler,
           Blocked  => False,
           Params   => <>);
   end Register_Handler;

   ---------------------
   -- Remove_Instance --
   ---------------------

   procedure Remove_Instance
     (Instance   : in out Server_Instance;
      Identifier : Rose.Objects.Capability_Identifier)
   is
      use Instance_Maps;
      Position : Cursor := Instance.Instances.Find (Identifier);
   begin
      if Has_Element (Position) then
         for Item of
           Instance.Instances.Constant_Reference (Position).Element.all
         loop
            Rose.System_Calls.Delete_Cap (Item.Capability);
         end loop;
         Instance.Instances.Delete (Position);
      end if;
   end Remove_Instance;

   ----------------------
   -- Set_Instance_Cap --
   ----------------------

   procedure Set_Instance_Cap
     (Instance   : in out Server_Instance;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier;
      Cap        : Rose.Capabilities.Capability)
   is
      use Instance_Maps;
      Position : Cursor;
      Inserted : Boolean;
   begin
      --  A server asks Has_Instance first, so an endpoint that is already
      --  there is left as it is: the first cap set is the one found.

      if Has_Instance (Instance, Endpoint, Identifier) then
         return;
      end if;

      Instance.Instances.Insert
        (Identifier, Handler_Vectors.Empty_Vector, Position, Inserted);
      Instance.Instances.Reference (Position).Append
        (Instanced_Handler_Record'
           (Endpoint   => Endpoint,
            Capability => Cap));

   exception
      when Storage_Error =>
         --  The heap is full: as with a full table in a boot module, the
         --  caps of the new object are null. A server cannot do more.
         null;
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

   ----------------------
   -- Unblock_Endpoint --
   ----------------------

   procedure Unblock_Endpoint
     (Context  : in out Server_Context;
      Endpoint : Rose.Objects.Endpoint_Id)
   is
      use type Rose.Capabilities.Capability;
      Handled : Boolean := False;
      Params  : aliased Rose.Invocation.Invocation_Record;
   begin
      Context.Block_Reply := False;

      for I in 1 .. Context.Endpoint_Count loop
         declare
            use type Rose.Objects.Endpoint_Id;
            Rec : Endpoint_Record renames Context.Endpoints (I);
         begin
            if Rec.Endpoint = Endpoint then
               if not Rec.Blocked then
                  return;
               end if;

               if Rec.Handler /= null then
                  Params := Rec.Params;
                  Rec.Handler (Params);
                  if not Context.Block_Reply then
                     Rec.Blocked := False;
                  end if;
               end if;
               Handled := True;
               exit;
            end if;
         end;
      end loop;

      if Handled then
         if not Context.Block_Reply
           and then Params.Control.Flags (Rose.Invocation.Reply)
           and then Params.Cap /= Rose.Capabilities.Null_Capability
         then
            Rose.System_Calls.Invoke_Capability (Params);
         end if;
      else
         Rose.System_Calls.Initialize_Reply (Params, Params.Reply_Cap);
         Rose.System_Calls.Send_Error
           (Params, Rose.Invocation.Invalid_Endpoint);
      end if;
   end Unblock_Endpoint;

end Rose.Server;
