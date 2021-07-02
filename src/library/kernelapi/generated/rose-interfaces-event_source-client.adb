with Rose.System_Calls;

package body Rose.Interfaces.Event_Source.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ------------------
   -- Add_Listener --
   ------------------

   procedure Add_Listener
     (Item     : Event_Source_Client;
      Listener : Rose.Interfaces.Event_Listener.Client.Event_Listener_Client)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Add_Listener);
      --  Listener in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Event_Listener.Client.Get_Interface_Cap (Listener));
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Add_Listener;

   --------------------------
   -- Get_Add_Listener_Cap --
   --------------------------

   function Get_Add_Listener_Cap (Item : Event_Source_Client)
      return Rose.Capabilities.Capability
   is (Item.Add_Listener);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Event_Source_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Event_Source_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Add_Listener := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client       :    out Event_Source_Client;
      Add_Listener : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Add_Listener := Add_Listener;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Event_Source.Client;
