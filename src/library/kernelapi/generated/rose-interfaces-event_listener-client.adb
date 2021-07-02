with Rose.System_Calls;

package body Rose.Interfaces.Event_Listener.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Event_Listener_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   ----------------------
   -- Get_On_Event_Cap --
   ----------------------

   function Get_On_Event_Cap (Item : Event_Listener_Client)
      return Rose.Capabilities.Capability
   is (Item.On_Event);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   --------------
   -- On_Event --
   --------------

   procedure On_Event
     (Item : Event_Listener_Client;
      Code : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.On_Event);
      --  Code in scalar;
      Rose.System_Calls.Send_Word (Params, Code);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end On_Event;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Event_Listener_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.On_Event := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client   :    out Event_Listener_Client;
      On_Event : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.On_Event := On_Event;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Event_Listener.Client;
