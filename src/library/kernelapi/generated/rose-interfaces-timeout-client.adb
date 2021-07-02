with Rose.System_Calls;

package body Rose.Interfaces.Timeout.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Timeout_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   ------------------------
   -- Get_On_Timeout_Cap --
   ------------------------

   function Get_On_Timeout_Cap (Item : Timeout_Client)
      return Rose.Capabilities.Capability
   is (Item.On_Timeout);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout (Item : Timeout_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.On_Timeout);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end On_Timeout;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Timeout_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.On_Timeout := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client     :    out Timeout_Client;
      On_Timeout : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.On_Timeout := On_Timeout;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Timeout.Client;
