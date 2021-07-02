with Rose.System_Calls;

package body Rose.Interfaces.Receiver.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Receiver_Client)
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
   -- Get_Send_Cap_Cap --
   ----------------------

   function Get_Send_Cap_Cap (Item : Receiver_Client)
      return Rose.Capabilities.Capability
   is (Item.Send_Cap);

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
     (Client        :    out Receiver_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Send_Cap := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client   :    out Receiver_Client;
      Send_Cap : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Send_Cap := Send_Cap;
      Client.Is_Open := True;
   end Open_Cap_Set;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (Item : Receiver_Client;
      Cap  : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Send_Cap);
      --  Cap in scalar;
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Send_Cap;

end Rose.Interfaces.Receiver.Client;
