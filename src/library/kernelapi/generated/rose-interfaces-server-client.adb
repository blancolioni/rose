with Rose.System_Calls;

package body Rose.Interfaces.Server.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Server_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   ---------------------------------
   -- Get_Published_Interface_Cap --
   ---------------------------------

   function Get_Published_Interface_Cap (Item : Server_Client)
      return Rose.Capabilities.Capability
   is (Item.Published_Interface);

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
     (Client        :    out Server_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Published_Interface := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client              :    out Server_Client;
      Published_Interface : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Published_Interface := Published_Interface;
      Client.Is_Open := True;
   end Open_Cap_Set;

   -------------------------
   -- Published_Interface --
   -------------------------

   function Published_Interface (Item : Server_Client)
      return Rose.Capabilities.Capability is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Published_Interface);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Capabilities.Capability;
      begin
         Result := Rose.Capabilities.Capability
            (Rose.System_Calls.Get_Word_32 (Params, 0));
         return Result;
      end;
   end Published_Interface;

end Rose.Interfaces.Server.Client;
