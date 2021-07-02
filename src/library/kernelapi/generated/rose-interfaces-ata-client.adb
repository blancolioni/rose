with Rose.System_Calls;

package body Rose.Interfaces.Ata.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ----------------
   -- Get_Device --
   ----------------

   function Get_Device (Item : Ata_Client)
      return Rose.Interfaces.Block_Device.Client.Block_Device_Client is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Device);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      begin
         Rose.Interfaces.Block_Device.Client.Open
           (Result,
            Params.Caps (0));
         return Result;
      end;
   end Get_Device;

   ------------------------
   -- Get_Get_Device_Cap --
   ------------------------

   function Get_Get_Device_Cap (Item : Ata_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Device);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Ata_Client)
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
     (Client        :    out Ata_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Get_Device := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client     :    out Ata_Client;
      Get_Device : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Get_Device := Get_Device;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Ata.Client;
