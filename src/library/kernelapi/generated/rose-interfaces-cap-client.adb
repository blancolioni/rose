with Rose.System_Calls;

package body Rose.Interfaces.Cap.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : Cap_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Destroy);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Destroy;

   ---------------------
   -- Get_Destroy_Cap --
   ---------------------

   function Get_Destroy_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability
   is (Item.Destroy);

   ---------------------------
   -- Get_Get_Object_Id_Cap --
   ---------------------------

   function Get_Get_Object_Id_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Object_Id);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Cap_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id (Item : Cap_Client)
      return Rose.Objects.Object_Id is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Object_Id);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Objects.Object_Id;
      begin
         Result := Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64
            (Params, 0));
         return Result;
      end;
   end Get_Object_Id;

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
     (Client        :    out Cap_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Destroy := Params.Caps (0);
      Client.Get_Object_Id := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client        :    out Cap_Client;
      Destroy       : in     Rose.Capabilities.Capability;
      Get_Object_Id : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Destroy := Destroy;
      Client.Get_Object_Id := Get_Object_Id;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Cap.Client;
