with Rose.System_Calls;

package body Rose.Interfaces.Kernel_Process.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : Kernel_Process_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Destroy);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Destroy;

   -----------
   -- Fault --
   -----------

   procedure Fault (Item : Kernel_Process_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Fault);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Fault;

   -------------------
   -- Get_Fault_Cap --
   -------------------

   function Get_Fault_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Fault);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   --------------------
   -- Get_Notify_Cap --
   --------------------

   function Get_Notify_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Notify);

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id (Item : Kernel_Process_Client)
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

   --------------------
   -- Get_Resume_Cap --
   --------------------

   function Get_Resume_Cap (Item : Kernel_Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Resume);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   ------------
   -- Notify --
   ------------

   procedure Notify
     (Item  : Kernel_Process_Client;
      Flags : Rose.Words.Word_32)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Notify);
      --  Flags in scalar;
      Rose.System_Calls.Send_Word (Params, Flags);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Notify;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Kernel_Process_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 5);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Destroy := Params.Caps (0);
      Client.Get_Object_Id := Params.Caps (1);
      Client.Resume := Params.Caps (2);
      Client.Fault := Params.Caps (3);
      Client.Notify := Params.Caps (4);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client        :    out Kernel_Process_Client;
      Destroy       : in     Rose.Capabilities.Capability;
      Get_Object_Id : in     Rose.Capabilities.Capability;
      Resume        : in     Rose.Capabilities.Capability;
      Fault         : in     Rose.Capabilities.Capability;
      Notify        : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Destroy := Destroy;
      Client.Get_Object_Id := Get_Object_Id;
      Client.Resume := Resume;
      Client.Fault := Fault;
      Client.Notify := Notify;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ------------
   -- Resume --
   ------------

   procedure Resume (Item : Kernel_Process_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Resume);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Resume;

end Rose.Interfaces.Kernel_Process.Client;
