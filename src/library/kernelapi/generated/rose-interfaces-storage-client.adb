with Rose.System_Calls;

package body Rose.Interfaces.Storage.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Add_Backing_Store --
   -----------------------

   procedure Add_Backing_Store
     (Item  : Storage_Client;
      Store : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Add_Backing_Store);
      --  Store in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Block_Device.Client.Get_Interface_Cap (Store));
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Add_Backing_Store;

   -------------------------------
   -- Get_Add_Backing_Store_Cap --
   -------------------------------

   function Get_Add_Backing_Store_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability
   is (Item.Add_Backing_Store);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -----------------------------
   -- Get_Reserve_Storage_Cap --
   -----------------------------

   function Get_Reserve_Storage_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability
   is (Item.Reserve_Storage);

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
     (Client        :    out Storage_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Reserve_Storage := Params.Caps (0);
      Client.Add_Backing_Store := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client            :    out Storage_Client;
      Reserve_Storage   : in     Rose.Capabilities.Capability;
      Add_Backing_Store : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Reserve_Storage := Reserve_Storage;
      Client.Add_Backing_Store := Add_Backing_Store;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Item : Storage_Client;
      Size : Rose.Words.Word_64)
   return Rose.Interfaces.Region.Client.Region_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Reserve_Storage);
      --  Size in scalar;
      Rose.System_Calls.Send_Word (Params, Size);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Region.Client.Region_Client;
      begin
         Rose.Interfaces.Region.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Reserve_Storage;

end Rose.Interfaces.Storage.Client;
