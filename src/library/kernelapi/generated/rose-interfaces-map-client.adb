with Rose.System_Calls;

package body Rose.Interfaces.Map.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ---------
   -- Add --
   ---------

   procedure Add
     (Item : Map_Client;
      Name : String;
      Cap  : Rose.Interfaces.Launch.Client.Launch_Client)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Add);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      --  Cap in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Launch.Client.Get_Interface_Cap (Cap));
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Add;

   ----------
   -- Find --
   ----------

   function Find
     (Item : Map_Client;
      Name : String)
   return Rose.Interfaces.Launch.Client.Launch_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Find);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Launch.Client.Launch_Client;
      begin
         Rose.Interfaces.Launch.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Find;

   -----------------
   -- Get_Add_Cap --
   -----------------

   function Get_Add_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability
   is (Item.Add);

   ------------------
   -- Get_Find_Cap --
   ------------------

   function Get_Find_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability
   is (Item.Find);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Map_Client)
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
   -- Get_Remove_Cap --
   --------------------

   function Get_Remove_Cap (Item : Map_Client)
      return Rose.Capabilities.Capability
   is (Item.Remove);

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
     (Client        :    out Map_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Add := Params.Caps (0);
      Client.Remove := Params.Caps (1);
      Client.Find := Params.Caps (2);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client :    out Map_Client;
      Add    : in     Rose.Capabilities.Capability;
      Remove : in     Rose.Capabilities.Capability;
      Find   : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Add := Add;
      Client.Remove := Remove;
      Client.Find := Find;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Item : Map_Client;
      Name : String)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Remove);
      --  Name in composite;
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Remove;

end Rose.Interfaces.Map.Client;
