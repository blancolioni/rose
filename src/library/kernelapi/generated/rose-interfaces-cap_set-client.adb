with Rose.System_Calls;

package body Rose.Interfaces.Cap_Set.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ------------
   -- Append --
   ------------

   procedure Append
     (Item : Cap_Set_Client;
      Caps : Rose.Capabilities.Capability_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Append);
      --  Caps in composite;
      for Cap of Caps loop
         Rose.System_Calls.Send_Cap (Params, Cap);
      end loop;
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Append;

   --------------------
   -- Get_Append_Cap --
   --------------------

   function Get_Append_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability
   is (Item.Append);

   -------------
   -- Get_Cap --
   -------------

   function Get_Cap
     (Item  : Cap_Set_Client;
      Index : Positive)
   return Rose.Capabilities.Capability
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Cap);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
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
   end Get_Cap;

   ---------------------
   -- Get_Get_Cap_Cap --
   ---------------------

   function Get_Get_Cap_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Cap);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Cap_Set_Client)
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
   -- Get_Length_Cap --
   --------------------

   function Get_Length_Cap (Item : Cap_Set_Client)
      return Rose.Capabilities.Capability
   is (Item.Length);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   ------------
   -- Length --
   ------------

   function Length (Item : Cap_Set_Client) return Natural is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Length);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Natural;
      begin
         Result := Natural (Rose.System_Calls.Get_Word_32 (Params, 0));
         return Result;
      end;
   end Length;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Cap_Set_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Append := Params.Caps (0);
      Client.Get_Cap := Params.Caps (1);
      Client.Length := Params.Caps (2);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client  :    out Cap_Set_Client;
      Append  : in     Rose.Capabilities.Capability;
      Get_Cap : in     Rose.Capabilities.Capability;
      Length  : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Append := Append;
      Client.Get_Cap := Get_Cap;
      Client.Length := Length;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Cap_Set.Client;
