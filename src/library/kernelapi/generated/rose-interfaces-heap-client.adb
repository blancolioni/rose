with Rose.System_Calls;

package body Rose.Interfaces.Heap.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -------------------
   -- Current_Bound --
   -------------------

   function Current_Bound (Item : Heap_Client) return Rose.Words.Word is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Current_Bound);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Words.Word;
      begin
         Result := Rose.System_Calls.Get_Word_32 (Params, 0);
         return Result;
      end;
   end Current_Bound;

   ---------------------------
   -- Get_Current_Bound_Cap --
   ---------------------------

   function Get_Current_Bound_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability
   is (Item.Current_Bound);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -------------------------------
   -- Get_Request_New_Bound_Cap --
   -------------------------------

   function Get_Request_New_Bound_Cap (Item : Heap_Client)
      return Rose.Capabilities.Capability
   is (Item.Request_New_Bound);

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
     (Client        :    out Heap_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Current_Bound := Params.Caps (0);
      Client.Request_New_Bound := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client            :    out Heap_Client;
      Current_Bound     : in     Rose.Capabilities.Capability;
      Request_New_Bound : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Current_Bound := Current_Bound;
      Client.Request_New_Bound := Request_New_Bound;
      Client.Is_Open := True;
   end Open_Cap_Set;

   -----------------------
   -- Request_New_Bound --
   -----------------------

   procedure Request_New_Bound
     (Item      : Heap_Client;
      New_Bound : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Request_New_Bound);
      --  New_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, New_Bound);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Request_New_Bound;

end Rose.Interfaces.Heap.Client;
