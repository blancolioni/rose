with Rose.System_Calls;

package body Rose.Interfaces.Interrupt_Handler.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ------------------------------
   -- Get_Handle_Interrupt_Cap --
   ------------------------------

   function Get_Handle_Interrupt_Cap (Item : Interrupt_Handler_Client)
      return Rose.Capabilities.Capability
   is (Item.Handle_Interrupt);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Interrupt_Handler_Client)
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
   -- Handle_Interrupt --
   ----------------------

   procedure Handle_Interrupt
     (Item : Interrupt_Handler_Client;
      Code : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Handle_Interrupt);
      --  Code in scalar;
      Rose.System_Calls.Send_Word (Params, Code);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Handle_Interrupt;

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
     (Client        :    out Interrupt_Handler_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Handle_Interrupt := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client           :    out Interrupt_Handler_Client;
      Handle_Interrupt : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Handle_Interrupt := Handle_Interrupt;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Interrupt_Handler.Client;
