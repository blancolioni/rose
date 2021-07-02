with Rose.System_Calls;

package body Rose.Interfaces.Timer.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Timer_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -----------------------
   -- Get_Set_Timer_Cap --
   -----------------------

   function Get_Set_Timer_Cap (Item : Timer_Client)
      return Rose.Capabilities.Capability
   is (Item.Set_Timer);

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
     (Client        :    out Timer_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Set_Timer := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client    :    out Timer_Client;
      Set_Timer : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Set_Timer := Set_Timer;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ---------------
   -- Set_Timer --
   ---------------

   function Set_Timer
     (Item         : Timer_Client;
      Milliseconds : Rose.Words.Word;
      Cap          : Rose.Capabilities.Capability)
   return Rose.Interfaces.Cap.Client.Cap_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Set_Timer);
      --  Milliseconds in scalar;
      Rose.System_Calls.Send_Word (Params, Milliseconds);
      --  Cap in scalar;
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Cap.Client.Cap_Client;
      begin
         Rose.Interfaces.Cap.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Set_Timer;

end Rose.Interfaces.Timer.Client;
