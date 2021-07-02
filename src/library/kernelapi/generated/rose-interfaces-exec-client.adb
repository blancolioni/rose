with Rose.System_Calls;

package body Rose.Interfaces.Exec.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ---------------------
   -- Get_Install_Cap --
   ---------------------

   function Get_Install_Cap (Item : Exec_Client)
      return Rose.Capabilities.Capability
   is (Item.Install);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Exec_Client)
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

   -------------
   -- Install --
   -------------

   function Install
     (Item         : Exec_Client;
      Executable   :
        Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Install_Caps : Rose.Capabilities.Capability_Array)
   return Rose.Interfaces.Launch.Client.Launch_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Install);
      --  Executable in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Stream_Reader.Client.Get_Interface_Cap
         (Executable));
      --  Install_Caps in composite;
      for Cap of Install_Caps loop
         Rose.System_Calls.Send_Cap (Params, Cap);
      end loop;
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
   end Install;

   ------------
   -- Launch --
   ------------

   function Launch
     (Item        : Exec_Client;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : System.Storage_Elements.Storage_Array)
   return Rose.Interfaces.Process.Client.Process_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Launch);
      --  Caps in composite;
      for Cap of Caps loop
         Rose.System_Calls.Send_Cap (Params, Cap);
      end loop;
      --  Environment in composite;
      Rose.System_Calls.Send_Storage_Array
        (Params,
         Environment,
         False);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Process.Client.Process_Client;
      begin
         Rose.Interfaces.Process.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Launch;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Exec_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Launch := Params.Caps (0);
      Client.Install := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client  :    out Exec_Client;
      Launch  : in     Rose.Capabilities.Capability;
      Install : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Launch := Launch;
      Client.Install := Install;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Exec.Client;
