with Rose.System_Calls;

package body Rose.Interfaces.File_System.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : File_System_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   ----------------------------
   -- Get_Root_Directory_Cap --
   ----------------------------

   function Get_Root_Directory_Cap (Item : File_System_Client)
      return Rose.Capabilities.Capability
   is (Item.Root_Directory);

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
     (Client        :    out File_System_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Root_Directory := Params.Caps (0);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client         :    out File_System_Client;
      Root_Directory : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Root_Directory := Root_Directory;
      Client.Is_Open := True;
   end Open_Cap_Set;

   --------------------
   -- Root_Directory --
   --------------------

   function Root_Directory (Item : File_System_Client)
      return Rose.Interfaces.Directory.Client.Directory_Client is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Root_Directory);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Directory.Client.Directory_Client;
      begin
         Rose.Interfaces.Directory.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Root_Directory;

end Rose.Interfaces.File_System.Client;
