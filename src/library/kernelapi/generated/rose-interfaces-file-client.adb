with Rose.System_Calls;

package body Rose.Interfaces.File.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : File_Client)
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

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out File_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Read := Params.Caps (0);
      Client.Write := Params.Caps (1);
      Client.Seek := Params.Caps (2);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client :    out File_Client;
      Read   : in     Rose.Capabilities.Capability;
      Write  : in     Rose.Capabilities.Capability;
      Seek   : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Read := Read;
      Client.Write := Write;
      Client.Seek := Seek;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ----------
   -- Read --
   ----------

   procedure Read
     (Item   : in     File_Client;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Read);
      --  Buffer out composite;
      --  Last out scalar;
      Rose.System_Calls.Receive_Buffer (Params, Buffer'Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Last := System.Storage_Elements.Storage_Count
         (Rose.System_Calls.Get_Word_32 (Params, 0));
      Rose.System_Calls.Copy_Received_Buffer (Buffer'Length, Buffer'Address);
   end Read;

   ----------
   -- Seek --
   ----------

   procedure Seek
     (Item              : File_Client;
      Offset_From_Start : Rose.Words.Word_64)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Seek);
      --  Offset_From_Start in scalar;
      Rose.System_Calls.Send_Word (Params, Offset_From_Start);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Seek;

   -----------
   -- Write --
   -----------

   procedure Write
     (Item   : File_Client;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Write);
      --  Buffer in composite;
      Rose.System_Calls.Send_Storage_Array
        (Params,
         Buffer,
         False);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Write;

end Rose.Interfaces.File.Client;
