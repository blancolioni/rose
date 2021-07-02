with Rose.System_Calls;

package body Rose.Interfaces.Block_Device.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ----------------------------
   -- Get_Get_Parameters_Cap --
   ----------------------------

   function Get_Get_Parameters_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Parameters);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Block_Device_Client)
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
   -- Get_Parameters --
   --------------------

   procedure Get_Parameters
     (Item        : in     Block_Device_Client;
      Block_Count :    out Block_Address_Type;
      Block_Size  :    out Rose.Interfaces.Block_Device.Block_Size_Type)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Parameters);
      --  Block_Count out scalar;
      --  Block_Size out scalar;
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Block_Count := Block_Address_Type (Rose.System_Calls.Get_Word_64
         (Params, 0));
      Block_Size := Rose.Interfaces.Block_Device.Block_Size_Type
         (Rose.System_Calls.Get_Word_32 (Params, 2));
   end Get_Parameters;

   -------------------------
   -- Get_Read_Blocks_Cap --
   -------------------------

   function Get_Read_Blocks_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability
   is (Item.Read_Blocks);

   --------------------------
   -- Get_Write_Blocks_Cap --
   --------------------------

   function Get_Write_Blocks_Cap (Item : Block_Device_Client)
      return Rose.Capabilities.Capability
   is (Item.Write_Blocks);

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
     (Client        :    out Block_Device_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Get_Parameters := Params.Caps (0);
      Client.Read_Blocks := Params.Caps (1);
      Client.Write_Blocks := Params.Caps (2);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client         :    out Block_Device_Client;
      Get_Parameters : in     Rose.Capabilities.Capability;
      Read_Blocks    : in     Rose.Capabilities.Capability;
      Write_Blocks   : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Get_Parameters := Get_Parameters;
      Client.Read_Blocks := Read_Blocks;
      Client.Write_Blocks := Write_Blocks;
      Client.Is_Open := True;
   end Open_Cap_Set;

   -----------------
   -- Read_Blocks --
   -----------------

   procedure Read_Blocks
     (Item   : in     Block_Device_Client;
      Start  : in     Block_Address_Type;
      Count  : in     Natural;
      Blocks :    out System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Read_Blocks);
      --  Start in scalar;
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word_64 (Start));
      --  Count in scalar;
      Rose.System_Calls.Send_Word (Params, Count);
      --  Blocks out composite;
      Rose.System_Calls.Receive_Buffer (Params, Blocks'Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Rose.System_Calls.Copy_Received_Buffer (Blocks'Length, Blocks'Address);
   end Read_Blocks;

   ------------------
   -- Write_Blocks --
   ------------------

   procedure Write_Blocks
     (Item   : Block_Device_Client;
      Start  : Block_Address_Type;
      Count  : Natural;
      Blocks : System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Write_Blocks);
      --  Start in scalar;
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word_64 (Start));
      --  Count in scalar;
      Rose.System_Calls.Send_Word (Params, Count);
      --  Blocks in composite;
      Rose.System_Calls.Send_Storage_Array
        (Params,
         Blocks,
         False);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Write_Blocks;

end Rose.Interfaces.Block_Device.Client;
