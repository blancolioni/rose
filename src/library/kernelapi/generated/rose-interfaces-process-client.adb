with Rose.System_Calls;

package body Rose.Interfaces.Process.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Item          : Process_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params,
         Item.Add_Nonpersistent_Segment);
      --  Virtual_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Virtual_Base);
      --  Virtual_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Virtual_Bound);
      --  Flags in scalar;
      Rose.System_Calls.Send_Word (Params, Flags);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Add_Nonpersistent_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Item          : Process_Client;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Interfaces.Region.Client.Region_Client;
      Region_Offset : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Add_Segment);
      --  Virtual_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Virtual_Base);
      --  Virtual_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Virtual_Bound);
      --  Region in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Region.Client.Get_Interface_Cap (Region));
      --  Region_Offset in scalar;
      Rose.System_Calls.Send_Word (Params, Region_Offset);
      --  Flags in scalar;
      Rose.System_Calls.Send_Word (Params, Flags);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Add_Segment;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : Process_Client) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Destroy);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Destroy;

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process
     (Item        : Process_Client;
      Exit_Status : Natural)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Exit_Process);
      --  Exit_Status in scalar;
      Rose.System_Calls.Send_Word (Params, Exit_Status);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Exit_Process;

   --------------------------
   -- Get_Exit_Process_Cap --
   --------------------------

   function Get_Exit_Process_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Exit_Process);

   ----------------------------
   -- Get_Heap_Interface_Cap --
   ----------------------------

   function Get_Heap_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Heap_Interface);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id (Item : Process_Client)
      return Rose.Objects.Object_Id is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Object_Id);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Objects.Object_Id;
      begin
         Result := Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64
            (Params, 0));
         return Result;
      end;
   end Get_Object_Id;

   -------------------------------
   -- Get_Publish_Interface_Cap --
   -------------------------------

   function Get_Publish_Interface_Cap (Item : Process_Client)
      return Rose.Capabilities.Capability
   is (Item.Publish_Interface);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   --------------------
   -- Heap_Interface --
   --------------------

   function Heap_Interface (Item : Process_Client)
      return Rose.Interfaces.Heap.Client.Heap_Client is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Heap_Interface);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Heap.Client.Heap_Client;
      begin
         Rose.Interfaces.Heap.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Heap_Interface;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Process_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Add_Segment := Params.Caps (0);
      Client.Add_Nonpersistent_Segment := Params.Caps (1);
      Client.Published_Interface := Params.Caps (2);
      Client.Destroy := Params.Caps (3);
      Client.Get_Object_Id := Params.Caps (4);
      Client.Heap_Interface := Params.Caps (5);
      Client.Exit_Process := Params.Caps (6);
      Client.Publish_Interface := Params.Caps (7);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client                    :    out Process_Client;
      Add_Segment               : in     Rose.Capabilities.Capability;
      Add_Nonpersistent_Segment : in     Rose.Capabilities.Capability;
      Published_Interface       : in     Rose.Capabilities.Capability;
      Destroy                   : in     Rose.Capabilities.Capability;
      Get_Object_Id             : in     Rose.Capabilities.Capability;
      Heap_Interface            : in     Rose.Capabilities.Capability;
      Exit_Process              : in     Rose.Capabilities.Capability;
      Publish_Interface         : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Add_Segment := Add_Segment;
      Client.Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Client.Published_Interface := Published_Interface;
      Client.Destroy := Destroy;
      Client.Get_Object_Id := Get_Object_Id;
      Client.Heap_Interface := Heap_Interface;
      Client.Exit_Process := Exit_Process;
      Client.Publish_Interface := Publish_Interface;
      Client.Is_Open := True;
   end Open_Cap_Set;

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Item          : Process_Client;
      Interface_Cap : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Publish_Interface);
      --  Interface_Cap in scalar;
      Rose.System_Calls.Send_Cap (Params, Interface_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Publish_Interface;

   -------------------------
   -- Published_Interface --
   -------------------------

   function Published_Interface (Item : Process_Client)
      return Rose.Capabilities.Capability is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Published_Interface);
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
   end Published_Interface;

end Rose.Interfaces.Process.Client;
