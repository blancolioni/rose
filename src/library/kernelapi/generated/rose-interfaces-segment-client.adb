with Rose.System_Calls;

package body Rose.Interfaces.Segment.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Item          : Segment_Client;
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
     (Item          : Segment_Client;
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

   ---------------------------------------
   -- Get_Add_Nonpersistent_Segment_Cap --
   ---------------------------------------

   function Get_Add_Nonpersistent_Segment_Cap (Item : Segment_Client)
      return Rose.Capabilities.Capability
   is (Item.Add_Nonpersistent_Segment);

   -------------------------
   -- Get_Add_Segment_Cap --
   -------------------------

   function Get_Add_Segment_Cap (Item : Segment_Client)
      return Rose.Capabilities.Capability
   is (Item.Add_Segment);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Segment_Client)
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
     (Client        :    out Segment_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Add_Segment := Params.Caps (0);
      Client.Add_Nonpersistent_Segment := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client                    :    out Segment_Client;
      Add_Segment               : in     Rose.Capabilities.Capability;
      Add_Nonpersistent_Segment : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Add_Segment := Add_Segment;
      Client.Add_Nonpersistent_Segment := Add_Nonpersistent_Segment;
      Client.Is_Open := True;
   end Open_Cap_Set;

end Rose.Interfaces.Segment.Client;
