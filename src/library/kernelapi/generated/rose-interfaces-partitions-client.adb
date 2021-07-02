with Rose.System_Calls;

package body Rose.Interfaces.Partitions.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ---------------------------
   -- Get_Get_Partition_Cap --
   ---------------------------

   function Get_Get_Partition_Cap (Item : Partitions_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Partition);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Partitions_Client)
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
   -- Get_Partition --
   -------------------

   procedure Get_Partition
     (Item                : in     Partitions_Client;
      Index               : in     Positive;
      Partition_Type_Low  :    out Rose.Words.Word_64;
      Partition_Type_High :    out Rose.Words.Word_64;
      Partition_Flags     :    out Rose.Words.Word_64;
      Start_Address       :    out Rose.Words.Word_64;
      Length              :    out Rose.Words.Word_64)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Partition);
      --  Index in scalar;
      Rose.System_Calls.Send_Word (Params, Index);
      --  Partition_Type_Low out scalar;
      --  Partition_Type_High out scalar;
      --  Partition_Flags out scalar;
      --  Start_Address out scalar;
      --  Length out scalar;
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Partition_Type_Low := Rose.System_Calls.Get_Word_64 (Params, 0);
      Partition_Type_High := Rose.System_Calls.Get_Word_64 (Params, 2);
      Partition_Flags := Rose.System_Calls.Get_Word_64 (Params, 4);
      Start_Address := Rose.System_Calls.Get_Word_64 (Params, 6);
      Length := Rose.System_Calls.Get_Word_64 (Params, 8);
   end Get_Partition;

   -----------------------------
   -- Get_Partition_Count_Cap --
   -----------------------------

   function Get_Partition_Count_Cap (Item : Partitions_Client)
      return Rose.Capabilities.Capability
   is (Item.Partition_Count);

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
     (Client        :    out Partitions_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Partition_Count := Params.Caps (0);
      Client.Get_Partition := Params.Caps (1);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client          :    out Partitions_Client;
      Partition_Count : in     Rose.Capabilities.Capability;
      Get_Partition   : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Partition_Count := Partition_Count;
      Client.Get_Partition := Get_Partition;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ---------------------
   -- Partition_Count --
   ---------------------

   function Partition_Count (Item : Partitions_Client) return Natural is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Partition_Count);
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
   end Partition_Count;

end Rose.Interfaces.Partitions.Client;
