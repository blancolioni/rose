with Rose.System_Calls;

package body Rose.Interfaces.Region.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   ----------------------
   -- Create_Subregion --
   ----------------------

   function Create_Subregion
     (Item            : Region_Client;
      Subregion_Base  : Rose.Objects.Object_Id;
      Subregion_Bound : Rose.Objects.Object_Id;
      Flags           : Rose.Words.Word)
   return Rose.Interfaces.Region.Client.Region_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Create_Subregion);
      --  Subregion_Base in scalar;
      Rose.System_Calls.Send_Object_Id (Params, Subregion_Base);
      --  Subregion_Bound in scalar;
      Rose.System_Calls.Send_Object_Id (Params, Subregion_Bound);
      --  Flags in scalar;
      Rose.System_Calls.Send_Word (Params, Flags);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Region.Client.Region_Client;
      begin
         Rose.Interfaces.Region.Client.Open (Result, Params.Caps (0));
         return Result;
      end;
   end Create_Subregion;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item : in     Region_Client;
      Page : in     Rose.Objects.Object_Id;
      Data :    out System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get);
      --  Page in scalar;
      Rose.System_Calls.Send_Object_Id (Params, Page);
      --  Data out composite;
      Rose.System_Calls.Receive_Buffer (Params, Data'Length);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Rose.System_Calls.Copy_Received_Buffer (Data'Length, Data'Address);
   end Get;

   ------------------------------
   -- Get_Create_Subregion_Cap --
   ------------------------------

   function Get_Create_Subregion_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Create_Subregion);

   -----------------
   -- Get_Get_Cap --
   -----------------

   function Get_Get_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Get);

   -----------------------
   -- Get_Get_Range_Cap --
   -----------------------

   function Get_Get_Range_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Get_Range);

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -----------------
   -- Get_Put_Cap --
   -----------------

   function Get_Put_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Put);

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
     (Item       : in     Region_Client;
      Base_Page  :    out Rose.Objects.Object_Id;
      Bound_Page :    out Rose.Objects.Object_Id)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Get_Range);
      --  Base_Page out scalar;
      --  Bound_Page out scalar;
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Base_Page := Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64
         (Params, 0));
      Bound_Page := Rose.Objects.Object_Id (Rose.System_Calls.Get_Word_64
         (Params, 2));
   end Get_Range;

   ------------------
   -- Get_Read_Cap --
   ------------------

   function Get_Read_Cap (Item : Region_Client)
      return Rose.Capabilities.Capability
   is (Item.Read);

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
     (Client        :    out Region_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 5);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.Get_Range := Params.Caps (0);
      Client.Get := Params.Caps (1);
      Client.Put := Params.Caps (2);
      Client.Read := Params.Caps (3);
      Client.Create_Subregion := Params.Caps (4);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client           :    out Region_Client;
      Get_Range        : in     Rose.Capabilities.Capability;
      Get              : in     Rose.Capabilities.Capability;
      Put              : in     Rose.Capabilities.Capability;
      Read             : in     Rose.Capabilities.Capability;
      Create_Subregion : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.Get_Range := Get_Range;
      Client.Get := Get;
      Client.Put := Put;
      Client.Read := Read;
      Client.Create_Subregion := Create_Subregion;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item : Region_Client;
      Page : Rose.Objects.Object_Id;
      Data : System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Put);
      --  Page in scalar;
      Rose.System_Calls.Send_Object_Id (Params, Page);
      --  Data in composite;
      Rose.System_Calls.Send_Storage_Array
        (Params,
         Data,
         False);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Put;

   ----------
   -- Read --
   ----------

   function Read (Item : Region_Client)
      return Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Read);
      Rose.System_Calls.Receive_Caps (Params, 8);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      declare
         Result : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      begin
         Rose.Interfaces.Stream_Reader.Client.Open
           (Result,
            Params.Caps (0));
         return Result;
      end;
   end Read;

end Rose.Interfaces.Region.Client;
