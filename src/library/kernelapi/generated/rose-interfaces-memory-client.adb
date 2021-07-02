with Rose.System_Calls;

package body Rose.Interfaces.Memory.Client is

   Last_Error : Rose.Invocation.Invocation_Error := Rose.Invocation.OK;

   -----------------------
   -- Get_Interface_Cap --
   -----------------------

   function Get_Interface_Cap (Item : Memory_Client)
      return Rose.Capabilities.Capability
   is (Item.Interface_Cap);

   --------------------
   -- Get_Last_Error --
   --------------------

   function Get_Last_Error return Rose.Invocation.Invocation_Error is
   begin
      return Last_Error;
   end Get_Last_Error;

   -------------------------
   -- Get_New_Process_Cap --
   -------------------------

   function Get_New_Process_Cap (Item : Memory_Client)
      return Rose.Capabilities.Capability
   is (Item.New_Process);

   ------------------------
   -- Get_Page_Fault_Cap --
   ------------------------

   function Get_Page_Fault_Cap (Item : Memory_Client)
      return Rose.Capabilities.Capability
   is (Item.Page_Fault);

   ------------------------------
   -- Get_Register_Process_Cap --
   ------------------------------

   function Get_Register_Process_Cap (Item : Memory_Client)
      return Rose.Capabilities.Capability
   is (Item.Register_Process);

   ----------------------------------
   -- Get_Take_Physical_Memory_Cap --
   ----------------------------------

   function Get_Take_Physical_Memory_Cap (Item : Memory_Client)
      return Rose.Capabilities.Capability
   is (Item.Take_Physical_Memory);

   ---------------
   -- Has_Error --
   ---------------

   function Has_Error return Boolean is
      use Rose.Invocation;
   begin
      return Last_Error /= OK;
   end Has_Error;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Item    : Memory_Client;
      Process : Rose.Interfaces.Kernel_Process.Client.Kernel_Process_Client)
   return Rose.Interfaces.Process.Client.Process_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.New_Process);
      --  Process in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Kernel_Process.Client.Get_Interface_Cap (Process));
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
   end New_Process;

   ----------
   -- Open --
   ----------

   procedure Open
     (Client        :    out Memory_Client;
      Interface_Cap : in     Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Client.Is_Open := False;
      Client.Interface_Cap := Interface_Cap;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Receive_Caps (Params, 4);
      Rose.System_Calls.Invoke_Capability (Params);
      Client.New_Process := Params.Caps (0);
      Client.Register_Process := Params.Caps (1);
      Client.Page_Fault := Params.Caps (2);
      Client.Take_Physical_Memory := Params.Caps (3);
      Client.Is_Open := True;
   end Open;

   ------------------
   -- Open_Cap_Set --
   ------------------

   procedure Open_Cap_Set
     (Client               :    out Memory_Client;
      New_Process          : in     Rose.Capabilities.Capability;
      Register_Process     : in     Rose.Capabilities.Capability;
      Page_Fault           : in     Rose.Capabilities.Capability;
      Take_Physical_Memory : in     Rose.Capabilities.Capability)
   is
   begin
      Client.Is_Open := False;
      Client.New_Process := New_Process;
      Client.Register_Process := Register_Process;
      Client.Page_Fault := Page_Fault;
      Client.Take_Physical_Memory := Take_Physical_Memory;
      Client.Is_Open := True;
   end Open_Cap_Set;

   ----------------
   -- Page_Fault --
   ----------------

   procedure Page_Fault
     (Item            : Memory_Client;
      Faulting_Object : Rose.Objects.Object_Id;
      Virtual_Page    : Rose.Words.Word;
      Physical_Page   : Rose.Words.Word;
      Action          : Rose.Interfaces.Memory.Page_Access_Type)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Page_Fault);
      --  Faulting_Object in scalar;
      Rose.System_Calls.Send_Object_Id (Params, Faulting_Object);
      --  Virtual_Page in scalar;
      Rose.System_Calls.Send_Word (Params, Virtual_Page);
      --  Physical_Page in scalar;
      Rose.System_Calls.Send_Word (Params, Physical_Page);
      --  Action in scalar;
      Rose.System_Calls.Send_Word (Params, Natural'
         (Rose.Interfaces.Memory.Page_Access_Type'Pos (Action)));
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Page_Fault;

   ----------------------
   -- Register_Process --
   ----------------------

   procedure Register_Process
     (Item        : Memory_Client;
      Process     : Rose.Interfaces.Process.Client.Process_Client;
      Exec_Base   : Rose.Words.Word;
      Exec_Bound  : Rose.Words.Word;
      Text_Base   : Rose.Words.Word;
      Text_Bound  : Rose.Words.Word;
      Data_Base   : Rose.Words.Word;
      Data_Bound  : Rose.Words.Word;
      Stack_Base  : Rose.Words.Word;
      Stack_Bound : Rose.Words.Word;
      Environment : System.Storage_Elements.Storage_Array)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Register_Process);
      --  Process in scalar;
      Rose.System_Calls.Send_Cap (Params,
         Rose.Interfaces.Process.Client.Get_Interface_Cap (Process));
      --  Exec_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Exec_Base);
      --  Exec_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Exec_Bound);
      --  Text_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Text_Base);
      --  Text_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Text_Bound);
      --  Data_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Data_Base);
      --  Data_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Data_Bound);
      --  Stack_Base in scalar;
      Rose.System_Calls.Send_Word (Params, Stack_Base);
      --  Stack_Bound in scalar;
      Rose.System_Calls.Send_Word (Params, Stack_Bound);
      --  Environment in composite;
      Rose.System_Calls.Send_Storage_Array
        (Params,
         Environment,
         False);
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
   end Register_Process;

   --------------------------
   -- Take_Physical_Memory --
   --------------------------

   procedure Take_Physical_Memory
     (Item   : in     Memory_Client;
      Size   : in     Rose.Words.Word;
      Start  :    out Rose.Words.Word;
      Amount :    out Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Item.Take_Physical_Memory);
      --  Size in scalar;
      Rose.System_Calls.Send_Word (Params, Size);
      --  Start out scalar;
      --  Amount out scalar;
      Rose.System_Calls.Invoke_Capability (Params);
      Last_Error := Rose.Invocation.OK;
      if Params.Control.Flags (Rose.Invocation.Error) then
         Last_Error := Params.Error;
      end if;
      Start := Rose.System_Calls.Get_Word_32 (Params, 0);
      Amount := Rose.System_Calls.Get_Word_32 (Params, 1);
   end Take_Physical_Memory;

end Rose.Interfaces.Memory.Client;
