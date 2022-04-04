with System.Standard_Caps;
with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.Words;

package body System.Caps is

   --  Standard_Streams_Cap  : Rose.Capabilities.Capability := 0;
   Standard_Input_Cap    : Rose.Capabilities.Capability := 0;
   Standard_Output_Cap   : Rose.Capabilities.Capability := 0;
   Standard_Error_Cap    : Rose.Capabilities.Capability := 0;

   --  Current_Directory_Cap : Rose.Capabilities.Capability := 0;
   --  Clock_Cap             : Rose.Capabilities.Capability := 0;

   Exit_Process_Cap      : Rose.Capabilities.Capability := 0;

   Get_Cap               : Rose.Capabilities.Capability := 0;

   procedure Exit_Process (Status : Integer);
   pragma Export (C, Exit_Process, "exit");

   Caps_Loaded : Boolean := False;
   procedure Load_Caps;

   Invoke_Buffer_Length : constant := 4096;
   Invoke_Buffer        : System.Storage_Elements.Storage_Array
     (1 .. Invoke_Buffer_Length);

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Status : Integer) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Load_Caps;
      Rose.System_Calls.Initialize_Send
        (Params, Exit_Process_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Status));
      Rose.System_Calls.Invoke_Capability (Params);
   end Exit_Process;

   ---------------
   -- Load_Caps --
   ---------------

   procedure Load_Caps is

      function Get_Environment_Cap
        (Index : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Cap  => Get_Cap,
           Data => (1 => Index)));

   begin
      if Caps_Loaded then
         return;
      end if;

      Caps_Loaded := True;

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer_Length);

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send
           (Params, System.Standard_Caps.Argument_Cap_Set);
         Rose.System_Calls.Receive_Caps (Params, 3);
         Rose.System_Calls.Invoke_Capability (Params);
         Get_Cap := Params.Caps (1);

         Rose.System_Calls.Initialize_Send
           (Params, System.Standard_Caps.Process_Interface_Cap);
         Rose.System_Calls.Receive_Caps (Params, 7);
         Rose.System_Calls.Invoke_Capability (Params);
         Exit_Process_Cap := Params.Caps (6);
      end;

      Standard_Input_Cap := Get_Environment_Cap (1);
      Standard_Input_Cap :=
        Rose.System_Calls.Client.Get_Capability
          (Standard_Input_Cap, (1 => 0));
      Standard_Output_Cap := Get_Environment_Cap (2);
      Standard_Error_Cap := Get_Environment_Cap (3);

   end Load_Caps;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return Rose.Capabilities.Capability is
   begin
      Load_Caps;
      return Standard_Error_Cap;
   end Standard_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return Rose.Capabilities.Capability is
   begin
      Load_Caps;
      return Standard_Input_Cap;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return Rose.Capabilities.Capability is
   begin
      Load_Caps;
      return Standard_Output_Cap;
   end Standard_Output;

end System.Caps;
