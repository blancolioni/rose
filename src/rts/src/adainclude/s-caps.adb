with System.Standard_Caps;

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

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Status : Integer) is
      use Rose.Capabilities;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Exit_Process_Cap = 0 then
         Rose.System_Calls.Initialize_Send
           (Params, System.Standard_Caps.Process_Interface_Cap);
         Rose.System_Calls.Receive_Caps (Params, 7);
         Rose.System_Calls.Invoke_Capability (Params);
         Exit_Process_Cap := Params.Caps (6);
      end if;
      Rose.System_Calls.Initialize_Send
        (Params, Exit_Process_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Status));
      Rose.System_Calls.Invoke_Capability (Params);
   end Exit_Process;

   -------------------------
   -- Get_Environment_Cap --
   -------------------------

   function Get_Environment_Cap
     (Index : Positive)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities;
   begin
      if Get_Cap = Null_Capability then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send
              (Params, System.Standard_Caps.Argument_Cap_Set);
            Rose.System_Calls.Receive_Caps (Params, 3);
            Rose.System_Calls.Invoke_Capability (Params);
            Get_Cap := Params.Caps (1);
         end;
      end if;
      return Rose.System_Calls.Client.Get_Capability
        (Cap  => Get_Cap,
         Data => (1 => Rose.Words.Word (Index)));
   end Get_Environment_Cap;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return Rose.Capabilities.Capability is
      use type Rose.Capabilities.Capability;
   begin
      if Standard_Error_Cap = Rose.Capabilities.Null_Capability then
         Standard_Error_Cap := Get_Environment_Cap (3);
      end if;
      return Standard_Error_Cap;
   end Standard_Error;

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return Rose.Capabilities.Capability is
      use type Rose.Capabilities.Capability;
   begin
      if Standard_Input_Cap = Rose.Capabilities.Null_Capability then
         Standard_Input_Cap := Get_Environment_Cap (1);
         Standard_Input_Cap :=
           Rose.System_Calls.Client.Get_Capability
             (Standard_Input_Cap, (1 => 0));
      end if;
      return Standard_Input_Cap;
   end Standard_Input;

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return Rose.Capabilities.Capability is
      use type Rose.Capabilities.Capability;
   begin
      if Standard_Output_Cap = Rose.Capabilities.Null_Capability then
         Standard_Output_Cap := Get_Environment_Cap (2);
      end if;
      return Standard_Output_Cap;
   end Standard_Output;

end System.Caps;
