with System.Standard_Caps;

with Rose.Interfaces.Process;
with Rose.Invocation;
with Rose.Standard_Streams;
with Rose.System_Calls;
with Rose.Words;

package body System.Caps is

   Exit_Process_Cap : Rose.Capabilities.Capability := 0;

   procedure Exit_Process (Status : Integer);
   pragma Export (C, Exit_Process, "exit");

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process (Status : Integer) is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Exit_Process_Cap = 0 then
         Rose.System_Calls.Initialize_Send
           (Params, System.Standard_Caps.Process_Interface_Cap);
         Rose.System_Calls.Receive_Caps
           (Params, Rose.Interfaces.Process.Subprogram_Count);
         Rose.System_Calls.Invoke_Capability (Params);
         Exit_Process_Cap :=
           Params.Caps (Rose.Interfaces.Process.Exit_Process_Index);
      end if;

      Rose.System_Calls.Initialize_Send
        (Params, Exit_Process_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Status));
      Rose.System_Calls.Invoke_Capability (Params);
   end Exit_Process;

   --------------------
   -- Standard_Error --
   --------------------

   function Standard_Error return Rose.Capabilities.Capability
   is (Rose.Standard_Streams.Standard_Error);

   --------------------
   -- Standard_Input --
   --------------------

   function Standard_Input return Rose.Capabilities.Capability
   is (Rose.Standard_Streams.Standard_Input);

   ---------------------
   -- Standard_Output --
   ---------------------

   function Standard_Output return Rose.Capabilities.Capability
   is (Rose.Standard_Streams.Standard_Output);

end System.Caps;
