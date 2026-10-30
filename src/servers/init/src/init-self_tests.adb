with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Launch.Client;

with Init.Calls;

package body Init.Self_Tests is

   use type Rose.Capabilities.Capability;

   Completed : constant Rose.Words.Word := 0;
   --  The first word of a command's reply when it completes
   --  (Rose.Command_Line.Launch).

   ---------
   -- Run --
   ---------

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "run_self_tests");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  The command replies when the tests have finished.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Clock);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      return not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Params.Data (0) = Completed;
   end Run;

end Init.Self_Tests;
