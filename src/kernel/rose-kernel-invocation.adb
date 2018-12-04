with Ada.Unchecked_Conversion;

with Rose.Words;
with Rose.Boot.Console;

with Rose.Capabilities.Layout;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Processes.Debug;

with Rose.Kernel.Debug;
with Rose.Kernel.Panic;

with Rose.Kernel.Capabilities;

with Rose.Invocation.Trace;

package body Rose.Kernel.Invocation is

   -----------------------
   -- Invoke_Capability --
   -----------------------

   procedure Invoke_Capability
     (Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Capabilities;
      use Rose.Capabilities.Layout;
      use Rose.Invocation;
      use Rose.Kernel.Processes;
      use type Rose.Objects.Object_Id;
      Pid : constant Process_Id := Current_Process_Id;
      Log         : constant Boolean := Log_Invocation
        or else Current_Object_Id = Log_Object_Id;
      Log_Details : constant Boolean := False;
      Cap         : Rose.Capabilities.Layout.Capability_Layout;
      function To_Word_32 is
        new Ada.Unchecked_Conversion (Rose.Invocation.Invocation_Access,
                                      Rose.Words.Word_32);
      pragma Unreferenced (To_Word_32);
   begin

      if Params.Control.Flags (Block)
        or else Params.Control.Flags (Send_Buffer)
      then
         Set_Current_State (Pid, Blocked);
      end if;

      Params.Reply_Cap := Params.Cap;

      if not Current_Process_Cap (Params.Cap, Cap) then
         Rose.Boot.Console.Put ("invoke: bad cap ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Cap));
         Rose.Boot.Console.New_Line;
         Rose.Invocation.Trace.Put (Params.all, True);
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id, True);
         Rose.Kernel.Panic.Panic ("bad cap");
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
      else
         if Log then
            Rose.Kernel.Debug.Put_Call
              ("invoke", Cap, Params.all);
            if Log_Details then
               Rose.Kernel.Processes.Debug.Report_Process
                 (Pid, False);
            end if;
         end if;

         Rose.Kernel.Capabilities.Handle
           (Cap, Params);
      end if;

      if Params.Control.Flags (Rose.Invocation.Error) then
         Rose.Kernel.Processes.Set_Current_State
           (Rose.Kernel.Processes.Current_Process_Id,
            Rose.Kernel.Processes.Ready);
      end if;

      Rose.Kernel.Processes.Set_Current_Invocation (Params.all);
      Rose.Kernel.Processes.Queue.Choose_Process;

   end Invoke_Capability;

end Rose.Kernel.Invocation;
