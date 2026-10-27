with Rose.Boot.Console;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Reply is

   use type Rose.Kernel.Processes.Process_Id;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      From_Pid : constant Rose.Kernel.Processes.Process_Id :=
                   Rose.Kernel.Processes.Current_Process_Id;
      To_Pid   : constant Rose.Kernel.Processes.Process_Id :=
                   Rose.Kernel.Processes.To_Process_Id
                     (Cap.Payload);
   begin
      if To_Pid = Rose.Kernel.Processes.Null_Process_Id
        or else not Rose.Kernel.Processes.Is_Blocked_On_Reply (To_Pid)
      then
         Rose.Boot.Console.Put ("reply-cap: ");
         Rose.Kernel.Processes.Debug.Put (From_Pid);
         Rose.Boot.Console.Put (" -> ");
         Rose.Kernel.Processes.Debug.Put (To_Pid);
         Rose.Boot.Console.Put (": receiver is not waiting for reply");
         Rose.Boot.Console.New_Line;
      else
         if Trace_Launch
           and then Rose.Kernel.Processes.Is_Command (From_Pid)
         then
            Rose.Kernel.Processes.Debug.Put (From_Pid);
            Rose.Boot.Console.Put (": trace: reply to ");
            Rose.Kernel.Processes.Debug.Put (To_Pid);
            Rose.Boot.Console.New_Line;
         end if;
         Rose.Kernel.Processes.Unmap_Invocation_Buffer (From_Pid, To_Pid);
         Rose.Kernel.Processes.Send_Reply
           (From_Pid, To_Pid, Params.all);
      end if;
   end Handle;

end Rose.Kernel.Capabilities.Reply;
