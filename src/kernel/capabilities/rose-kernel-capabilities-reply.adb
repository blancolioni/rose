with Rose.Kernel.Processes;

package body Rose.Kernel.Capabilities.Reply is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      From_Pid : constant Rose.Objects.Process_Id :=
                   Rose.Kernel.Processes.Current_Process_Id;
      To_Pid   : constant Rose.Objects.Process_Id :=
                   Rose.Objects.To_Process_Id
                     (Cap.Payload);
   begin
      Rose.Kernel.Processes.Unmap_Invocation_Buffer (From_Pid);
      Rose.Kernel.Processes.Send_Reply
        (From_Pid, To_Pid, Params.all);
   end Handle;

end Rose.Kernel.Capabilities.Reply;
