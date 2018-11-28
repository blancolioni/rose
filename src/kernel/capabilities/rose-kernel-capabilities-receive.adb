with Rose.Kernel.Processes;

package body Rose.Kernel.Capabilities.Receive is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      pragma Unreferenced (Cap);
      use type Rose.Kernel.Processes.Process_Id;
      Receiver_Id : constant Rose.Kernel.Processes.Process_Id :=
                              Rose.Kernel.Processes.Current_Process_Id;
      Next_Sender : constant Rose.Kernel.Processes.Process_Id :=
                      Rose.Kernel.Processes.Next_Blocked_Sender
                        (Receiver_Id);
   begin
      if Next_Sender = Rose.Kernel.Processes.Null_Process_Id then
         if Params.Control.Flags (Rose.Invocation.Block) then
            Rose.Kernel.Processes.Receive (Receiver_Id, Params.all);
         else
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Request_Would_Block);
         end if;
      else
         Rose.Kernel.Processes.Unblock_And_Send
           (From_Process_Id => Next_Sender,
            To_Process_Id   => Receiver_Id,
            Receiver_Params => Params);
      end if;
   end Handle;

end Rose.Kernel.Capabilities.Receive;
