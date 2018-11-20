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
      use type Rose.Objects.Process_Id;
      Receiver_Id : constant Rose.Objects.Process_Id :=
                              Rose.Kernel.Processes.Current_Process_Id;
      Next_Sender : constant Rose.Objects.Process_Id :=
                      Rose.Kernel.Processes.Next_Blocked_Sender
                        (Receiver_Id);
   begin
      if Next_Sender = 0 then
         if Params.Control.Flags (Rose.Invocation.Block) then
            Rose.Kernel.Processes.Receive (Receiver_Id, Params.all);
         else
            Rose.Kernel.Processes.Return_Error
              (Params, Rose.Invocation.Request_Would_Block);
         end if;
      else
         Rose.Kernel.Processes.Send_Cap
           (From_Process => Next_Sender,
            To_Process   => Receiver_Id,
            Sender_Cap   => 0,
            Receiver_Cap => Params.Cap,
            Params       => Params.all);
      end if;
   end Handle;

end Rose.Kernel.Capabilities.Receive;
