with Rose.Boot.Console;
with Rose.Words;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Processes is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      Process_Id : constant Rose.Objects.Process_Id :=
                     Rose.Objects.To_Process_Id
                       (Cap.Payload);
   begin
      case Cap.Header.Endpoint is
         when Resume_Process_Endpoint =>

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);

            Rose.Kernel.Processes.Set_Current_State
              (Process_Id, Rose.Kernel.Processes.Ready);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Faulted_Process_Endpoint =>

            Rose.Boot.Console.Put (Rose.Words.Word_8 (Process_Id));
            Rose.Boot.Console.Put (" faulted at ");
            Rose.Boot.Console.Put
              (Rose.Kernel.Processes.Current_EIP (Process_Id));
            Rose.Boot.Console.New_Line;
            Rose.Kernel.Processes.Debug.Report_Process
              (Process_Id, False);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);
         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Processes;
