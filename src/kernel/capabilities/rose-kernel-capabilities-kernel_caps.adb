with Rose.Kernel.Processes;
with Rose.Kernel.Panic;

package body Rose.Kernel.Capabilities.Kernel_Caps is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Enable_Paging_Endpoint =>
            Rose.Kernel.Processes.Set_Process_Handlers
              (On_Launch     => Params.Caps (0),
               On_Kill       => Params.Caps (1),
               On_Page_Fault => Params.Caps (2));
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);
         when others =>
            Rose.Kernel.Panic.Panic
              ("bad kernel cap endpoint");

      end case;
   end Handle;

end Rose.Kernel.Capabilities.Kernel_Caps;
