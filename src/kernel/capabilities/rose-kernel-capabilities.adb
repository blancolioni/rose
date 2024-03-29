with Rose.Kernel.Capabilities.Arch;
with Rose.Kernel.Capabilities.Boot;
with Rose.Kernel.Capabilities.Copy;
with Rose.Kernel.Capabilities.Create;
with Rose.Kernel.Capabilities.Endpoint;
with Rose.Kernel.Capabilities.Interrupt;
with Rose.Kernel.Capabilities.Kernel_Caps;
with Rose.Kernel.Capabilities.Meta;
with Rose.Kernel.Capabilities.Pages;
with Rose.Kernel.Capabilities.Page_Table;
with Rose.Kernel.Capabilities.Physical_Memory;
with Rose.Kernel.Capabilities.Processes;
with Rose.Kernel.Capabilities.Receive;
with Rose.Kernel.Capabilities.Reply;

with Rose.Words;
with Rose.Boot.Console;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

with Rose.Kernel.Debug;

package body Rose.Kernel.Capabilities is

   use Rose.Capabilities.Layout;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      package Handlers renames Rose.Kernel.Capabilities;
   begin
      if Log_Invocation then
         Rose.Kernel.Debug.Put_Call
           ("invoke", Cap, Params.all);
      end if;
      case Cap.Header.Cap_Type is
         when Arch_Cap =>
            Handlers.Arch.Handle (Cap, Params);
         when Boot_Cap =>
            Handlers.Boot.Handle (Cap, Params);
         when Copy_Cap =>
            Handlers.Copy.Handle (Cap, Params);
         when Create_Cap =>
            Handlers.Create.Handle (Cap, Params);
         when Endpoint_Cap =>
            Handlers.Endpoint.Handle (Cap, Params);
         when Interrupt_Cap =>
            Handlers.Interrupt.Handle (Cap, Params);
         when Kernel_Cap =>
            Handlers.Kernel_Caps.Handle (Cap, Params);
         when Meta_Cap =>
            Handlers.Meta.Handle (Cap, Params);
         when Page_Object_Cap =>
            Handlers.Pages.Handle (Cap, Params);
         when Page_Table_Cap =>
            Handlers.Page_Table.Handle (Cap, Params);
         when Physical_Memory_Cap =>
            Handlers.Physical_Memory.Handle (Cap, Params);
         when Process_Cap =>
            Handlers.Processes.Handle (Cap, Params);
         when Receive_Cap =>
            Handlers.Receive.Handle (Cap, Params);
         when Reply_Cap =>
            Handlers.Reply.Handle (Cap, Params);
         when others =>
            Rose.Boot.Console.Put
              ("kernel: pid ");
            Rose.Kernel.Processes.Debug.Put
              (Rose.Kernel.Processes.Current_Process_Id);
            Rose.Boot.Console.Put (": no handler for cap type: ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8
                 (Capability_Type'Pos (Cap.Header.Cap_Type)));
            Rose.Boot.Console.New_Line;
      end case;

   end Handle;

end Rose.Kernel.Capabilities;
