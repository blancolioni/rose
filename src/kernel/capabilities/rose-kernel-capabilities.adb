with Rose.Kernel.Capabilities.Arch;
with Rose.Kernel.Capabilities.Boot;
with Rose.Kernel.Capabilities.Copy;
with Rose.Kernel.Capabilities.Create;
with Rose.Kernel.Capabilities.Endpoint;
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

package body Rose.Kernel.Capabilities is

   use Rose.Capabilities.Layout;

--     type Cap_Handler is access
--     procedure (Cap    : Rose.Capabilities.Layout.Capability_Layout;
--                  Params : Rose.Invocation.Invocation_Access);
--
--     Handler : constant array (Capability_Type) of Cap_Handler :=
--              (Rose.Capabilities.Layout.Arch_Cap => Arch_Cap.Handle'Access,
--                  Rose.Capabilities.Layout.Create_Cap =>
--                    Rose.Kernel.Capabilities.Create_Cap.Handle'Access,
--                  Rose.Capabilities.Layout.Endpoint_Cap =>
--                    Rose.Kernel.Capabilities.Endpoint_Cap.Handle'Access,
--                  Rose.Capabilities.Layout.Entry_Cap    =>
--                    Rose.Kernel.Capabilities.Entry_Cap.Handle'Access,
--                  Rose.Capabilities.Layout.Meta_Cap     =>
--                    Meta_Cap.Handle'Access,
--                  others                                => null);

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      package Handlers renames Rose.Kernel.Capabilities;
   begin
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
            Rose.Boot.Console.Put
              (Rose.Words.Word_8
                 (Rose.Kernel.Processes.Current_Process_Id));
            Rose.Boot.Console.Put (": no handler for cap type: ");
            Rose.Boot.Console.Put
              (Rose.Words.Word_8
                 (Capability_Type'Pos (Cap.Header.Cap_Type)));
            Rose.Boot.Console.New_Line;
      end case;

   end Handle;

end Rose.Kernel.Capabilities;
