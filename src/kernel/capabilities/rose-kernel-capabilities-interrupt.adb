with Rose.Boot.Console;
with Rose.Arch.Interrupt_Table;

with Rose.Words;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

with Rose.Arch.Interrupts;
with Rose.Kernel.Interrupts;

package body Rose.Kernel.Capabilities.Interrupt is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Reserve_Interrupt_Endpoint =>
            if not Params.Control.Flags (Rose.Invocation.Send_Caps) then
               Rose.Kernel.Processes.Debug.Put
                 (Rose.Kernel.Processes.Current_Process_Id);
               Rose.Boot.Console.Put (": while installing IRQ ");
               Rose.Boot.Console.Put (Natural (Cap.Payload));
               Rose.Boot.Console.Put_Line
                 (": missing handler cap");
               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            if not Rose.Kernel.Processes.Has_Cap
              (Rose.Kernel.Processes.Current_Process_Id,
               Params.Caps (0))
            then
               Rose.Boot.Console.Put
                 ("interrupt: invalid cap ");
               Rose.Boot.Console.Put (Natural (Params.Caps (0)));
               Rose.Boot.Console.New_Line;

               Rose.Kernel.Processes.Return_Error
                 (Params, Rose.Invocation.Request_Error);
               return;
            end if;

            declare
               use type Rose.Objects.Object_Id;
               Handler_Cap : constant Rose.Capabilities.Capability :=
                       Params.Caps (0);
               Vector      : constant Rose.Arch.Interrupts.Interrupt_Vector :=
                               Rose.Arch.Interrupts.Interrupt_Vector
                                 (Cap.Payload mod 64);
            begin
               if Rose.Kernel.Interrupts.Has_Handler (Vector) then
                  Rose.Boot.Console.Put ("interrupt: ");
                  Rose.Boot.Console.Put (Natural (Vector));
                  Rose.Boot.Console.Put (" already has a handler");
                  Rose.Boot.Console.New_Line;
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Request_Error);
                  return;
               end if;

               Rose.Kernel.Processes.Debug.Put
                 (Rose.Kernel.Processes.Current_Process_Id);
               Rose.Boot.Console.Put (": added handler for irq ");
               Rose.Boot.Console.Put (Natural (Vector));
               Rose.Boot.Console.New_Line;

               Rose.Kernel.Interrupts.Set_Handler
                 (Vector,
                  Rose.Kernel.Processes.Current_Object_Id,
                  Handler_Cap);
               if Vector in 32 .. 47 then
                  Rose.Arch.Interrupt_Table.Enable_Interrupt (Vector);
               end if;

            end;

            Params.Control.Flags :=
              (Rose.Invocation.Reply  => True, others => False);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when others =>
            Rose.Boot.Console.Put
              ("kernel: interrupt: unknown endpoint: ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Interrupt;
