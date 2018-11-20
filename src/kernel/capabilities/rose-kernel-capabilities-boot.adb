with Rose.Words;

with Rose.Kernel.Modules;
with Rose.Kernel.Processes.Init;
with Rose.Kernel.Validation;

with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Boot is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      pragma Unreferenced (Cap);
      use Rose.Invocation;
      Process_Id : constant Rose.Objects.Process_Id :=
                     Rose.Kernel.Processes.Init.Load_Boot_Module
                       (Priority => Rose.Kernel.Processes.Process_Priority
                          (Params.Data (0)),
                        Module   =>
                          Rose.Kernel.Modules.Module_Index
                            (Params.Data (0)));
   begin
      Rose.Boot.Console.Put ("boot module ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Data (0)));

      if Params.Control.Flags (Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               To_Cap : constant Rose.Capabilities.Capability :=
                          Rose.Kernel.Processes.Create_Cap
                            (Process_Id);
            begin
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (Params.Caps (Cap_Index)));
               Rose.Boot.Console.Put ("/");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (To_Cap));

               Rose.Kernel.Processes.Copy_Cap_Layout
                 (From_Process => Rose.Kernel.Processes.Current_Process_Id,
                  From_Cap     => Params.Caps (Cap_Index),
                  To_Process   => Process_Id,
                  To_Cap       => To_Cap);
               Rose.Kernel.Validation.Create_Cap
                 (Process_Id, To_Cap,
                  Rose.Kernel.Processes.Cap_Type
                    (Process_Id, To_Cap));
            end;
         end loop;
      end if;

      Rose.Boot.Console.New_Line;

      Params.Control.Flags := (Reply  => True, Send_Words => True,
                               others => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Data (0) := Rose.Words.Word (Process_Id);
      Rose.Kernel.Processes.Set_Current_State
        (Rose.Kernel.Processes.Current_Process_Id,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Boot;
