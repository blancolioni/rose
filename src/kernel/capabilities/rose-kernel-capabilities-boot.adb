with Rose.Objects;
with Rose.Words;

with Rose.Kernel.Modules;
with Rose.Kernel.Processes.Init;

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
      Current_Pid      : constant Rose.Kernel.Processes.Process_Id :=
                           Rose.Kernel.Processes.Current_Process_Id;
      New_Pid          : Rose.Kernel.Processes.Process_Id;
      Oid              : constant Rose.Objects.Object_Id :=
                           Rose.Objects.Object_Id (Params.Data (2));

   begin

      --  Data: the module, its priority, and its object id.  The object
      --  id is one of the boot modules', other than idle's and init's,
      --  and no live process has it.

      declare
         use Rose.Kernel.Processes;
         use type Rose.Objects.Object_Id;
      begin
         if Params.Control.Last_Sent_Word < 2
           or else Oid not in Rose.Objects.Boot_Process_Object_Id
           or else Oid <= Rose.Objects.Init_Object_Id
           or else To_Process_Id (Oid) /= Null_Process_Id
         then
            Return_Error (Params, Request_Error);
            return;
         end if;
      end;

      New_Pid :=
        Rose.Kernel.Processes.Init.Load_Boot_Module
          (Priority => Rose.Kernel.Processes.Process_Priority
             (Params.Data (1)),
           Module   =>
             Rose.Kernel.Modules.Module_Index
               (Params.Data (0)),
           Oid         => Oid);

      Rose.Boot.Console.Put ("boot module ");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Data (0)));

      if Params.Control.Flags (Send_Caps) then
         for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
            declare
               To_Cap : constant Rose.Capabilities.Capability :=
                          Rose.Kernel.Processes.Create_Cap
                            (New_Pid);
            begin
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (To_Cap));
               Rose.Boot.Console.Put ("/");
               Rose.Boot.Console.Put
                 (Rose.Words.Word_8 (Params.Caps (Cap_Index)));

               Rose.Kernel.Processes.Copy_Cap_Layout
                 (From_Process_Id => Current_Pid,
                  From_Cap        => Params.Caps (Cap_Index),
                  To_Process_Id   => New_Pid,
                  To_Cap          => To_Cap);
            end;
         end loop;
      end if;

      Rose.Boot.Console.New_Line;

      --  If the caller receives caps, the reply has a copy cap for the
      --  new process, so that the caller never has to make one (#115).

      declare
         use type Rose.Capabilities.Capability;
         Receives_Caps : constant Boolean :=
                           Params.Control.Flags (Recv_Caps);
         Copy_Cap      : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
      begin
         if Receives_Caps then
            Copy_Cap := Rose.Kernel.Processes.Create_Cap (Current_Pid);
            if Copy_Cap /= Rose.Capabilities.Null_Capability then
               Rose.Kernel.Processes.Set_Cap
                 (Pid    => Current_Pid,
                  Cap    => Copy_Cap,
                  Layout =>
                    (Header  =>
                       (Cap_Type => Rose.Capabilities.Layout.Copy_Cap,
                        Endpoint => 1,
                        others   => <>),
                     Payload => Oid));
            end if;
         end if;

         Params.Control.Flags := (Reply  => True, others => False);

         if Copy_Cap /= Rose.Capabilities.Null_Capability then
            Params.Control.Flags (Send_Caps) := True;
            Params.Control.Last_Sent_Cap := 0;
            Params.Caps (0) := Copy_Cap;
         end if;
      end;

      Rose.Invocation.Send_Object_Id
        (Params.all, Rose.Kernel.Processes.To_Object_Id (New_Pid));
      Rose.Kernel.Processes.Set_Current_State
        (Current_Pid,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Boot;
