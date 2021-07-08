with Rose.Boot.Console;

with Rose.Kernel.Processes;
with Rose.Kernel.Capabilities.Meta;

package body Rose.Kernel.Capabilities.Processes is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      Pid : constant Rose.Kernel.Processes.Process_Id :=
              Rose.Kernel.Processes.To_Process_Id
                (Cap.Payload);
      Current_Pid : constant Rose.Kernel.Processes.Process_Id :=
                      Rose.Kernel.Processes.Current_Process_Id;
   begin
      case Cap.Header.Endpoint is
         when Process_Interface_Endpoint =>
            declare
               use Rose.Capabilities, Rose.Capabilities.Layout;
               use Rose.Invocation;
               Caps : Rose.Capabilities.Capability_Array (1 .. 5);
            begin
               Rose.Kernel.Processes.Create_Caps (Current_Pid, Caps);

               declare
                  Destroy_Cap       : constant Capability := Caps (1);
                  Get_Object_Id_Cap : constant Capability := Caps (2);
                  Resume_Cap        : constant Capability := Caps (3);
                  Fault_Cap         : constant Capability := Caps (4);
                  Notify_Cap        : constant Capability := Caps (5);
               begin
                  if Destroy_Cap = Null_Capability then
                     Rose.Kernel.Processes.Return_Error
                       (Params, Out_Of_Capabilities);
                  else
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Destroy_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Process_Cap,
                                Endpoint => Kill_Process_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Get_Object_Id_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Meta_Cap,
                                Endpoint => Meta.Get_Object_Id_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Resume_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Process_Cap,
                                Endpoint => Resume_Process_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Fault_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Process_Cap,
                                Endpoint => Faulted_Process_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Notify_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Process_Cap,
                                Endpoint => Notify_Process_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));

                     Params.Control.Flags :=
                       (Rose.Invocation.Reply => True, others => False);

                     for Cap of Caps loop
                        Send_Cap (Params.all, Cap);
                     end loop;

                     Rose.Kernel.Processes.Set_Current_State
                       (Current_Pid,
                        Rose.Kernel.Processes.Ready);
                  end if;
               end;
            end;

         when Resume_Process_Endpoint =>

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);

            Rose.Kernel.Processes.Set_Current_State
              (Pid, Rose.Kernel.Processes.Ready);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Faulted_Process_Endpoint =>

            Rose.Kernel.Processes.Set_Current_State
              (Pid, Rose.Kernel.Processes.Faulted);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Kill_Process_Endpoint =>
            Rose.Boot.Console.Put ("Killing: ");
            Rose.Boot.Console.Put (Natural (Cap.Payload));
            Rose.Boot.Console.New_Line;

            Rose.Kernel.Processes.Kill_Process (Pid);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Start_Process_Endpoint =>

            Rose.Boot.Console.Put ("starting process ");
            Rose.Boot.Console.Put (Natural (Cap.Payload));
            Rose.Boot.Console.Put (" at ");
            Rose.Boot.Console.Put (Params.Data (0));
            Rose.Boot.Console.New_Line;

            Rose.Kernel.Processes.Start_Process
              (Pid, Params.Data (0));

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);
            Rose.Invocation.Send_Object_Id
              (Params.all, Cap.Payload);

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Initial_Cap_Endpoint =>
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               for Cap_Index in 0 .. Params.Control.Last_Sent_Cap loop
                  declare
                     Cap    : constant Rose.Capabilities.Capability :=
                                Rose.Kernel.Processes.Create_Cap
                                  (Pid);
                     Layout : Rose.Capabilities.Layout.Capability_Layout;
                  begin
                     Rose.Kernel.Processes.Get_Cap
                       (Current_Pid, Params.Caps (Cap_Index), Layout);

                     Rose.Kernel.Processes.Set_Cap
                       (Pid, Cap, Layout);
                  end;
               end loop;
            end if;

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when others =>
            Rose.Boot.Console.Put ("invalid endpoint: ");
            Rose.Boot.Console.Put (Natural (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;

      end case;

   end Handle;

end Rose.Kernel.Capabilities.Processes;
