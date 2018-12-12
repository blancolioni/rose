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
               Destroy_Cap       : constant Rose.Capabilities.Capability :=
                                     Rose.Kernel.Processes.Create_Caps
                                       (Current_Pid, 5);
               Get_Object_Id_Cap : constant Rose.Capabilities.Capability :=
                                     Destroy_Cap + 1;
               Resume_Cap        : constant Rose.Capabilities.Capability :=
                                     Destroy_Cap + 2;
               Fault_Cap         : constant Rose.Capabilities.Capability :=
                                     Destroy_Cap + 3;
               Notify_Cap        : constant Rose.Capabilities.Capability :=
                                     Destroy_Cap + 4;
            begin
               if Destroy_Cap = Null_Capability then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Out_Of_Capabilities);
               else
                  Rose.Kernel.Processes.Set_Cap
                    (Current_Pid, Destroy_Cap,
                     Capability_Layout'
                       (Header  =>
                            (Cap_Type => Meta_Cap,
                             Endpoint => Meta.Delete_Cap,
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
                  Send_Cap (Params.all, Destroy_Cap);
                  Send_Cap (Params.all, Get_Object_Id_Cap);
                  Send_Cap (Params.all, Resume_Cap);
                  Send_Cap (Params.all, Fault_Cap);
                  Send_Cap (Params.all, Notify_Cap);
                  Rose.Kernel.Processes.Set_Current_State
                    (Current_Pid,
                     Rose.Kernel.Processes.Ready);
               end if;
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
         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Processes;
