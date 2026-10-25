with Rose.Boot.Console;

with Rose.Kernel.Debug;
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
               Caps : Rose.Capabilities.Capability_Array (1 .. 6);
            begin
               Rose.Kernel.Processes.Create_Caps (Current_Pid, Caps);

               declare
                  Get_Name_Cap      : constant Capability := Caps (1);
                  Destroy_Cap       : constant Capability := Caps (2);
                  Get_Object_Id_Cap : constant Capability := Caps (3);
                  Resume_Cap        : constant Capability := Caps (4);
                  Fault_Cap         : constant Capability := Caps (5);
                  Notify_Cap        : constant Capability := Caps (6);
               begin
                  if Get_Name_Cap = Null_Capability then
                     Rose.Kernel.Processes.Return_Error
                       (Params, Out_Of_Capabilities);
                  else
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Get_Name_Cap,
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type => Process_Cap,
                                Endpoint => Get_Name_Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
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

                     Rose.Kernel.Debug.Put_Cap_Layout
                       ("initial-cap", Params.Caps (Cap_Index), Layout);

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

         when Command_Endpoint_Endpoint =>
            declare
               use Rose.Capabilities;
               use type Rose.Invocation.Parameter_Word_Index;
               use type Rose.Objects.Endpoint_Id;
               use type Rose.Objects.Endpoint_Index;
               Endpoint_Id    : Rose.Objects.Endpoint_Id := 0;
               Local_Endpoint : Rose.Objects.Endpoint_Index := 0;
               Receive_Cap    : Capability := Null_Capability;
               Endpoint_Cap   : Capability := Null_Capability;
            begin
               if Params.Control.Flags (Rose.Invocation.Send_Words)
                 and then Params.Control.Last_Sent_Word >= 1
               then
                  Endpoint_Id :=
                    Rose.Objects.Endpoint_Id (Params.Data (0))
                    + 2 ** 32 * Rose.Objects.Endpoint_Id (Params.Data (1));
               end if;

               if Endpoint_Id = 0 then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Request_Error);
                  Rose.Kernel.Processes.Set_Current_State
                    (Current_Pid, Rose.Kernel.Processes.Ready);
                  return;
               end if;

               Local_Endpoint :=
                 Rose.Kernel.Processes.Require_Endpoint (Pid, Endpoint_Id);
               if Local_Endpoint /= 0 then
                  Receive_Cap := Rose.Kernel.Processes.Create_Cap (Pid);
               end if;
               if Receive_Cap /= Null_Capability then
                  Endpoint_Cap :=
                    Rose.Kernel.Processes.New_Cap
                      (Current_Pid,
                       Rose.Capabilities.Layout.Endpoint_Capability
                         (Rose.Kernel.Processes.To_Object_Id (Pid),
                          Local_Endpoint, 0));
               end if;

               if Endpoint_Cap = Null_Capability then
                  if Receive_Cap /= Null_Capability then
                     Rose.Kernel.Processes.Delete_Cap (Pid, Receive_Cap);
                  end if;
                  Rose.Kernel.Processes.Return_Error
                    (Params,
                     (if Local_Endpoint = 0
                      then Rose.Invocation.Out_Of_Endpoints
                      else Rose.Invocation.Out_Of_Capabilities));
                  Rose.Kernel.Processes.Set_Current_State
                    (Current_Pid, Rose.Kernel.Processes.Ready);
                  return;
               end if;

               Rose.Kernel.Processes.Set_Cap
                 (Pid, Receive_Cap,
                  Rose.Capabilities.Layout.Receive_Capability
                    (Rose.Kernel.Processes.To_Object_Id (Pid),
                     Local_Endpoint));
               Rose.Kernel.Processes.Set_Receive_Cap
                 (Pid, Local_Endpoint, Receive_Cap);
               Rose.Kernel.Processes.Set_Command_Endpoint
                 (Pid, Local_Endpoint);

               Params.Control.Flags :=
                 (Rose.Invocation.Reply => True, others => False);
               Rose.Invocation.Send_Cap (Params.all, Endpoint_Cap);
               Rose.Kernel.Processes.Set_Current_State
                 (Current_Pid, Rose.Kernel.Processes.Ready);
            end;

         when others =>
            Rose.Boot.Console.Put ("invalid endpoint: ");
            Rose.Boot.Console.Put (Natural (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;

      end case;

   end Handle;

end Rose.Kernel.Capabilities.Processes;
