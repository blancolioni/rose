with Rose.Boot.Console;
with Rose.Words;

with Rose.Kernel.Debug;
with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;
with Rose.Kernel.Capabilities.Meta;

package body Rose.Kernel.Capabilities.Processes is

   use type Rose.Kernel.Processes.Process_Id;

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
      if Pid = Rose.Kernel.Processes.Null_Process_Id then
         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Invalid_Object);
         return;
      end if;

      case Cap.Header.Endpoint is
         when Process_Interface_Endpoint =>

            --  The Kernel_Process interface: a cap for each subprogram, in
            --  the order of its index constants, a page at a time. The
            --  words are 0 (the subprograms), the index of the first cap
            --  and the number wanted (idle#101).

            declare
               use Rose.Capabilities, Rose.Capabilities.Layout;
               use Rose.Invocation;
               use type Rose.Words.Word;

               type Subprogram_Endpoint is
                  record
                     Cap_Type : Capability_Type;
                     Endpoint : Rose.Objects.Endpoint_Index;
                  end record;

               Endpoints : constant array (Natural range 0 .. 5)
                 of Subprogram_Endpoint :=
                   [(Process_Cap, Get_Name_Endpoint),
                    (Process_Cap, Kill_Process_Endpoint),
                    (Meta_Cap, Meta.Get_Object_Id_Endpoint),
                    (Process_Cap, Resume_Process_Endpoint),
                    (Process_Cap, Faulted_Process_Endpoint),
                    (Process_Cap, Notify_Process_Endpoint)];
               --  Name, Destroy, Get_Object_Id, Resume, Fault, Notify.

               Sent  : constant Natural := Sent_Word_Count (Params.all);
               First : Natural := 0;
               Count : Natural := Max_Capabilities;
            begin
               if Sent >= 2 then
                  if Params.Data (1) > Rose.Words.Word (Endpoints'Last) then
                     First := Endpoints'Last + 1;
                  else
                     First := Natural (Params.Data (1));
                  end if;
               end if;

               if Sent >= 3
                 and then Params.Data (2) in
                   1 .. Rose.Words.Word (Max_Capabilities)
               then
                  Count := Natural (Params.Data (2));
               end if;

               if First > Endpoints'Last then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Invalid_Operation);
                  return;
               end if;

               declare
                  Last : constant Natural :=
                           Natural'Min (Endpoints'Last, First + Count - 1);
                  Caps : Rose.Capabilities.Capability_Array
                           (1 .. Last - First + 1);
               begin
                  Rose.Kernel.Processes.Create_Caps (Current_Pid, Caps);

                  if Caps (Caps'First) = Null_Capability then
                     Rose.Kernel.Processes.Return_Error
                       (Params, Out_Of_Capabilities);
                     return;
                  end if;

                  for I in Caps'Range loop
                     Rose.Kernel.Processes.Set_Cap
                       (Current_Pid, Caps (I),
                        Capability_Layout'
                          (Header  =>
                               (Cap_Type =>
                                  Endpoints (First + I - 1).Cap_Type,
                                Endpoint =>
                                  Endpoints (First + I - 1).Endpoint,
                                others   => <>),
                           Payload => Cap.Payload));
                  end loop;

                  Params.Control.Flags :=
                    (Rose.Invocation.Reply => True, others => False);

                  for Cap of Caps loop
                     Send_Cap (Params.all, Cap);
                  end loop;

                  Rose.Kernel.Processes.Set_Current_State
                    (Current_Pid,
                     Rose.Kernel.Processes.Ready);
               end;
            end;

         when Signal_Process_Endpoint =>

            --  The sender goes on at once, whatever the process is doing
            --  (ADR 0006).

            Rose.Kernel.Processes.Raise_Signal
              (Pid, Natural (Cap.Header.Identifier));
            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);
            Rose.Kernel.Processes.Set_Current_State
              (Current_Pid, Rose.Kernel.Processes.Ready);

         when Resume_Process_Endpoint =>

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);

            Rose.Kernel.Processes.Set_Current_State
              (Pid, Rose.Kernel.Processes.Ready);
            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Faulted_Process_Endpoint =>

            --  The memory server has found a fault it can't handle. Its
            --  report goes to the console, which can be gone from the
            --  screen, so the process's state goes to the serial log, with
            --  the instruction it faulted at.

            Rose.Boot.Console.Put ("kernel: process faulted: ");
            Rose.Kernel.Processes.Debug.Put (Pid);
            Rose.Boot.Console.New_Line;
            Rose.Kernel.Processes.Debug.Report_Process (Pid);

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

                     --  A capability that the caller does not hold is not
                     --  copied.  It still takes a number, which holds
                     --  nothing, since those that follow count on it.

                     if Rose.Kernel.Processes.Has_Cap
                       (Current_Pid, Params.Caps (Cap_Index))
                     then
                        Rose.Kernel.Processes.Get_Cap
                          (Current_Pid, Params.Caps (Cap_Index), Layout);

                        Rose.Kernel.Debug.Put_Cap_Layout
                          ("initial-cap", Params.Caps (Cap_Index), Layout);

                        Rose.Kernel.Processes.Set_Cap
                          (Pid, Cap, Layout);
                     end if;
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
