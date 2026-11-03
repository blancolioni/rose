with Rose.Kernel.Physical_Memory;
with Rose.Kernel.Processes;

package body Rose.Kernel.Capabilities.Pages is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      Pid : constant Rose.Kernel.Processes.Process_Id :=
                     Rose.Kernel.Processes.Current_Process_Id;
   begin
      case Cap.Header.Endpoint is
         when Identity_Map_Page =>
            declare
               Physical_Page : constant Physical_Page_Address :=
                                 Page_Object_Id_To_Physical_Page_Address
                                   (Cap.Payload);
               Virtual_Page  : constant Virtual_Page_Address :=
                                 Virtual_Page_Address (Physical_Page);
            begin
               Rose.Kernel.Processes.Map_Page
                 (Pid       => Pid,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Physical_Page,
                  Readable      => True,
                  Writable      => True,
                  Executable    => False,
                  User          => True);
            end;

            Params.Control.Flags :=
              (Rose.Invocation.Reply => True, others => False);

            Rose.Kernel.Processes.Set_Current_State
              (Pid, Rose.Kernel.Processes.Ready);

         when Map_Device_Range =>
            declare
               use Rose.Invocation;
               use Rose.Words;
               Range_Id   : constant Rose.Objects.Object_Id :=
                              Cap.Payload
                              - Rose.Objects.Page_Object_Id'First;
               First_Page : constant Physical_Page_Address :=
                              Physical_Page_Address
                                (Range_Id mod 2 ** 32);
               Page_Count : constant Word :=
                              Word (Range_Id / 2 ** 32);
               Address    : constant Word := Params.Data (0);
               Valid      : Boolean :=
                              Params.Control.Flags (Send_Words)
                              and then Params.Control.Last_Sent_Word = 0
                              and then Address mod Physical_Page_Bytes = 0
                              and then Address >= Device_Window_Base
                              and then Address < Device_Window_Bound
                              and then Page_Count > 0
                              and then Page_Count
                                <= (Device_Window_Bound - Address)
                                   / Physical_Page_Bytes;
            begin
               if Valid then
                  for I in 0 .. Page_Count - 1 loop
                     if Rose.Kernel.Physical_Memory.Is_RAM
                       (First_Page + Physical_Page_Address (I))
                     then
                        Valid := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if Valid then
                  for I in 0 .. Page_Count - 1 loop
                     Rose.Kernel.Processes.Map_Page
                       (Pid           => Pid,
                        Virtual_Page  =>
                          Virtual_Address_To_Page
                            (Virtual_Address
                               (Address + I * Physical_Page_Bytes)),
                        Physical_Page =>
                          First_Page + Physical_Page_Address (I),
                        Readable      => True,
                        Writable      => True,
                        Executable    => False,
                        User          => True);
                  end loop;

                  Params.Control.Flags := (Reply => True, others => False);
               else
                  Rose.Kernel.Processes.Return_Error (Params, Request_Error);
               end if;
            end;

            Rose.Kernel.Processes.Set_Current_State
              (Pid, Rose.Kernel.Processes.Ready);

         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Pages;
