with Rose.Limits;
with Rose.Words;

with Rose.Kernel.Capabilities.Pages;
with Rose.Kernel.Physical_Memory;
with Rose.Kernel.Processes;

with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Physical_Memory is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Invocation;
      use Rose.Words;
      Process_Id : constant Rose.Kernel.Processes.Process_Id :=
                     Rose.Kernel.Processes.Current_Process_Id;

      function Get_User_Region_Count return Natural
        renames Rose.Kernel.Physical_Memory.Get_User_Region_Count;

   begin
      case Cap.Header.Endpoint is
         when Reserve_Device_Memory =>
            if Params.Control.Flags (Send_Words)
              and then Params.Control.Last_Sent_Word = 1
            then
               declare
                  Success : Boolean;
               begin
                  Rose.Kernel.Physical_Memory.Allocate_Region
                    (Base    =>
                       Rose.Addresses.Physical_Address (Params.Data (0)),
                     Bound   =>
                       Rose.Addresses.Physical_Address (Params.Data (1)),
                     Class   => Rose.Kernel.Physical_Memory.Device,
                     Usage   => Rose.Kernel.Physical_Memory.Device_Pages,
                     Success => Success);
                  if not Success then
                     Rose.Boot.Console.Put ("reserving ");
                     Rose.Boot.Console.Put (Params.Data (0));
                     Rose.Boot.Console.Put (" - ");
                     Rose.Boot.Console.Put (Params.Data (1));
                     Rose.Boot.Console.Put (" failed");
                     Rose.Boot.Console.New_Line;
                     return;
                  end if;
               end;
            else
               null;
            end if;

         when Get_Region_Count =>

            Params.Control.Flags :=
              (Rose.Invocation.Reply      => True,
               Rose.Invocation.Send_Words => True,
               others                     => False);
            Params.Control.Last_Sent_Word := 0;

            Params.Data (0) :=
              Rose.Words.Word
                (Rose.Kernel.Physical_Memory.Get_User_Region_Count);

            Rose.Kernel.Processes.Set_Current_State
              (Rose.Kernel.Processes.Current_Process_Id,
               Rose.Kernel.Processes.Ready);

         when Get_Region =>
            if Params.Data (0) in 1 .. Word (Get_User_Region_Count) then
               declare
                  use Rose.Objects;
                  use Rose.Kernel.Physical_Memory;
                  use Rose.Capabilities.Layout;
                  Base    : Rose.Addresses.Physical_Page_Address;
                  Bound   : Rose.Addresses.Physical_Page_Address;
                  Caps    : Rose.Capabilities.Capability_Array (1 .. 3);
               begin

                  Rose.Kernel.Processes.Create_Caps
                    (Rose.Kernel.Processes.Current_Process_Id, Caps);

                  declare
                     Map_Cap : constant Rose.Capabilities.Capability :=
                                 Caps (1);
                     Unmap_Cap : constant Rose.Capabilities.Capability :=
                                   Caps (2);
                     Set_Cap   : constant Rose.Capabilities.Capability :=
                                   Caps (3);

                     function Layout (Endpoint : Endpoint_Index)
                                      return Capability_Layout
                     is (Header  =>
                           (Cap_Type  => Page_Table_Cap,
                            Endpoint  => Endpoint,
                            others    => <>),
                         Payload =>
                            Rose.Objects.Object_Id (Base) * 2 ** 32
                         + Rose.Objects.Object_Id (Bound));

                  begin
                     Get_User_Region (Positive (Params.Data (0)), Base, Bound);

                     for I in Caps'Range loop
                        Rose.Kernel.Processes.Set_Cap
                          (Process_Id, Caps (I),
                           Layout (Endpoint_Index (I)));
                     end loop;

                     Params.Control.Flags :=
                       (Rose.Invocation.Reply      => True,
                        Rose.Invocation.Send_Words => True,
                        Rose.Invocation.Send_Caps  => True,
                        others                     => False);

                     Params.Control.Last_Sent_Word := 1;
                     Params.Control.Last_Sent_Cap := 2;

                     Params.Data (0) := Rose.Words.Word (Base);
                     Params.Data (1) := Rose.Words.Word (Bound);
                     Params.Caps (0) := Map_Cap;
                     Params.Caps (1) := Unmap_Cap;
                     Params.Caps (2) := Set_Cap;

                     Rose.Kernel.Processes.Set_Current_State
                       (Rose.Kernel.Processes.Current_Process_Id,
                        Rose.Kernel.Processes.Ready);
                  end;
               end;
            end if;

         when Device_Page =>
            declare
               use type Rose.Capabilities.Capability;
               use type Rose.Objects.Object_Id;
               Page    : constant Word := Params.Data (0);
               New_Cap : Rose.Capabilities.Capability;
            begin
               if not Params.Control.Flags (Send_Words)
                 or else Params.Control.Last_Sent_Word /= 0
                 or else Page > Rose.Limits.Max_Physical_Pages
                 or else Rose.Kernel.Physical_Memory.Is_RAM
                   (Rose.Addresses.Physical_Page_Address (Page))
               then
                  Rose.Kernel.Processes.Return_Error (Params, Request_Error);
               else
                  New_Cap := Rose.Kernel.Processes.Create_Cap (Process_Id);
                  if New_Cap = Rose.Capabilities.Null_Capability then
                     Rose.Kernel.Processes.Return_Error
                       (Params, Out_Of_Capabilities);
                  else
                     Rose.Kernel.Processes.Set_Cap
                       (Pid    => Process_Id,
                        Cap    => New_Cap,
                        Layout =>
                          (Header  =>
                             (Cap_Type =>
                                  Rose.Capabilities.Layout.Page_Object_Cap,
                              Endpoint =>
                                Rose.Kernel.Capabilities.Pages
                                  .Identity_Map_Page,
                              others   => <>),
                           Payload =>
                             Rose.Objects.Page_Object_Id'First
                           + Rose.Objects.Object_Id (Page)));
                     Params.Control.Flags :=
                       (Reply => True, Send_Caps => True, others => False);
                     Params.Control.Last_Sent_Cap := 0;
                     Params.Caps (0) := New_Cap;
                  end if;
               end if;

               Rose.Kernel.Processes.Set_Current_State
                 (Process_Id, Rose.Kernel.Processes.Ready);
            end;

         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Physical_Memory;
