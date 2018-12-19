with Rose.Words;

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

         when others =>
            null;
      end case;

   end Handle;

end Rose.Kernel.Capabilities.Physical_Memory;
