with Rose.Invocation;
with Rose.Objects;

with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Rose.Console_IO;
with Rose.Words;

with Rose.Devices.PCI.Server;

with PCI.Devices;

package body PCI.Server is

   procedure Create_Caps is
   begin
      Console_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Rose.Console_IO.Open (Console_Cap);
      Rose.Console_IO.Put_Line ("pci: starting");

      Command_Port_Out :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Data_Port_Out :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Data_Port_In :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

   end Create_Caps;

   ------------------
   -- Scan_Devices --
   ------------------

   procedure Scan_Devices is
      Current_Endpoint : Rose.Objects.Endpoint_Id :=
                            Rose.Devices.PCI.PCI_Base_Device_Endpoint_Id;
   begin
      for Bus in Rose.Devices.PCI.Bus_Type loop
         for Slot in Rose.Devices.PCI.Device_Type loop
            declare
               use type Rose.Words.Word_16;
               Vendor : constant Rose.Words.Word_16 :=
                          Rose.Devices.PCI.Server.Get
                            (Command_Port_Out, Data_Port_In,
                             Bus, Slot, 0, 0);
            begin

               if Vendor /= Rose.Devices.PCI.Invalid_Vendor_Id then
                  for Fn in Rose.Devices.PCI.Function_Type range 0 .. 1 loop
                     declare
                        use type Rose.Objects.Endpoint_Id;
                        Device : constant Rose.Words.Word_16 :=
                                   Rose.Devices.PCI.Server.Get
                                     (Command_Port_Out, Data_Port_In,
                                      Bus, Slot, Fn, 1);
                     begin
                        if Device /= Rose.Devices.PCI.Invalid_Device_Id then
                           PCI.Devices.Add_Device
                             (Bus        => Bus,
                              Slot       => Slot,
                              Fn         => Fn,
                              Vendor     => Vendor,
                              Device     => Device,
                              Device_EP  => Current_Endpoint,
                              Device_Cap =>
                                Rose.System_Calls.Server.Create_Endpoint
                                  (Create_Endpoint_Cap, Current_Endpoint));
                           Current_Endpoint := Current_Endpoint + 1;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
      end loop;
   end Scan_Devices;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      use Rose.Words;
      use Rose.Invocation;
      use type Rose.Objects.Endpoint_Id;
      Params      : aliased Rose.Invocation.Invocation_Record;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Endpoint_Cap);
      PCI_Cap     : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Endpoint
                        (Create_Endpoint_Cap,
                         Rose.Devices.PCI.PCI_Endpoint_Id);
      pragma Unreferenced (PCI_Cap);
   begin

      loop
         Params.Control.Flags :=
           (Receive     => True,
            Block       => True,
            Recv_Words  => True,
            others      => False);
         Params.Control.Last_Recv_Word := Parameter_Word_Index'Last;
         Params.Cap := Receive_Cap;

         Rose.System_Calls.Invoke_Capability (Params);

         if Params.Control.Flags (Error) then
            Rose.Console_IO.Put ("error: ");
            Rose.Console_IO.Put (Params.Data (0));
            Rose.Console_IO.New_Line;
            return;
         end if;

         if Params.Endpoint = Rose.Devices.PCI.PCI_Endpoint_Id then
            declare
               use Rose.Capabilities;
               Vendor  : constant Rose.Words.Word_16 :=
                           Rose.Words.Word_16 (Params.Data (0));
               Device  : constant Rose.Words.Word_16 :=
                           Rose.Words.Word_16 (Params.Data (1));
               Cap     : constant Rose.Capabilities.Capability :=
                           PCI.Devices.Find_Device_Cap
                             (Vendor, Device);
            begin
               if Cap = Null_Capability then
                  Rose.Console_IO.Put ("pci: cannot find device ");
                  Rose.Console_IO.Put (Vendor);
                  Rose.Console_IO.Put (" ");
                  Rose.Console_IO.Put (Device);
                  Rose.Console_IO.New_Line;
                  Params.Control.Flags :=
                    (Reply     => True,
                     Error     => True,
                     others    => False);
               else
                  Params.Control.Flags :=
                    (Reply     => True,
                     Send_Caps => True,
                     others    => False);
                  Params.Control.Last_Sent_Cap := 0;
                  Params.Caps (0) := Cap;
               end if;
               Params.Cap := Params.Reply_Cap;
               Rose.System_Calls.Invoke_Capability (Params);
            end;
         else
            declare
               Found : Boolean;
               Bus   : Rose.Devices.PCI.Bus_Type;
               Slot  : Rose.Devices.PCI.Device_Type;
               Fn    : Rose.Devices.PCI.Function_Type;
               Data  : Word_32;
            begin
               PCI.Devices.Get_Device
                 (Params.Endpoint, Found, Bus, Slot, Fn);

               if Found then
                  Data :=
                    Rose.Devices.PCI.Server.Get
                      (Command_Port_Out, Data_Port_In,
                       Bus, Slot, Fn,
                       Rose.Devices.PCI.Register_32 (Params.Data (0)));
                  Params.Control.Flags :=
                    (Reply      => True,
                     Send_Words => True,
                     others     => False);
                  Params.Control.Last_Sent_Word := 0;
                  Params.Data (0) := Data;
                  Params.Cap := Params.Reply_Cap;
                  Rose.System_Calls.Invoke_Capability (Params);

               else
                  Rose.Console_IO.Put
                    ("pci: unrecognised endpoint: ");
                  Rose.Console_IO.Put (Rose.Words.Word (Params.Cap));
                  Rose.Console_IO.New_Line;
               end if;
            end;
         end if;
      end loop;
   end Start_Server;

end PCI.Server;
