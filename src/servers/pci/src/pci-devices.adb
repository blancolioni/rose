with Rose.Console_IO;

package body PCI.Devices is

   Table : PCI_Device_Table;

   ----------------
   -- Add_Device --
   ----------------

   procedure Add_Device
     (Bus        : Rose.Devices.PCI.Bus_Type;
      Slot       : Rose.Devices.PCI.Device_Type;
      Fn         : Rose.Devices.PCI.Function_Type;
      Vendor     : Rose.Words.Word_16;
      Device     : Rose.Words.Word_16;
      Device_EP  : Rose.Objects.Endpoint_Id;
      Device_Cap : Rose.Capabilities.Capability)
   is
      use Rose.Words;
      Address : constant Rose.Devices.PCI.Device_Address :=
                  Rose.Devices.PCI.To_Address (Bus, Slot, Fn);
   begin
      if PCI_Device_Count < Max_PCI_Devices then
         PCI_Device_Count := PCI_Device_Count + 1;
         Rose.Console_IO.Put (Word_8 (Bus));
         Rose.Console_IO.Put (":");
         Rose.Console_IO.Put (Word_8 (Slot));
         Rose.Console_IO.Put (".");
         Rose.Console_IO.Put (Natural (Fn));
         Rose.Console_IO.Put (" ");
         Rose.Console_IO.Put (Rose.Words.Word_32 (Address));
         Rose.Console_IO.Put ("    ");
         Rose.Console_IO.Put (Vendor);
         Rose.Console_IO.Put (":");
         Rose.Console_IO.Put (Device);
         Rose.Console_IO.Put (" cap ");
         Rose.Console_IO.Put (Rose.Words.Word_8 (Device_Cap));
         Rose.Console_IO.New_Line;

         Table (PCI_Device_Count) :=
           PCI_Device_Record'
             (Active  => True,
              Address => Address,
              Bus     => Bus,
              Slot    => Slot,
              Fn      => Fn,
              Vendor  => Vendor,
              Device  => Device,
              EP      => Device_EP,
              Cap     => Device_Cap);
      end if;
   end Add_Device;

   ---------------------
   -- Find_Device_Cap --
   ---------------------

   function Find_Device_Cap
     (Vendor, Device : Rose.Words.Word_16)
      return Rose.Capabilities.Capability
   is
   begin
      for Index in 1 .. PCI_Device_Count loop
         declare
            use type Rose.Words.Word_16;
            Item : PCI_Device_Record renames Table (Index);
         begin
            if Item.Vendor = Vendor and then Item.Device = Device then
               return Item.Cap;
            end if;
         end;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Find_Device_Cap;

   -----------------
   -- Get_Address --
   -----------------

   procedure Get_Device
     (Device_EP  : Rose.Objects.Endpoint_Id;
      Found      : out Boolean;
      Bus        : out Rose.Devices.PCI.Bus_Type;
      Slot       : out Rose.Devices.PCI.Device_Type;
      Fn         : out Rose.Devices.PCI.Function_Type)
   is
   begin
      Found := False;
      for Index in 1 .. PCI_Device_Count loop
         declare
            use type Rose.Objects.Endpoint_Id;
            Item : PCI_Device_Record renames Table (Index);
         begin
            if Item.EP = Device_EP then
               Found := True;
               Bus := Item.Bus;
               Slot := Item.Slot;
               Fn := Item.Fn;
               exit;
            end if;
         end;
      end loop;
   end Get_Device;

--     ---------------
--     -- Read_Word --
--     ---------------
--
--     function Read_Word
--       (Bus  : Bus_Number;
--        Slot : Device_Number;
--        Fn   : Function_Number;
--        Reg  : Register_Number)
--        return Rose.Words.Word_32
--     is
--        use Rose.Words;
--        Address : constant Word_32 :=
--                    2 ** 31
--                    + Word_32 (Bus) * 2 ** 16
--                    + Word_32 (Slot) * 2 ** 11
--                    + Word_32 (Fn) * 2 ** 8
--                    + Word_32 (Reg) * 4;
--        Data    : Word_32;
--     begin
--        Data := PCI.Calls.Port_Out_In (Address);
--        return Data;
--     end Read_Word;

end PCI.Devices;
