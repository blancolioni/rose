with Rose.Console_IO;

package body PCI.Devices is

   Table : array (Device_Index) of Device_Record;
   Count : Device_Count := 0;

   ----------------
   -- Add_Device --
   ----------------

   procedure Add_Device (Item : Device_Record) is
      use Rose.Words;
   begin
      if Count = Device_Count'Last then
         Rose.Console_IO.Put_Line ("pci: too many devices");
         return;
      end if;

      Count := Count + 1;
      Table (Count) := Item;

      Rose.Console_IO.Put (Word_8 (Item.Bus));
      Rose.Console_IO.Put (":");
      Rose.Console_IO.Put (Word_8 (Item.Slot));
      Rose.Console_IO.Put (".");
      Rose.Console_IO.Put (Natural (Item.Fn));
      Rose.Console_IO.Put (" ");
      Rose.Console_IO.Put (Word_16 (Item.Vendor));
      Rose.Console_IO.Put (":");
      Rose.Console_IO.Put (Word_16 (Item.Device));
      if Item.Interrupt_Line /= No_Interrupt then
         Rose.Console_IO.Put (" irq ");
         Rose.Console_IO.Put (Item.Interrupt_Line);
      end if;
      Rose.Console_IO.New_Line;
   end Add_Device;

   ----------
   -- Find --
   ----------

   function Find
     (Vendor, Device : Rose.Words.Word_32;
      Index          : Natural)
      return Device_Count
   is
      use type Rose.Words.Word_32;
      Seen : Natural := 0;
   begin
      for I in 1 .. Count loop
         if Table (I).Vendor = Vendor and then Table (I).Device = Device then
            if Seen = Index then
               return I;
            end if;
            Seen := Seen + 1;
         end if;
      end loop;
      return 0;
   end Find;

   ----------
   -- Find --
   ----------

   function Find
     (Id : Rose.Objects.Capability_Identifier)
      return Device_Count
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      for I in 1 .. Count loop
         if Identifier (Table (I)) = Id then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   ---------
   -- Get --
   ---------

   function Get (Index : Device_Index) return Device_Record is
   begin
      return Table (Index);
   end Get;

   ----------------
   -- Identifier --
   ----------------

   function Identifier
     (Item : Device_Record)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      return Rose.Objects.Capability_Identifier (Item.Bus) * 256
        + Rose.Objects.Capability_Identifier (Item.Slot) * 8
        + Rose.Objects.Capability_Identifier (Item.Fn);
   end Identifier;

end PCI.Devices;
