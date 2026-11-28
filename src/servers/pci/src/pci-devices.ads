with Rose.Objects;
with Rose.Words;

with Rose.Devices.PCI;

package PCI.Devices is

   --  The PCI functions that the scan found, in the order of their bus,
   --  slot and function. Each is named by its identifier: its bus, slot
   --  and function as 16 bits, which is the identifier of its caps (#292).

   Max_PCI_Devices : constant := 32;

   type Device_Count is range 0 .. Max_PCI_Devices;
   subtype Device_Index is Device_Count range 1 .. Device_Count'Last;

   type BAR_Index is range 0 .. 5;

   type BAR_Record is
      record
         Is_IO      : Boolean := False;
         Base       : Rose.Words.Word_32 := 0;
         Size       : Rose.Words.Word_32 := 0;
         Upper_Half : Boolean := False;
      end record;
   --  A decoded base address register. Size is 0 if the BAR isn't used,
   --  or is memory above 4 GB. Upper_Half is True for the second register
   --  of a 64-bit BAR, which isn't a BAR of its own.

   type BAR_Array is array (BAR_Index) of BAR_Record;

   No_Interrupt : constant := 255;

   type Device_Record is
      record
         Bus            : Rose.Devices.PCI.Bus_Type;
         Slot           : Rose.Devices.PCI.Device_Type;
         Fn             : Rose.Devices.PCI.Function_Type;
         Vendor         : Rose.Words.Word_32;
         Device         : Rose.Words.Word_32;
         BARs           : BAR_Array;
         Interrupt_Line : Natural;
      end record;

   function Identifier
     (Item : Device_Record)
      return Rose.Objects.Capability_Identifier;

   procedure Add_Device (Item : Device_Record);
   --  Add Item, unless the table is full, which is reported.

   function Find
     (Vendor, Device : Rose.Words.Word_32;
      Index          : Natural)
      return Device_Count;
   --  The table index of the device with these ids that comes Index
   --  after the first, or 0.

   function Find
     (Id : Rose.Objects.Capability_Identifier)
      return Device_Count;
   --  The table index of the device whose identifier is Id, or 0.

   function Get (Index : Device_Index) return Device_Record;

end PCI.Devices;
