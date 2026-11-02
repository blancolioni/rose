with System;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Capabilities.Layout;

package Rose.Kernel.Cap_Slots
  with SPARK_Mode,
       Abstract_State => Slot_Memory,
       Initializes    => Slot_Memory
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   --  The two places where the capability tables (Rose.Kernel.Cap_Tables)
   --  touch memory through an address (ADR 0023).  A layout is a slot of a
   --  page, which is a page of the kernel heap, and is read and written
   --  here and nowhere else.  They are not in SPARK, and are a few lines
   --  each, so what the tables are proved from is what these say, which is
   --  that a slot reads as what was last written to it, and that writing
   --  one changes no other.

   use type System.Address;
   use type Rose.Capabilities.Capability;
   use type Rose.Capabilities.Layout.Capability_Layout;

   Capabilities_Per_Page : constant :=
                   Rose.Addresses.Physical_Page_Bytes
                 / Rose.Capabilities.Layout.Capability_Layout_Bytes;

   subtype Slot_Index is
     Rose.Capabilities.Capability range 1 .. Capabilities_Per_Page;

   type Capability_Page is
     array (Slot_Index) of Rose.Capabilities.Layout.Capability_Layout
       with Size => Rose.Addresses.Physical_Page_Bits;

   --  What the memory holds, which only the proof sees.

   type Memory_Image is private;

   function Image return Memory_Image
   with Ghost,
        Import,
        Global => (Input => Slot_Memory);
   --  All of the memory that holds layouts, now.

   function Value
     (Memory : Memory_Image;
      Page   : System.Address;
      Slot   : Slot_Index)
      return Rose.Capabilities.Layout.Capability_Layout
   with Ghost,
        Import,
        Global => null;
   --  The layout in a slot of the page at Page.

   function Updated
     (Memory : Memory_Image;
      Page   : System.Address;
      Slot   : Slot_Index;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
      return Memory_Image
   with Ghost,
        Import,
        Global => null;
   --  Memory, with a layout written to a slot.

   procedure Lemma_Updated_Slot
     (Memory : Memory_Image;
      Page   : System.Address;
      Slot   : Slot_Index;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   with Ghost,
        Import,
        Global => null,
        Always_Terminates,
        Post   => Value (Updated (Memory, Page, Slot, Layout), Page, Slot)
                  = Layout;
   --  A slot holds what was written to it.

   procedure Lemma_Updated_Other
     (Memory     : Memory_Image;
      Page       : System.Address;
      Slot       : Slot_Index;
      Layout     : Rose.Capabilities.Layout.Capability_Layout;
      Other_Page : System.Address;
      Other_Slot : Slot_Index)
   with Ghost,
        Import,
        Global => null,
        Always_Terminates,
        Pre    => Other_Page /= Page or else Other_Slot /= Slot,
        Post   => Value (Updated (Memory, Page, Slot, Layout),
                         Other_Page, Other_Slot)
                  = Value (Memory, Other_Page, Other_Slot);
   --  Writing a slot changes no other slot.

   function Read
     (Page : System.Address;
      Slot : Slot_Index)
      return Rose.Capabilities.Layout.Capability_Layout
   with Global => (Input => Slot_Memory),
        Pre    => Page /= System.Null_Address,
        Post   => Read'Result = Value (Image, Page, Slot);

   procedure Write
     (Page   : System.Address;
      Slot   : Slot_Index;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   with Global => (In_Out => Slot_Memory),
        Pre    => Page /= System.Null_Address,
        Post   => Image = Updated (Image'Old, Page, Slot, Layout);

private

   type Memory_Image is new Natural;
   --  Only that two images can be told apart: a record with nothing in it
   --  would make every image equal to every other, and the contracts of
   --  Write say nothing.

end Rose.Kernel.Cap_Slots;
