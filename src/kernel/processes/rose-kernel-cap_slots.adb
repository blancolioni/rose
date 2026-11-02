package body Rose.Kernel.Cap_Slots
  with SPARK_Mode => Off
is

   ----------
   -- Read --
   ----------

   function Read
     (Page : System.Address;
      Slot : Slot_Index)
      return Rose.Capabilities.Layout.Capability_Layout
   is
      Contents : Capability_Page
        with Import, Address => Page;
   begin
      return Contents (Slot);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Page   : System.Address;
      Slot   : Slot_Index;
      Layout : Rose.Capabilities.Layout.Capability_Layout)
   is
      Contents : Capability_Page
        with Import, Address => Page;
   begin
      Contents (Slot) := Layout;
   end Write;

end Rose.Kernel.Cap_Slots;
