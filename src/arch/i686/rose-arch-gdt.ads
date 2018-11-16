package Rose.Arch.GDT is

   procedure Create_GDT;
   pragma Export (C, Create_GDT, "create_gdt");

   procedure Set_Global_Descriptor
     (Entry_Index     : Rose.Words.Word_8;
      Base            : Rose.Words.Word_32;
      Limit           : Rose.Words.Word_32;
      Descriptor_Type : Rose.Words.Word_8);

end Rose.Arch.GDT;
