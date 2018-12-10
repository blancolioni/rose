with System.Storage_Elements;
with Rose.Words;

package Elf.Format is

   subtype Elf_Address is Rose.Words.Word_32;

   type E_Identifier is
     new System.Storage_Elements.Storage_Array (0 .. 15);
   for E_Identifier'Size use 128;

   Magic : constant E_Identifier :=
             (16#7f#, 16#45#, 16#4c#, 16#46#, others => 0);

   type E_Types is new Rose.Words.Word_16;
   ET_None   : constant E_Types := 0;
   ET_Rel    : constant E_Types := 1;
   ET_Exec   : constant E_Types := 2;
   ET_Dyn    : constant E_Types := 3;
   ET_Core   : constant E_Types := 4;
   ET_Loproc : constant E_Types := 16#FF00#;
   ET_Hiproc : constant E_Types := 16#FFFF#;

   type E_Machines is new Rose.Words.Word_16;
   EM_None   : constant E_Machines := 0;
   EM_M32    : constant E_Machines := 1;
   EM_SPARC  : constant E_Machines := 2;
   EM_386    : constant E_Machines := 3;
   EM_68K    : constant E_Machines := 4;
   EM_88K    : constant E_Machines := 5;
   EM_860    : constant E_Machines := 6;
   EM_MIPS   : constant E_Machines := 7;
   EM_X86_64 : constant E_Machines := 62;

   type Elf_Class is
     (Elf_Class_None, Elf_Class_32, Elf_Class_64);

   type Section_Offset is new Rose.Words.Word_32;

   type Elf_Header is
      record
         E_Ident     : E_Identifier;
         E_Type      : E_Types;
         E_Machine   : E_Machines;
         E_Version   : Rose.Words.Word_32;
         E_Entry     : Elf_Address;
         E_Phoff     : Rose.Words.Word_32;
         E_Shoff     : Section_Offset;
         E_Flags     : Rose.Words.Word_32;
         E_Ehsize    : Rose.Words.Word_16;
         E_Phentsize : Rose.Words.Word_16;
         E_Phnum     : Rose.Words.Word_16;
         E_Shentsize : Rose.Words.Word_16;
         E_Shnum     : Rose.Words.Word_16;
         E_Shstrndx  : Rose.Words.Word_16;
      end record
     with Size => 16#34# * 8;

   function Valid (Header : Elf_Header) return Boolean;

   --  Special section offsets
   SHN_Undefined : constant Section_Offset := 0;
   SHN_Loreserve : constant Section_Offset := 16#FF00#;
   SHN_Loproc    : constant Section_Offset := 16#FF00#;
   SHN_Hiproc    : constant Section_Offset := 16#FF1F#;
   SHN_Abs       : constant Section_Offset := 16#FFF1#;
   SHN_Common    : constant Section_Offset := 16#FFF2#;
   SHN_Hireserve : constant Section_Offset := 16#FFFF#;

   type Section_Header is
      record
         Sh_Name       : Rose.Words.Word_32;
         Sh_Type       : Rose.Words.Word_32;
         Sh_Flags      : Rose.Words.Word_32;
         Sh_Addr       : Rose.Words.Word_32;
         Sh_Off        : Section_Offset;
         Sh_Size       : Rose.Words.Word_32;
         Sh_Link       : Rose.Words.Word_32;
         Sh_Info       : Rose.Words.Word_32;
         Sh_Addr_Align : Rose.Words.Word_32;
         Sh_Ent_Size   : Rose.Words.Word_32;
      end record;

   --  constants for section types
   Sht_Null      : constant := 0;
   Sht_Progbits  : constant := 1;
   Sht_Symtab    : constant := 2;
   Sht_Strtab    : constant := 3;
   Sht_Rela      : constant := 4;
   Sht_Hash      : constant := 5;
   Sht_Dynamic   : constant := 6;
   Sht_Note      : constant := 7;
   Sht_Nobits    : constant := 8;
   Sht_Rel       : constant := 9;
   Sht_Shlib     : constant := 10;
   Sht_Dynsym    : constant := 11;
   Sht_Loproc    : constant := 16#7000_0000#;
   Sht_Hiproc    : constant := 16#7FFF_FFFF#;
   Sht_Louser    : constant := 16#8000_0000#;
   Sht_Hiuser    : constant := 16#FFFF_FFFF#;

   --  constants for section attribute flags
   SHF_WRITE     : constant := 16#01#;
   SHF_ALLOC     : constant := 16#02#;
   SHF_EXECINSTR : constant := 16#04#;
   SHF_MASKPROC  : constant := 16#F000_000#;

   type Program_Header_Type is new Rose.Words.Word_32;

   --  constants for P_Type
   PT_NULL    : constant Program_Header_Type := 0;
   PT_LOAD    : constant Program_Header_Type := 1;
   PT_DYNAMIC : constant Program_Header_Type := 2;
   PT_INTERP  : constant Program_Header_Type := 3;
   PT_NOTE    : constant Program_Header_Type := 4;
   PT_SHLIB   : constant Program_Header_Type := 5;
   PT_PHDR    : constant Program_Header_Type := 6;
   PT_LOPROC  : constant Program_Header_Type := 16#7000_0000#;
   PT_HIPROC  : constant Program_Header_Type := 16#7FFF_FFFF#;

   type Program_Header is
      record
         P_Type    : Program_Header_Type;
         P_Offset  : Rose.Words.Word_32;
         P_VAddr   : Elf_Address;
         P_PAddr   : Elf_Address;
         P_FileSz  : Rose.Words.Word_32;
         P_MemSz   : Rose.Words.Word_32;
         P_Flags   : Rose.Words.Word_32;
         P_Align   : Rose.Words.Word_32;
      end record;

private

   function Valid (Header : Elf_Header) return Boolean
   is (Header.E_Ident (0 .. 3) = Magic (0 .. 3));

end Elf.Format;
