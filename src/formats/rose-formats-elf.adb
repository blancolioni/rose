with Ada.Unchecked_Conversion;

with Rose.Addresses;
with Rose.Boot.Console;

package body Rose.Formats.ELF is

   procedure Report (Header : Elf_Header);
   procedure Report (Header : Program_Header);

   -------------------
   -- Get_Elf_Class --
   -------------------

   function Get_Elf_Class (Header : Elf_Header) return Elf_Class is
   begin
      case Header.E_Ident (4) is
         when 1 =>
            return Elf_Class_32;
         when 2 =>
            return Elf_Class_64;
         when others =>
            return Elf_Class_None;
      end case;
   end Get_Elf_Class;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Image : Elf_Image) return Boolean is
      Header : Elf_Header;
      pragma Import (Ada, Header);
      for Header'Address use Image.Base;
   begin
      return Valid (Header);
   end Is_Valid;

   ----------
   -- Load --
   ----------

   function Load
     (Base   : System.Address;
      Length : System.Storage_Elements.Storage_Count)
      return Elf_Image
   is
      use System.Storage_Elements;
      function To_Address is
        new Ada.Unchecked_Conversion (Word, System.Address);
      function To_Word is
        new Ada.Unchecked_Conversion (System.Address, Word);

      Header : Elf_Header;
      pragma Import (Ada, Header);
      for Header'Address use Base;
      Image  : System.Storage_Elements.Storage_Array (1 .. Length);
      pragma Import (Ada, Image);
      for Image'Address use Base;
      PHs    : array (1 .. Header.E_Phnum) of Program_Header;
      pragma Import (Ada, PHs);
      for PHs'Address use To_Address (To_Word (Base) + Header.E_Phoff);
   begin
      if False then
         Report (Header);
         for Header of PHs loop
            Report (Header);
         end loop;
      end if;

      return (Base, Length);
   end Load;

   ------------
   -- Report --
   ------------

   procedure Report (Header : Elf_Header) is
      use Rose.Boot.Console;
   begin
      Put ("E_Ident:     ");
      for I in Header.E_Ident'Range loop
         Put (" ");
         Put (Rose.Words.Word_8 (Header.E_Ident (I)));
      end loop;
      New_Line;
      Put ("E_Type:       ");
      Put (Rose.Words.Word_8 (Header.E_Type));
      New_Line;
      Put ("E_Machine:    ");
      Put (Rose.Words.Word_8 (Header.E_Machine));
      New_Line;
      Put ("E_Version:    ");
      Put (Rose.Words.Word_8 (Header.E_Version));
      New_Line;
      Put ("E_Entry:      ");
      Put (Rose.Words.Word_32 (Header.E_Entry));
      New_Line;
      Put ("E_Phoff:      ");
      Put (Header.E_Phoff);
      New_Line;
      Put ("E_Phnum:      ");
      Put (Rose.Words.Word_8 (Header.E_Phnum));
      New_Line;
   end Report;

   ------------
   -- Report --
   ------------

   procedure Report (Header : Program_Header) is
      use Rose.Boot.Console;
   begin
      Put (Word_8 (Header.P_Type));
      Put (" ");
      Put (Word_8 (Header.P_Flags));
      Put (" ");
      Put (Header.P_Offset);
      Put (" ");
      Put (Word_32 (Header.P_VAddr));
      Put (" ");
      Put (Header.P_FileSz);
      Put (" ");
      Put (Header.P_MemSz);
      New_Line;
   end Report;

   --------------------------
   -- Scan_Program_Headers --
   --------------------------

   procedure Scan_Program_Headers
     (Image   : Elf_Image;
      Process : not null access
        procedure (Segment_Type : Program_Header_Type;
                   File_Offset  : Rose.Formats.ELF.Elf_Addr;
                   V_Address    : Rose.Formats.ELF.Elf_Addr;
                   File_Size    : Rose.Words.Word_32;
                   Memory_Size  : Rose.Words.Word_32;
                   Alignment    : Rose.Words.Word_32;
                   Readable     : Boolean;
                   Writable     : Boolean;
                   Executable   : Boolean))
   is
      use Rose.Addresses;
      Header : Elf_Header;
      pragma Import (Ada, Header);
      for Header'Address use Image.Base;
      PH_Addr : constant System.Address :=
                  System'To_Address
                    (To_Virtual_Address (Image.Base)
                     + Virtual_Bytes (Header.E_Phoff));
      PH_Count : constant Word_16 := Header.E_Phnum;
      PHs      : array (1 .. PH_Count) of Program_Header;
      pragma Import (Ada, PHs);
      for PHs'Address use PH_Addr;
   begin
      for PH of PHs loop
         Process (Segment_Type => PH.P_Type,
                  File_Offset  => PH.P_Offset,
                  V_Address    => PH.P_VAddr,
                  File_Size    => PH.P_FileSz,
                  Memory_Size  => PH.P_MemSz,
                  Alignment    => PH.P_Align,
                  Readable     => (PH.P_Flags and 4) = 4,
                  Writable     => (PH.P_Flags and 2) = 2,
                  Executable   => (PH.P_Flags and 1) = 1);
      end loop;
   end Scan_Program_Headers;

   -------------------
   -- Start_Address --
   -------------------

   function Start_Address (Image : Elf_Image) return Elf_Addr is
      Header : Elf_Header;
      pragma Import (Ada, Header);
      for Header'Address use Image.Base;
   begin
      return Header.E_Entry;
   end Start_Address;

   -----------
   -- Valid --
   -----------

   function Valid (Header : Elf_Header) return Boolean is
   begin
      return Header.E_Ident (0 .. 3) = (16#7F#, 16#45#, 16#4C#, 16#46#)
        and then Header.E_Ident (4) in 1 .. 2;
   end Valid;

end Rose.Formats.ELF;
