with System.Storage_Elements;

with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Elf.Format;

with Rose.Console_IO;

package body Elf.Loader is

   use Rose.Interfaces.Region.Client;
   use Rose.Interfaces.Process_Memory.Client;
   use Rose.Interfaces.Storage.Client;

   procedure Read
     (Region : Region_Client;
      Base   : Rose.Objects.Object_Id;
      Offset : System.Storage_Elements.Storage_Count;
      Item   : System.Address;
      Size   : System.Storage_Elements.Storage_Count);

   procedure Read_Header
     (Region : Region_Client;
      Base   : Rose.Objects.Object_Id;
      Header : out Elf.Format.Elf_Header);

   procedure Scan_Program_Headers
     (Store   : Storage_Client;
      Region  : Region_Client;
      Base    : Rose.Objects.Object_Id;
      Process : Process_Memory_Client;
      Header  : Elf.Format.Elf_Header);

   procedure Process_Program_Header
     (Top_Store    : Storage_Client;
      Image_Region : Region_Client;
      Process      : Process_Memory_Client;
      Base_Object  : Rose.Objects.Object_Id;
      Segment_Type : Elf.Format.Program_Header_Type;
      File_Offset  : Elf.Format.Elf_Address;
      V_Address    : Elf.Format.Elf_Address;
      File_Size    : Rose.Words.Word_32;
      Memory_Size  : Rose.Words.Word_32;
      Readable     : Boolean;
      Writable     : Boolean;
      Executable   : Boolean);

   --------------------
   -- Load_Elf_Image --
   --------------------

   procedure Load_Elf_Image
     (Process : Rose.Interfaces.Process_Memory.Client.Process_Memory_Client;
      Store   : Rose.Interfaces.Storage.Client.Storage_Client;
      Image   : Rose.Interfaces.Region.Client.Region_Client;
      Success : out Boolean)
   is
      Header      : Elf.Format.Elf_Header;
      Base, Bound : Rose.Objects.Object_Id;
   begin
      Get_Range (Image, Base, Bound);
      Success := True;
      Read_Header (Image, Base, Header);
      if not Elf.Format.Valid (Header) then
         Rose.Console_IO.Put_Line ("elf: invalid header");
         Success := False;
         return;
      end if;

      Rose.Console_IO.Put ("elf: base  ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Base));
      Rose.Console_IO.New_Line;
      Rose.Console_IO.Put ("elf: bound ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Bound));
      Rose.Console_IO.New_Line;


      Scan_Program_Headers (Store, Image, Base, Process, Header);

   end Load_Elf_Image;

   ----------------------------
   -- Process_Program_Header --
   ----------------------------

   procedure Process_Program_Header
     (Top_Store    : Storage_Client;
      Image_Region : Region_Client;
      Process      : Process_Memory_Client;
      Base_Object  : Rose.Objects.Object_Id;
      Segment_Type : Elf.Format.Program_Header_Type;
      File_Offset  : Elf.Format.Elf_Address;
      V_Address    : Elf.Format.Elf_Address;
      File_Size    : Rose.Words.Word_32;
      Memory_Size  : Rose.Words.Word_32;
      Readable     : Boolean;
      Writable     : Boolean;
      Executable   : Boolean)
   is
      use type Rose.Objects.Object_Id;
      use Rose.Words;
      use Elf.Format;
      Segment_Base  : Rose.Objects.Object_Id;
      Segment_Bound : Rose.Objects.Object_Id;
      Region        : Region_Client := Image_Region;
      Flags         : Word;
   begin
      case Segment_Type is
         when PT_LOAD =>
            Flags :=
              (if Readable
               then Rose.Interfaces.Process_Memory.Segment_Readable else 0)
              + (if Writable
                 then Rose.Interfaces.Process_Memory.Segment_Writable else 0)
              + (if Executable
                 then Rose.Interfaces.Process_Memory.Segment_Executable
                 else 0);

            if not Writable then
               Segment_Base :=
                 Base_Object +
                   Rose.Objects.Object_Id (File_Offset)
                 / Rose.Limits.Page_Size;
               Segment_Bound :=
                 Segment_Base +
                   Rose.Objects.Object_Id (File_Size - 1)
                 / Rose.Limits.Page_Size
                 + 1;
            else
               Region :=
                 Reserve_Storage
                   (Top_Store, Word_64 (File_Size));
               Get_Range (Region, Segment_Base, Segment_Bound);

               declare
                  Offset      : Word_32 := 0;
                  Page_Offset : Rose.Objects.Object_Id;
                  Page        : System.Storage_Elements.Storage_Array
                    (1 .. Rose.Limits.Page_Size);
               begin
                  while Offset < Memory_Size loop
                     Page_Offset :=
                       Rose.Objects.Object_Id (Offset / Rose.Limits.Page_Size);
                     Get (Image_Region, Base_Object + Page_Offset, Page);
                     Put (Region, Segment_Base + Page_Offset, Page);
                     Offset := Offset + Rose.Limits.Page_Size;
                  end loop;
               end;
            end if;

            Add_Segment
              (Item         => Process,
               Virtual_Base => V_Address / Rose.Limits.Page_Size,
               Region       => Region,
               Flags        => Flags);

         when others =>
            null;
      end case;
   end Process_Program_Header;

   ----------
   -- Read --
   ----------

   procedure Read
     (Region : Region_Client;
      Base   : Rose.Objects.Object_Id;
      Offset : System.Storage_Elements.Storage_Count;
      Item   : System.Address;
      Size   : System.Storage_Elements.Storage_Count)
   is
      use Rose.Objects;
      use System.Storage_Elements;
      Page       : Object_Id := Base
        + Object_Id (Offset / Rose.Limits.Page_Size);
      Last       : Storage_Count := 0;
      Remaining  : Storage_Count := Size;
      Storage    : Storage_Array (1 .. Size);
      pragma Import (Ada, Storage);
      for Storage'Address use Item;
      Buffer     : Storage_Array (1 .. Rose.Limits.Page_Size);
      Buf_Offset : Storage_Count := Offset mod Rose.Limits.Page_Size;
   begin

      Rose.Console_IO.Put ("elf: read: base ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Base));
      Rose.Console_IO.Put (" offset=");
      Rose.Console_IO.Put (Natural (Offset));
      Rose.Console_IO.Put (" size=");
      Rose.Console_IO.Put (Natural (Size));
      Rose.Console_IO.New_Line;

      while Last < Size loop
         Get (Region, Page, Buffer);

         if Remaining <= Buffer'Last - Buf_Offset then
            Storage (Last + 1 .. Last + Remaining) :=
              Buffer (Buf_Offset .. Remaining);
            Rose.Console_IO.Put (Storage);
            return;
         end if;

         Storage (Last + Buf_Offset .. Last + Buffer'Last) :=
           Buffer (Buf_Offset .. Buffer'Last);
         Page := Page + 1;
         Last := Last + Buffer'Last;
         Remaining := Remaining - Buffer'Last;
         Buf_Offset := 0;
      end loop;
      Rose.Console_IO.Put (Storage);
   end Read;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Region : Region_Client;
      Base   : Rose.Objects.Object_Id;
      Header : out Elf.Format.Elf_Header)
   is
      use System.Storage_Elements;
   begin
      Read (Region, Base, 0,
            Header'Address, Header'Size / System.Storage_Unit);
   end Read_Header;

   --------------------------
   -- Scan_Program_Headers --
   --------------------------

   procedure Scan_Program_Headers
     (Store   : Storage_Client;
      Region  : Region_Client;
      Base    : Rose.Objects.Object_Id;
      Process : Process_Memory_Client;
      Header  : Elf.Format.Elf_Header)
   is
      use System.Storage_Elements;
      use Rose.Words;
      PH_Count    : constant Natural := Natural (Header.E_Phnum);
      PHs         : array (1 .. PH_Count) of Elf.Format.Program_Header;
   begin
      Read
        (Region => Region,
         Base   => Base,
         Offset => Storage_Count (Header.E_Phoff),
         Item   => PHs'Address,
         Size   => PHs'Size / System.Storage_Unit);

      for PH of PHs loop
         Process_Program_Header
           (Top_Store    => Store,
            Image_Region => Region,
            Process      => Process,
            Base_Object  => Base,
            Segment_Type => PH.P_Type,
            File_Offset  => PH.P_Offset,
            V_Address    => PH.P_VAddr,
            File_Size    => PH.P_FileSz,
            Memory_Size  => PH.P_MemSz,
            Readable     => (PH.P_Flags and 4) = 4,
            Writable     => (PH.P_Flags and 2) = 2,
            Executable   => (PH.P_Flags and 1) = 1);
      end loop;
   end Scan_Program_Headers;

end Elf.Loader;
