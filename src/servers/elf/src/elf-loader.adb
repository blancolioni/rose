with System.Storage_Elements;

with Rose.Limits;
with Rose.Objects;

with Rose.Console_IO;
with Rose.Interfaces.Processes.Segment;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Trace;

with Elf.Format;
with Elf.Segments;

package body Elf.Loader is

   use Rose.Interfaces.Storage.Region.Client;
   use Rose.Interfaces.Storage.Region_Access.Client;
   use Rose.Interfaces.Processes.Process.Client;
   use Rose.Interfaces.Storage.Bank.Client;

   Stack_Bound : constant := 16#E000_0000#;
   Stack_Size  : constant := 16#0004_0000#;
   --  The store allocates a region's backing blocks when it is reserved,
   --  so every process's stack uses this much of the backing store,
   --  however little of it is touched (#305).
   Stack_Base  : constant := Stack_Bound - Stack_Size;

   Heap_Base   : constant := 16#4000_0000#;
   Heap_Size   : constant := 16#0010_0000#;
   --  Each program's heap starts empty at Heap_Base, and grows when the
   --  program asks (interfaces/heap.idl), up to Heap_Size. As with the
   --  stack, the store allocates all of its backing blocks now (#305).

   procedure Read
     (Region : Region_Access_Client;
      Base   : Rose.Objects.Object_Id;
      Offset : System.Storage_Elements.Storage_Count;
      Item   : System.Address;
      Size   : System.Storage_Elements.Storage_Count);

   procedure Read_Header
     (Region : Region_Access_Client;
      Base   : Rose.Objects.Object_Id;
      Header : out Elf.Format.Elf_Header);

   procedure Scan_Program_Headers
     (Bank      : Bank_Client;
      Region     : Region_Access_Client;
      Base       : Rose.Objects.Object_Id;
      Process    : Process_Client;
      Header     : Elf.Format.Elf_Header;
      Image_Size : Rose.Words.Word_64;
      Success    : out Boolean);
   --  Add the segments of the image to Process.  The headers are not
   --  trusted: Success is False if they are not valid (see Elf.Segments).

   procedure Process_Program_Header
     (Top_Bank    : Bank_Client;
      Image_Region : Region_Access_Client;
      Process      : Process_Client;
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
     (Process : Rose.Interfaces.Processes.Process.Client.Process_Client;
      Bank   : Rose.Interfaces.Storage.Bank.Client.Bank_Client;
      Image   : Region_Access_Client;
      Start   : out Rose.Words.Word;
      Success : out Boolean)
   is
      use type Rose.Objects.Object_Id;
      use type Rose.Words.Word_64;
      Header      : Elf.Format.Elf_Header;
      Base, Bound : Rose.Objects.Object_Id;
      Image_Size  : Rose.Words.Word_64;
   begin
      Get_Range (Image, Base, Bound);
      Success := True;

      --  The size of the image, with room to spare in a Word_64 for the
      --  validation of what the headers say.

      Image_Size :=
        (if Bound > Base
         then Rose.Words.Word_64'Min (Rose.Words.Word_64 (Bound - Base),
                                      2 ** 32)
              * Rose.Limits.Page_Size
         else 0);

      if Image_Size < Elf.Format.Elf_Header'Size / System.Storage_Unit then
         Rose.Console_IO.Put_Line ("elf: image is too small");
         Success := False;
         return;
      end if;

      Read_Header (Image, Base, Header);
      Rose.Trace.Event ("header read");
      if not Elf.Format.Valid (Header)
        or else not Elf.Segments.Table_Fits (Header, Image_Size)
      then
         Rose.Console_IO.Put_Line ("elf: invalid header");
         Success := False;
         return;
      end if;

      Start := Header.E_Entry;
      Scan_Program_Headers
        (Bank, Image, Base, Process, Header, Image_Size, Success);
      if not Success then
         return;
      end if;
      Rose.Trace.Event ("segments added");

      --  The stack and the heap belong to the process: mem releases them
      --  when it is gone. Elf hands each region over, and forgets it
      --  rather than closing it, which would release it (#403).

      declare
         use type Rose.Words.Word;
         Region : Rose.Interfaces.Storage.Region.Client.Region_Client :=
           Reserve_Storage
             (Bank, Rose.Words.Word_64 (Stack_Size));
      begin
         Add_Private_Segment
           (Item          => Process,
            Virtual_Base  => Stack_Base / Rose.Limits.Page_Size,
            Virtual_Bound => Stack_Bound / Rose.Limits.Page_Size,
            Region        => Region,
            Flags         =>
              Rose.Interfaces.Processes.Segment.Segment_Readable
            + Rose.Interfaces.Processes.Segment.Segment_Writable);
         Forget (Region);
         Rose.Trace.Event ("stack added");
      end;

      declare
         use type Rose.Words.Word;
         Region : Rose.Interfaces.Storage.Region.Client.Region_Client :=
           Reserve_Storage
             (Bank, Rose.Words.Word_64 (Heap_Size));
      begin
         Add_Private_Segment
           (Item          => Process,
            Virtual_Base  => Heap_Base / Rose.Limits.Page_Size,
            Virtual_Bound => (Heap_Base + Heap_Size) / Rose.Limits.Page_Size,
            Region        => Region,
            Flags         =>
              Rose.Interfaces.Processes.Segment.Segment_Readable
            + Rose.Interfaces.Processes.Segment.Segment_Writable
            + Rose.Interfaces.Processes.Segment.Segment_Resizable);
         Forget (Region);
         Rose.Trace.Event ("heap added");
      end;
   end Load_Elf_Image;

   ----------------------------
   -- Process_Program_Header --
   ----------------------------

   procedure Process_Program_Header
     (Top_Bank    : Bank_Client;
      Image_Region : Region_Access_Client;
      Process      : Process_Client;
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
      Virtual_Bound : constant Word := V_Address + Memory_Size;
      Page_Bound    : constant Word :=
                        (Virtual_Bound + Rose.Limits.Page_Size - 1)
                        / Rose.Limits.Page_Size;
      --  The page after the segment's last one. A segment that ends on a
      --  page boundary doesn't reach the next page, which may be the
      --  start of the next segment.
      Flags         : Word;
   begin
      case Segment_Type is
         when PT_LOAD =>
            Flags :=
              (if Readable
               then Rose.Interfaces.Processes.Segment.Segment_Readable else 0)
              + (if Writable
                 then Rose.Interfaces.Processes.Segment
                   .Segment_Writable else 0)
              + (if Executable
                 then Rose.Interfaces.Processes.Segment.Segment_Executable
                 else 0);

            --  A read-only segment is mapped from the program's image,
            --  which all its processes share, and which none of them
            --  owns. A writable one has storage of its own, which elf
            --  hands to the process, and forgets (#403).

            if not Writable then
               Add_Segment
                 (Item          => Process,
                  Virtual_Base  => V_Address / Rose.Limits.Page_Size,
                  Virtual_Bound => Page_Bound,
                  Region        => Image_Region,
                  Region_Offset => File_Offset,
                  Flags         => Flags);
            else
               declare
                  Region : Region_Client :=
                             Reserve_Storage
                               (Top_Bank, Word_64 (Memory_Size));
                  Offset      : Word_32 := 0;
                  Image_Start : constant Rose.Objects.Object_Id :=
                                  Rose.Objects.Object_Id
                                    (File_Offset / Rose.Limits.Page_Size);
                  Page_Offset : Rose.Objects.Object_Id;
                  Page        : System.Storage_Elements.Storage_Array
                    (1 .. Rose.Limits.Page_Size);
               begin
                  Rose.Trace.Event ("storage reserved, bytes ",
                                    Natural (Memory_Size));

                  Get_Range (Region, Segment_Base, Segment_Bound);

                  while Offset < File_Size loop
                     Page_Offset :=
                       Rose.Objects.Object_Id (Offset / Rose.Limits.Page_Size);
                     Get (Image_Region,
                          Base_Object + Image_Start + Page_Offset, Page);
                     Put (Region, Segment_Base + Page_Offset, Page);
                     Offset := Offset + Rose.Limits.Page_Size;
                  end loop;
                  Rose.Trace.Event ("segment copied, bytes ",
                                    Natural (File_Size));

                  Add_Private_Segment
                    (Item          => Process,
                     Virtual_Base  => V_Address / Rose.Limits.Page_Size,
                     Virtual_Bound => Page_Bound,
                     Region        => Region,
                     Flags         => Flags);
                  Forget (Region);
               end;
            end if;

         when others =>
            null;
      end case;
   end Process_Program_Header;

   ----------
   -- Read --
   ----------

   procedure Read
     (Region : Region_Access_Client;
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
      Buf_Offset : Storage_Count := Offset mod Rose.Limits.Page_Size + 1;
   begin

      while Last < Size loop
         Get (Region, Page, Buffer);

         if Remaining <= Buffer'Last - Buf_Offset then
            Storage (Last + 1 .. Last + Remaining) :=
              Buffer (Buf_Offset .. Remaining);
            return;
         end if;

         Storage (Last + Buf_Offset .. Last + Buffer'Last) :=
           Buffer (Buf_Offset .. Buffer'Last);
         Page := Page + 1;
         Last := Last + Buffer'Last;
         Remaining := Remaining - Buffer'Last;
         Buf_Offset := 1;
      end loop;
   end Read;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Region : Region_Access_Client;
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
     (Bank      : Bank_Client;
      Region     : Region_Access_Client;
      Base       : Rose.Objects.Object_Id;
      Process    : Process_Client;
      Header     : Elf.Format.Elf_Header;
      Image_Size : Rose.Words.Word_64;
      Success    : out Boolean)
   is
      use System.Storage_Elements;
      PH_Count : constant Natural := Natural (Header.E_Phnum);
      --  At most Elf.Segments.Max_Program_Headers, by Table_Fits.
      PHs      : Elf.Segments.Program_Header_Array (1 .. PH_Count);
      List     : Elf.Segments.Segment_List;
   begin
      Read
        (Region => Region,
         Base   => Base,
         Offset => Storage_Count (Header.E_Phoff),
         Item   => PHs'Address,
         Size   => PHs'Size / System.Storage_Unit);

      Elf.Segments.Describe (PHs, Image_Size, Stack_Base, List, Success);
      if not Success then
         Rose.Console_IO.Put_Line ("elf: invalid program headers");
         return;
      end if;

      for I in 1 .. List.Count loop
         declare
            Item : Elf.Segments.Segment renames List.Items (I);
         begin
            Process_Program_Header
              (Top_Bank    => Bank,
               Image_Region => Region,
               Process      => Process,
               Base_Object  => Base,
               Segment_Type => Elf.Format.PT_LOAD,
               File_Offset  => Item.File_Offset,
               V_Address    => Item.Virtual_Address,
               File_Size    => Item.File_Size,
               Memory_Size  => Item.Memory_Size,
               Readable     => Item.Readable,
               Writable     => Item.Writable,
               Executable   => Item.Executable);
         end;
      end loop;
   end Scan_Program_Headers;

end Elf.Loader;
