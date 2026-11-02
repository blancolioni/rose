with System;
with System.Storage_Elements;

with Rose.Boot.Console;

package body Rose.Multiboot is

   pragma Warnings (Off);

   Report_Memory_Map : constant Boolean := True;
   Report_Modules    : constant Boolean := True;

   Physmem_Low, Physmem_High : Word := 0;

   function Physical_Memory_Low return Word is (Physmem_Low);

   function Physical_Memory_High return Word is (Physmem_High);

   --  The framebuffer tag (type 8): the address is a 64-bit word at 8,
   --  then pitch, width and height, then the depth and the type as
   --  bytes at 28 and 29.  For an RGB framebuffer, the position and size
   --  of the red, green and blue fields follow as bytes from 32.

   Framebuffer_Type_RGB : constant := 1;

   FB_Usable                    : Boolean := False;
   FB_Address, FB_Pitch         : Word_32 := 0;
   FB_Width, FB_Height          : Word_32 := 0;

   function Have_Framebuffer return Boolean is (FB_Usable);
   function Framebuffer_Address return Word_32 is (FB_Address);
   function Framebuffer_Pitch return Word_32 is (FB_Pitch);
   function Framebuffer_Width return Word_32 is (FB_Width);
   function Framebuffer_Height return Word_32 is (FB_Height);

   type Multiboot_Tag_Type is
     (End_Of_Tags,
      Command_Line,
      Boot_Loader_Name,
      Module,
      Basic_Memory_Info,
      Boot_Device,
      Memory_Map,
      VBE,
      Frame_Buffer,
      ELF_Sections,
      APM,
      EFI_32,
      EFI_64,
      SMBIOS,
      ACPI_Old,
      ACPI_New,
      Network,
      EFI_MMAP,
      EFI_BS,
      EFI_32_IH,
      EFI_64_IH,
      Load_Base_Address);

   Found : array (Multiboot_Tag_Type) of Boolean :=
             (others => False);

   procedure Scan_Multiboot_Tags
     (Match : Multiboot_Tag_Type;
      Process : not null access
        procedure (Tag_Address : Word));

   function Get_Word_32
     (From_Tag_Address : Word;
      Offset           : Word)
      return Word_32;

   function Get_Word_8
     (From_Tag_Address : Word;
      Offset           : Word)
      return Word_8;

   procedure Load_Framebuffer
     (Tag_Address : Word);

   procedure Get_String
     (From_Tag_Address : Word;
      Offset           : Word;
      Text             : out String;
      Last             : out Natural);

   procedure Report
     (Tag_Address : Word);

   -----------------
   -- Check_Magic --
   -----------------

   procedure Check_Magic is
      Magic : Word_32;
      pragma Import (C, Magic, "magic");
   begin
      if Magic /= Magic_Value then
         Rose.Boot.Console.Put ("bad multiboot magic: ");
         Rose.Boot.Console.Put (Magic);
         Rose.Boot.Console.New_Line;
         loop
            null;
         end loop;
      end if;
   end Check_Magic;

   ----------------
   -- Get_String --
   ----------------

   procedure Get_String
     (From_Tag_Address : Word;
      Offset           : Word;
      Text             : out String;
      Last             : out Natural)
   is
      use System.Storage_Elements;
      Size  : Storage_Count;
      for Size'Address use System'To_Address (From_Tag_Address + 4);
      pragma Import (Ada, Size);
      Start : constant System.Address :=
                System'To_Address (From_Tag_Address + Offset);
      Storage : Storage_Array (1 .. Size - Storage_Count (Offset));
      for Storage'Address use Start;
      pragma Import (Ada, Storage);
   begin
      Last := Text'First - 1;
      for Element of Storage loop
         exit when Element = 0;
         Last := Last + 1;
         exit when Last > Text'Last;
         Text (Last) := Character'Val (Element);
      end loop;
   end Get_String;

   ----------------
   -- Get_Word_8 --
   ----------------

   function Get_Word_8
     (From_Tag_Address : Word;
      Offset           : Word)
      return Word_8
   is
      Value : Word_8;
      for Value'Address use System'To_Address (From_Tag_Address + Offset);
      pragma Import (Ada, Value);
   begin
      return Value;
   end Get_Word_8;

   -----------------
   -- Get_Word_32 --
   -----------------

   function Get_Word_32
     (From_Tag_Address : Word;
      Offset           : Word)
      return Word_32
   is
      Value : Word_32;
      for Value'Address use System'To_Address (From_Tag_Address + Offset);
      pragma Import (Ada, Value);
   begin
      return Value;
   end Get_Word_32;

   function Have_Physical_Memory_Range return Boolean
   is (Found (Basic_Memory_Info));

   function Have_Memory_Map return Boolean
   is (Found (Memory_Map));

   function Have_Modules return Boolean
   is (Found (Module));

   ----------------------
   -- Load_Framebuffer --
   ----------------------

   procedure Load_Framebuffer
     (Tag_Address : Word)
   is
      Address_Low  : constant Word_32 := Get_Word_32 (Tag_Address, 8);
      Address_High : constant Word_32 := Get_Word_32 (Tag_Address, 12);
      Pitch        : constant Word_32 := Get_Word_32 (Tag_Address, 16);
      Width        : constant Word_32 := Get_Word_32 (Tag_Address, 20);
      Height       : constant Word_32 := Get_Word_32 (Tag_Address, 24);
      Depth        : constant Word_8 := Get_Word_8 (Tag_Address, 28);
      FB_Type      : constant Word_8 := Get_Word_8 (Tag_Address, 29);
   begin
      FB_Usable :=
        FB_Type = Framebuffer_Type_RGB
        and then Depth = 32
        and then Address_High = 0
        and then Word_64 (Address_Low) + Word_64 (Pitch) * Word_64 (Height)
                   < 2 ** 32;

      if FB_Usable then
         FB_Address := Address_Low;
         FB_Pitch := Pitch;
         FB_Width := Width;
         FB_Height := Height;
      end if;
   end Load_Framebuffer;

   ---------------------------
   -- Load_Multiboot_Header --
   ---------------------------

   procedure Load_Multiboot_Header is
      Local_Address : constant System.Address :=
                        System'To_Address (Multiboot_Address);
      Size          : Rose.Words.Word_32;
      for Size'Address use Local_Address;
      pragma Import (Ada, Size);

      procedure Process_Record
        (Start : Word_32);

      --------------------
      -- Process_Record --
      --------------------

      procedure Process_Record
        (Start : Word_32)
      is
         Tag_Type : Word_32;
         for Tag_Type'Address use System'To_Address (Start);
         pragma Import (Ada, Tag_Type);

         Tag  : constant Multiboot_Tag_Type :=
                  Multiboot_Tag_Type'Val (Tag_Type);
      begin
         Report (Start);
         Found (Tag) := True;

         if Tag = Basic_Memory_Info then
            Physmem_Low := 0;
            Physmem_High := Get_Word_32 (Start, 12);
         elsif Tag = Frame_Buffer then
            Load_Framebuffer (Start);
         end if;
      end Process_Record;

   begin
      Rose.Boot.Console.Put
        ("Announced mbi size: ");
      Rose.Boot.Console.Put (Natural (Size));
      Rose.Boot.Console.New_Line;
      Scan_Multiboot_Tags (End_Of_Tags, Process_Record'Access);
   end Load_Multiboot_Header;

   ------------
   -- Report --
   ------------

   procedure Report
     (Tag_Address : Word)
   is
      use Rose.Boot.Console;
      Tag_Type : Word_32;
      for Tag_Type'Address use System'To_Address (Tag_Address);
      pragma Import (Ada, Tag_Type);
      Size : Word_32;
      for Size'Address use System'To_Address (Tag_Address + 4);
      pragma Import (Ada, Size);

      Tag      : constant Multiboot_Tag_Type :=
                   Multiboot_Tag_Type'Val (Tag_Type);
   begin
      case Tag is
         when Command_Line =>
            Put ("command line: ");
            declare
               Line : String (1 .. 100);
               Last : Natural;
            begin
               Get_String (Tag_Address, 8, Line, Last);
               Put (Line (1 .. Last));
               New_Line;
            end;

         when Boot_Loader_Name =>
            Put ("boot loader: ");
            declare
               Line : String (1 .. 100);
               Last : Natural;
            begin
               Get_String (Tag_Address, 8, Line, Last);
               Put (Line (1 .. Last));
               New_Line;
            end;

         when Basic_Memory_Info =>
            Put ("basic memory info: low=");
            Put (Natural (Get_Word_32 (Tag_Address, 8)));
            Put ("K");
            Put (" high=");
            Put (Natural (Get_Word_32 (Tag_Address, 12)));
            Put ("K");
            New_Line;
         when Load_Base_Address =>
            Put ("load base address: ");
            Put (Get_Word_32 (Tag_Address, 8));
            New_Line;
         when Frame_Buffer =>
            Put ("framebuffer: address=");
            Put (Get_Word_32 (Tag_Address, 12));
            Put (":");
            Put (Get_Word_32 (Tag_Address, 8));
            Put (" pitch=");
            Put (Natural (Get_Word_32 (Tag_Address, 16)));
            Put (" size=");
            Put (Natural (Get_Word_32 (Tag_Address, 20)));
            Put ("x");
            Put (Natural (Get_Word_32 (Tag_Address, 24)));
            Put ("x");
            Put (Natural (Get_Word_8 (Tag_Address, 28)));
            Put (" type=");
            Put (Natural (Get_Word_8 (Tag_Address, 29)));
            if Get_Word_8 (Tag_Address, 29) = Framebuffer_Type_RGB then
               Put (" red=");
               Put (Natural (Get_Word_8 (Tag_Address, 32)));
               Put ("/");
               Put (Natural (Get_Word_8 (Tag_Address, 33)));
               Put (" green=");
               Put (Natural (Get_Word_8 (Tag_Address, 34)));
               Put ("/");
               Put (Natural (Get_Word_8 (Tag_Address, 35)));
               Put (" blue=");
               Put (Natural (Get_Word_8 (Tag_Address, 36)));
               Put ("/");
               Put (Natural (Get_Word_8 (Tag_Address, 37)));
            end if;
            New_Line;
         when others =>
            Put ("multiboot: tag=");
            Put (Natural (Tag_Type));
            Put ("; size=");
            Put (Natural (Size));
            New_Line;

            Put ("payload:");
            for I in 2 .. Size / 4 - 1 loop
               Put (" ");
               Put (Get_Word_32 (Tag_Address, I * 4));
            end loop;
            New_Line;
      end case;

   end Report;

   ---------------------------
   -- Scan_Kernel_Arguments --
   ---------------------------

   procedure Scan_Kernel_Arguments
     (Process : not null access
        procedure (Argument : String))
   is

      procedure Process_Command_Line_Tag
        (Info_Start : Word);

      ------------------------------
      -- Process_Command_Line_Tag --
      ------------------------------

      procedure Process_Command_Line_Tag
        (Info_Start : Word)
      is
         Command_Line : String (1 .. 1024);
         Last         : Natural;
         Start        : Natural := 1;
      begin
         Get_String (Info_Start, 8, Command_Line, Last);
         for I in 1 .. Last + 1 loop
            if I = Last + 1
              or else Command_Line (I) = ' '
            then
               if I > Start then
                  declare
                     Argument : constant String :=
                                  Command_Line (Start .. I - 1);
                  begin
                     Process (Argument);
                  end;
               end if;
               Start := I + 1;
            end if;
         end loop;
      end Process_Command_Line_Tag;

   begin
      Scan_Multiboot_Tags (Command_Line, Process_Command_Line_Tag'Access);
   end Scan_Kernel_Arguments;

   ---------------------
   -- Scan_Memory_Map --
   ---------------------

   procedure Scan_Memory_Map
     (Process : not null access
        procedure (Available : Boolean;
                   Low       : Rose.Words.Word_64;
                   High      : Rose.Words.Word_64))
   is
      procedure Process_Memory_Region
        (Start : Word);

      ---------------------------
      -- Process_Memory_Region --
      ---------------------------

      procedure Process_Memory_Region
        (Start : Word)
      is
         use Rose.Boot.Console;
         Size       : constant Word_32 := Get_Word_32 (Start, 4);
         Entry_Size : constant Word_32 := Get_Word_32 (Start, 8);
         Version    : constant Word_32 := Get_Word_32 (Start, 12);
         Addr       : Word_32 := 16;
      begin
         while Addr < Size loop
            declare
               Base       : constant Word_32 := Get_Word_32 (Start, Addr);
               Length     : constant Word_32 := Get_Word_32 (Start, Addr + 8);
               Entry_Type : constant Word_32 := Get_Word_32 (Start, Addr + 16);
               Reserved   : constant Word_32 := Get_Word_32 (Start, Addr + 20);
            begin
               Put ("region: addr=");
               Put (Addr);
               Put ("; base = ");
               Put (Base);
               Put ("; bound=");
               Put (Base + Length);
               Put ("; Type=");
               Put (Natural (Entry_Type));
               New_Line;
               Process (Entry_Type = 1,
                        Word_64 (Base), Word_64 (Base + Length));
               Addr := Addr + Entry_Size;
            end;
         end loop;
      end Process_Memory_Region;

   begin
      Scan_Multiboot_Tags
        (Match   => Memory_Map,
         Process => Process_Memory_Region'Access);
   end Scan_Memory_Map;

   ------------------
   -- Scan_Modules --
   ------------------

   procedure Scan_Modules
     (Process : not null access
        procedure (Mod_Start  : Rose.Words.Word_32;
                   Mod_End    : Rose.Words.Word_32;
                   Mod_Text   : String))
   is
      procedure Process_Module
        (Start : Word);

      --------------------
      -- Process_Module --
      --------------------

      procedure Process_Module
        (Start : Word)
      is
         Base : constant Word_32 := Get_Word_32 (Start, 8);
         Bound : constant Word_32 := Get_Word_32 (Start, 12);
         Command : String (1 .. 200);
         Last    : Natural;
      begin
         Get_String (Start, 16, Command, Last);
         Process (Base, Bound, Command (1 .. Last));
      end Process_Module;

   begin

      Scan_Multiboot_Tags (Module, Process_Module'Access);

--        if not Have_Memory_Map then
--           return;
--        end if;
--
--        if Report_Modules then
--           Rose.Boot.Console.Put ("Module count: ");
--           Rose.Boot.Console.Put (Count);
--           Rose.Boot.Console.New_Line;
--        end if;
--
--        for Mod_Info of Mods loop
--           declare
--              Text_Address : constant System.Address :=
--                System'To_Address (Mod_Info.Data);
--              Text_Buffer : String (1 .. 40);
--              for Text_Buffer'Address use Text_Address;
--              pragma Import (Ada, Text_Buffer);
--              Last : Natural := 0;
--           begin
--              while Last < Text_Buffer'Last
--                and then Text_Buffer (Last + 1) /= Character'Val (0)
--              loop
--                 Last := Last + 1;
--              end loop;
--              Process (Mod_Info.First, Mod_Info.Last,
--                       Text_Buffer (1 .. Last));
--           end;
--        end loop;

   end Scan_Modules;

   -------------------------
   -- Scan_Multiboot_Tags --
   -------------------------

   procedure Scan_Multiboot_Tags
     (Match   : Multiboot_Tag_Type;
      Process : not null access
        procedure (Tag_Address : Word_32))
   is
      Addr : Word_32 := Multiboot_Address + 8;
   begin
      loop
         declare
            Tag_Type : Word_32;
            for Tag_Type'Address use System'To_Address (Addr);
            pragma Import (Ada, Tag_Type);
            Size     : Word_32;
            for Size'Address use System'To_Address (Addr + 4);
            pragma Import (Ada, Size);

            Tag      : constant Multiboot_Tag_Type :=
                         Multiboot_Tag_Type'Val (Tag_Type);
         begin
            exit when Tag = End_Of_Tags;

            if Match = End_Of_Tags or else Tag = Match then
               Process (Addr);
            end if;

            Addr := Addr + Size;
            if Addr mod 8 /= 0 then
               Addr := Addr + (8 - Addr mod 8);
            end if;
         end;
      end loop;
   end Scan_Multiboot_Tags;

end Rose.Multiboot;
