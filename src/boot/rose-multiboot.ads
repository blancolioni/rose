with Rose.Words;                        use Rose.Words;

package Rose.Multiboot is

   procedure Load_Multiboot_Header;

   function Check_Magic return Boolean;

   function Have_Physical_Memory_Range return Boolean;
   function Have_Memory_Map return Boolean;
   function Have_Modules return Boolean;

   function Physical_Memory_Low return Word_32;
   function Physical_Memory_High return Word_32;

   procedure Scan_Modules
     (Process : not null access
        procedure (Mod_Start  : Word_32;
                   Mod_End    : Word_32;
                   Mod_Text   : String));

   procedure Scan_Memory_Map
     (Process : not null access
        procedure (Available : Boolean;
                   Low       : Word_64;
                   High      : Word_64));

   procedure Scan_Kernel_Arguments
     (Process : not null access
        procedure (Argument : String));

private

   subtype Magic_Values is Word_32;

   Magic_Value : constant Magic_Values := 16#2BAD_B002#;

   --  We need to import the "mbd" symbol...
   Info_Address : constant Word_32;

   pragma Import (Assembly, Info_Address, "mbd");

   type Multiboot_Header_Array is
     array (0 .. 21) of Word_32
     with Pack, Size => 22 * 4 * 8;

   Header : Multiboot_Header_Array;

   ----------------------------------------------------------------------------
   --  Multiboot information.
   ----------------------------------------------------------------------------
   type Features is
      record
         Memory               : Boolean; --  Bit 0
         Boot_Device          : Boolean; --  Bit 1
         Command_Line         : Boolean; --  Bit 2
         Modules              : Boolean; --  Bit 3
         Symbol_Table         : Boolean; --  Bit 4 - this is Aout only.
         Section_Header_Table : Boolean; --  Bit 5 - this is ELF only.
         BIOS_Memory_Map      : Boolean; --  Bit 6
         Drives               : Boolean; --  Bit 7
         ROM_Configuration    : Boolean; --  Bit 8
         Boot_Loader          : Boolean; --  Bit 9
         APM_Table            : Boolean; --  Bit 10
         Graphics_Table       : Boolean; --  Bit 11
      end record;

   for Features use
      record
         Memory               at 0 range 0 .. 0;
         Boot_Device          at 0 range 1 .. 1;
         Command_Line         at 0 range 2 .. 2;
         Modules              at 0 range 3 .. 3;
         Symbol_Table         at 0 range 4 .. 4;
         Section_Header_Table at 0 range 5 .. 5;
         BIOS_Memory_Map      at 0 range 6 .. 6;
         Drives               at 0 range 7 .. 7;
         ROM_Configuration    at 1 range 0 .. 0;
         Boot_Loader          at 1 range 1 .. 1;
         APM_Table            at 1 range 2 .. 2;
         Graphics_Table       at 1 range 3 .. 3;
      end record;

   for Features'Size use 32;

   ----------------------------------------------------------------------------
   --  Boot device information.
   ----------------------------------------------------------------------------
   type Boot_Devices is
      record
         Drive       : Word_8;
         Partition_1 : Word_8;
         Partition_2 : Word_8;
         Partition_3 : Word_8;
      end record;

   for Boot_Devices use
      record
         Drive       at 0 range 0 .. 7;
         Partition_1 at 1 range 0 .. 7;
         Partition_2 at 2 range 0 .. 7;
         Partition_3 at 3 range 0 .. 7;
      end record;

   for Boot_Devices'Size use 32;

   Invalid_Partition : constant Word_8 := 16#ff#;

   ----------------------------------------------------------------------------
   --  Memory information.
   --  These values are in KB
   ----------------------------------------------------------------------------
   type Memory_Info is
      record
         Upper : Word_32;
         Lower : Word_32;
      end record;

   pragma Convention (C, Memory_Info);

   ----------------------------------------------------------------------------
   --  Loadable module information.
   ----------------------------------------------------------------------------
   type Modules is
      record
         First    : Word_32;
         Last     : Word_32;
         Data     : Word_32; --  NULL terminated C string.
         Reserved : Word_32;    --  Should be 0.
      end record;

   type Modules_Array is array (Natural range <>) of Modules;

   pragma Convention (C, Modules_Array);

   --  type Modules_Array_Access is access Modules_Array;

   --  pragma Convention (C, Modules_Array_Access);

   type Modules_Info is
      record
         Count : Word_32;
         First : Word_32;
      end record;

   ----------------------------------------------------------------------------
   --  Symbols information.
   ----------------------------------------------------------------------------
   pragma Convention (C, Modules_Info);

   type Symbols_Variant is (Aout, ELF);

   function Get_Symbols_Variant return Symbols_Variant;

   ----------------------------------------------------------------------------
   --  a.out symbols or ELF sections
   --
   --  TODO: a.out only - This can be implemented by anyone who wants to use
   --  aout.
   --
   --  From what I can tell from the spec, Addr points to a size followed by
   --  an array of a.out nlist structures. This is then followed by a size
   --  of a set of strings, then the sizeof (unsigned), then the strings
   --  (NULL terminated).
   --
   --  Table_Size and String_Size are the same as the ones listed above.
   ----------------------------------------------------------------------------
   type Symbols (Variant : Symbols_Variant := ELF) is
      record
         case Variant is
            when Aout =>
               Table_Size  : Word_32;
               String_Size : Word_32;
               Aout_Addr   : Word_32;
               Reserved    : Word_32;     --  Always 0.
            when ELF =>
               Number      : Word_32;
               Size        : Word_32;
               ELF_Addr    : Word_32;
               Shndx       : Word_32;
         end case;
      end record;

   pragma Convention (C, Symbols);
   pragma Unchecked_Union (Symbols);

   ----------------------------------------------------------------------------
   --  Memory map information.
   ----------------------------------------------------------------------------

   type Memory_Type is new Word_32 with Size => 32;

   Memory_Available : constant Memory_Type := 1;
   Memory_Reserved  : constant Memory_Type := 2;

   type Memory_Map_Entry is
      record
         Size        : Word_32;
         Base_Low    : Word_32;
         Base_High   : Word_32;
         Length_Low  : Word_32;
         Length_High : Word_32;
         Sort        : Memory_Type;
      end record
        with Size => 192;

   type Memory_Map_Entry_Access is access Memory_Map_Entry;

   type Memory_Map_Info is
      record
         Length : Word_32;
         Addr   : Word_32;
      end record;

   pragma Convention (C, Memory_Map_Info);

   ----------------------------------------------------------------------------
   --  Returns null on failure or if the memory map doesn't exist.
   ----------------------------------------------------------------------------
   function First_Memory_Map_Entry return Memory_Map_Entry_Access;

   ----------------------------------------------------------------------------
   --  Returns null on failure or if we have seen all of the memory map.
   ----------------------------------------------------------------------------
   function Next_Memory_Map_Entry
     (Current : Memory_Map_Entry_Access) return Memory_Map_Entry_Access;

   ----------------------------------------------------------------------------
   --  Drives information.
   --  TODO: Complete
   ----------------------------------------------------------------------------
   type Drives_Info is
      record
         Length : Word_32;
         Addr   : Word_32;
      end record;

   pragma Convention (C, Drives_Info);

   ----------------------------------------------------------------------------
   --  APM table.
   ----------------------------------------------------------------------------
   type APM_Table is
      record
         Version         : Word_16;
         C_Seg           : Word_16;
         Offset          : Word_32;
         C_Seg_16        : Word_16;
         D_Seg           : Word_16;
         Flags           : Word_16;
         C_Seg_Length    : Word_16;
         C_Seg_16_Length : Word_16;
         D_Seg_Length    : Word_16;
      end record;

   pragma Convention (C, APM_Table);

   type APM_Table_Access is access APM_Table;

   function Get_APM_Table return APM_Table_Access;

   ----------------------------------------------------------------------------
   --  Graphics information.
   ----------------------------------------------------------------------------
   type VBE_Info is
      record
         Control_Info  : Word_32;
         Mode_Info     : Word_32;
         Mode          : Word_32;
         Interface_Seg : Word_32;
         Interface_Off : Word_32;
         Interface_Len : Word_32;
      end record;

   pragma Convention (C, VBE_Info);

   type MB_Info is
      record
         Flags            : Features;
         Memory           : Memory_Info;
         Boot_Device      : Boot_Devices;
         Cmd_Line         : Word_32;  --  NULL terminated C string.
         Modules          : Modules_Info;
         Syms             : Symbols;
         Memory_Map       : Memory_Map_Info;
         Drives           : Drives_Info;
         Config_Table     : Word_32;
         Boot_Loader_Name : Word_32;         --  NULL terminated C string.
         APM              : Word_32;
         VBE              : VBE_Info;
      end record;

   pragma Convention (C, MB_Info);

   for MB_Info use
      record
         Flags at 0 range 0 .. 31;
         Memory at 4 range 0 .. 63;
         Boot_Device at 12 range 0 .. 31;
         Cmd_Line at 16 range 0 .. 31;
         Modules at 20 range 0 .. 63;
         Syms at 28 range 0 .. 16 * 8 - 1;
         Memory_Map at 44 range 0 .. 63;
         Drives at 52 range 0 .. 63;
         Config_Table at 60 range 0 .. 31;
         Boot_Loader_Name at 64 range 0 .. 31;
         APM at 68 range 0 .. 31;
         VBE at 72 range 0 .. 24 * 8 - 1;
      end record;

   ----------------------------------------------------------------------------
   --  Magic number.
   ----------------------------------------------------------------------------
   Magic : constant Magic_Values;

   pragma Import (Assembly, Magic, "magic");

   function Check_Magic return Boolean
   is (Magic = Magic_Value);

   Physical_Memory_Flag : constant := 2 ** 0;
   Command_Line_Flag    : constant := 2 ** 2;
   Module_Flag          : constant := 2 ** 3;
   Memory_Map_Flag      : constant := 2 ** 6;

   function Have_Physical_Memory_Range return Boolean
   is ((Header (0) and Physical_Memory_Flag) /= 0);

   function Have_Memory_Map return Boolean
   is ((Header (0) and Memory_Map_Flag) /= 0);

   function Have_Kernel_Arguments return Boolean
   is ((Header (0) and Command_Line_Flag) /= 0);

   function Have_Modules return Boolean
   is ((Header (0) and Module_Flag) /= 0);

   function Physical_Memory_Low return Word_32
   is (Header (1));

   function Physical_Memory_High return Word_32
   is (Header (2));

end Rose.Multiboot;
