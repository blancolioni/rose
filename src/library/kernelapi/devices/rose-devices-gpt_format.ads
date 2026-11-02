with System.Storage_Elements;

with Rose.Words;

package Rose.Devices.GPT_Format
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   --  The format of a GUID partition table (see Rose.Devices.GPT): the
   --  header and the partition entries as records, how they are laid out
   --  in bytes, and what makes them valid.  The bytes are from a disk,
   --  which may be corrupt, so nothing here trusts them; there is no
   --  input or output, and no address.

   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;
   use type System.Storage_Elements.Storage_Offset;

   GPT_Magic : constant Rose.Words.Word_64 := 16#5452_4150_2049_4645#;

   Entry_Size    : constant := 128;
   Array_Entries : constant := 128;
   Array_Bytes   : constant := Entry_Size * Array_Entries;
   --  The standard partition array: 128 entries of 128 bytes, 16 KiB

   Header_Bytes : constant := 92;

   Max_Partitions : constant := 16;
   --  The partitions in use that are kept in memory

   Chunk_Bytes   : constant := 4096;
   Chunk_Entries : constant := Chunk_Bytes / Entry_Size;
   --  The entries are read and written a page at a time, which is as
   --  much as one call carries.

   Name_Length : constant := 36;
   --  A partition name is 36 UTF-16LE code units.

   type Header is
      record
         Magic                 : Rose.Words.Word_64 := GPT_Magic;
         Revision              : Rose.Words.Word_32 := 16#0001_0000#;
         Header_Size           : Rose.Words.Word_32 := Header_Bytes;
         Header_CRC            : Rose.Words.Word_32 := 0;
         Reserved_Zero         : Rose.Words.Word_32 := 0;
         Current_LBA           : Rose.Words.Word_64 := 1;
         Backup_LBA            : Rose.Words.Word_64 := 0;
         First_Usable_LBA      : Rose.Words.Word_64 := 0;
         Last_Usable_LBA       : Rose.Words.Word_64 := 0;
         Disk_GUID_Lo          : Rose.Words.Word_64 := 0;
         Disk_GUID_Hi          : Rose.Words.Word_64 := 0;
         Start_Partition_LBA   : Rose.Words.Word_64 := 2;
         Partition_Entry_Count : Rose.Words.Word_32 := Array_Entries;
         Partition_Entry_Size  : Rose.Words.Word_32 := Entry_Size;
         Partition_Array_CRC   : Rose.Words.Word_32 := 0;
      end record;

   type Partition_Entry is
      record
         Partition_Type_Low  : Rose.Words.Word_64 := 0;
         Partition_Type_High : Rose.Words.Word_64 := 0;
         Partition_Id_Low    : Rose.Words.Word_64 := 0;
         Partition_Id_High   : Rose.Words.Word_64 := 0;
         First_LBA           : Rose.Words.Word_64 := 0;
         Last_LBA            : Rose.Words.Word_64 := 0;
         Flags               : Rose.Words.Word_64 := 0;
         Name                : String (1 .. 2 * Name_Length) :=
                                 (others => Character'Val (0));
      end record;

   --  Bytes: little-endian, and the arrays start at 0.

   function Fits
     (Bytes  : System.Storage_Elements.Storage_Array;
      Offset : System.Storage_Elements.Storage_Offset;
      Size   : System.Storage_Elements.Storage_Offset)
      return Boolean
   is (Bytes'First = 0
       and then Bytes'Last in -1 .. 2 ** 30
       and then Size in 0 .. 2 ** 30
       and then Offset >= 0
       and then Offset <= Bytes'Last - Size + 1);
   --  Size bytes from Offset are in Bytes.

   procedure Encode_Header
     (Item   : Header;
      Bytes  : in out System.Storage_Elements.Storage_Array)
   with Pre => Fits (Bytes, 0, Header_Bytes);
   --  Put Item in the first Header_Bytes bytes.

   function Decode_Header
     (Bytes : System.Storage_Elements.Storage_Array)
      return Header
   with Pre => Fits (Bytes, 0, Header_Bytes);

   procedure Encode_Entry
     (Item   : Partition_Entry;
      Bytes  : in out System.Storage_Elements.Storage_Array;
      Offset : System.Storage_Elements.Storage_Offset)
   with Pre => Fits (Bytes, Offset, Entry_Size);
   --  Put Item in the Entry_Size bytes at Offset.

   function Decode_Entry
     (Bytes  : System.Storage_Elements.Storage_Array;
      Offset : System.Storage_Elements.Storage_Offset)
      return Partition_Entry
   with Pre => Fits (Bytes, Offset, Entry_Size);

   procedure Update_CRC
     (CRC  : in out Rose.Words.Word_32;
      Data : System.Storage_Elements.Storage_Array);
   --  CRC32, as GPT, zlib and Ethernet use it.  CRC starts as all ones,
   --  and the result is its complement.

   function Header_CRC (Item : Header) return Rose.Words.Word_32;
   --  The CRC of the header, with its own CRC field taken as zero.

   --  The disk

   function Array_Blocks (Block_Size : Natural) return Natural
   is (Array_Bytes / Block_Size)
   with Pre => Block_Size in 1 .. Array_Bytes;
   --  The blocks each copy of the partition array takes.

   function Geometry_Valid
     (Block_Size  : Natural;
      Block_Count : Rose.Words.Word_64)
      return Boolean
   is (Block_Size in 512 | 1024 | 2048 | 4096
       and then Block_Count >=
                  2 * Rose.Words.Word_64 (Array_Blocks (Block_Size)) + 4);
   --  A disk that can hold a GPT: the blocks are a size that divides the
   --  chunks that the entries are read and written in, and there is room
   --  for the MBR, both headers and both copies of the entries, and a
   --  block to use.

   function First_Usable_Block
     (Block_Size : Natural)
      return Rose.Words.Word_64
   is (2 + Rose.Words.Word_64 (Array_Blocks (Block_Size)))
   with Pre => Block_Size in 1 .. Array_Bytes;
   --  The first block after the MBR, the header and the entries.

   function Last_Usable_Block
     (Block_Size  : Natural;
      Block_Count : Rose.Words.Word_64)
      return Rose.Words.Word_64
   is (Block_Count - 2 - Rose.Words.Word_64 (Array_Blocks (Block_Size)))
   with Pre => Geometry_Valid (Block_Size, Block_Count);
   --  The last block before the backup entries and header.

   function Header_Valid
     (Item        : Header;
      Header_LBA  : Rose.Words.Word_64;
      Block_Size  : Natural;
      Block_Count : Rose.Words.Word_64)
      return Boolean
   with Pre => Geometry_Valid (Block_Size, Block_Count);
   --  Item, read from Header_LBA, is a header that its disk can have: the
   --  signature, size, address and CRC are right, the entries are the
   --  standard size and there are not too many, they are on the disk, and
   --  so is the range of blocks that partitions may use.

   --  The partitions in use

   subtype Partition_Count is Natural range 0 .. Max_Partitions;

   type Partition_Array is
     array (Natural range 0 .. Max_Partitions - 1) of Partition_Entry;

   type Partition_Table is
      record
         Count : Partition_Count := 0;
         Parts : Partition_Array;
      end record;

   function Valid_Entry
     (Item         : Partition_Entry;
      First_Usable : Rose.Words.Word_64;
      Last_Usable  : Rose.Words.Word_64)
      return Boolean
   is (First_Usable <= Item.First_LBA
       and then Item.First_LBA <= Item.Last_LBA
       and then Item.Last_LBA <= Last_Usable);
   --  The partition is not empty, and is in the blocks that partitions
   --  may use.

   function Disjoint (Left, Right : Partition_Entry) return Boolean
   is (Left.Last_LBA < Right.First_LBA
       or else Right.Last_LBA < Left.First_LBA);

   function Valid
     (Table        : Partition_Table;
      First_Usable : Rose.Words.Word_64;
      Last_Usable  : Rose.Words.Word_64)
      return Boolean
   is ((for all I in 0 .. Table.Count - 1 =>
          Valid_Entry (Table.Parts (I), First_Usable, Last_Usable))
       and then
         (for all J in 0 .. Table.Count - 1 =>
            (for all I in 0 .. J - 1 =>
               Disjoint (Table.Parts (I), Table.Parts (J)))));
   --  Every partition is valid, and no two overlap.

   procedure Add
     (Table        : in out Partition_Table;
      Item         : Partition_Entry;
      First_Usable : Rose.Words.Word_64;
      Last_Usable  : Rose.Words.Word_64;
      Success      : out Boolean)
   with Pre  => Valid (Table, First_Usable, Last_Usable),
        Post => Valid (Table, First_Usable, Last_Usable)
                and then
                  (if Success
                   then Table.Count = Table.Count'Old + 1
                     and then Table.Parts (Table.Count'Old) = Item
                   else Table = Table'Old);
   --  Add Item as the last partition, unless it is not valid, or overlaps
   --  one, or there is no room for it.

end Rose.Devices.GPT_Format;
