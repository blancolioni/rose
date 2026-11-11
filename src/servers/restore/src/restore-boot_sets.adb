with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Directories;
with Rose.System_Calls;
with Rose.Version;
with Rose.Words;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Releasable_Reader.Client;

package body Restore.Boot_Sets is

   use System.Storage_Elements;
   use Rose.Interfaces.Block_Device;
   use Rose.Interfaces.Block_Device.Client;

   Block_Bytes : constant := 4096;
   --  The block size of the boot partition's partition server
   --  (src/servers/init/boot.yaml), and of the menu

   Boot_Directory : constant String := "/rose/install/boot/";
   --  Where the install media has the boot set and the menus
   --  (scripts/make-boot-disk)

   Set_Prefix  : constant String := "# rose-boot-set ";
   --  The start of the menu's first line, which goes on with the slot and
   --  its version

   Placeholder : constant String := "@PREVIOUS_VERSION@";
   --  In the install media's menus, where the previous set's version goes

   Line_Feed : constant Storage_Element := 10;

   type Byte_Offset is new Rose.Words.Word_32;
   --  A byte's place on the boot partition.  FAT16 is smaller than
   --  4 GiB, and the kernel's runtime has no 64-bit division.

   Max_Sectors : constant Byte_Offset := 2 ** 23;
   --  The most 512-byte sectors that Byte_Offset can count

   type Cluster_Number is new Rose.Words.Word_32;

   End_Of_Chain : constant Cluster_Number := 16#FFF8#;

   subtype Short_Name is String (1 .. 11);
   --  A directory entry's name, in FAT's 8.3 form

   Device        : Block_Device_Client;
   Device_Blocks : Block_Address_Type := 0;

   Failed : Boolean := False;
   --  Set when a read of the partition fails

   Cache       : Storage_Array (1 .. Block_Bytes)
     with Alignment => 4096;
   Cached      : Block_Address_Type := 0;
   Cache_Valid : Boolean := False;
   --  The last block read

   Buffer : Storage_Array (1 .. Block_Bytes)
     with Alignment => 4096;
   --  The block being written

   Menu_Block : Storage_Array (1 .. Block_Bytes)
     with Alignment => 4096;
   --  The new menu

   Transfer : Storage_Array (1 .. Block_Bytes)
     with Alignment => 4096;
   --  The invocation buffer while Find and Write_Update run, which
   --  blocks and files are read into and written from.  Restore has none
   --  of its own: Rose.Console_IO makes one the first time it writes, and
   --  reading the log takes it away (Restore.Checkpoint_Log).

   Saved_Address : System.Address := System.Null_Address;
   Saved_Length  : Storage_Count := 0;
   --  The invocation buffer before Lend_Buffer

   --  The file system's geometry, from its boot sector

   Cluster_Bytes : Byte_Offset := 0;
   FAT_Offset    : Byte_Offset := 0;
   Root_Offset   : Byte_Offset := 0;
   Root_Entries  : Byte_Offset := 0;
   Data_Offset   : Byte_Offset := 0;
   Cluster_Count : Cluster_Number := 0;

   function Byte_At (Offset : Byte_Offset) return Byte_Offset;
   function Word_16_At (Offset : Byte_Offset) return Byte_Offset;
   function Word_32_At (Offset : Byte_Offset) return Byte_Offset;
   --  The little-endian value at Offset

   function Cluster_Offset (Cluster : Cluster_Number) return Byte_Offset;

   procedure Find_Entry
     (Directory : Cluster_Number;
      Name      : Short_Name;
      First     : out Cluster_Number;
      Size      : out Byte_Offset;
      Found     : out Boolean);
   --  Find Name in Directory, or in the root directory if Directory is 0

   procedure Find_Layout
     (Partition : out Boot_Partition;
      Found     : out Boolean);
   --  Find without the messages

   procedure Get_Extent
     (First  : Cluster_Number;
      Size   : Byte_Offset;
      Extent : out File_Extent;
      Found  : out Boolean);
   --  The blocks of the file whose first cluster is First.  Found is
   --  False if the file isn't contiguous, or doesn't start and end on a
   --  block.

   procedure Lend_Buffer;
   --  Make Transfer the invocation buffer

   procedure Load (Address : Block_Address_Type);
   --  Read the block at Address into Cache, if it isn't there

   function Matches
     (Offset : Byte_Offset;
      Name   : Short_Name)
      return Boolean;

   function Next_Cluster (Cluster : Cluster_Number) return Cluster_Number;
   --  The next cluster in the chain from the FAT; 0 if Cluster isn't one

   procedure Prepare_Menu
     (Path      : String;
      Partition : Boot_Partition;
      Ready     : out Boolean);
   --  Read the menu at Path into Menu_Block, with the current set's
   --  version in place of Placeholder

   procedure Read_Geometry (Found : out Boolean);
   --  Read the boot sector.  Found is False if it isn't FAT16 with
   --  512-byte sectors.

   procedure Read_Menu_Line
     (Partition : in out Boot_Partition;
      Found     : out Boolean);
   --  Read the current slot and its version from the menu's first line

   procedure Return_Buffer;
   --  Put back the invocation buffer from before Lend_Buffer

   procedure Write_Block
     (Address : Block_Address_Type;
      Done    : out Boolean);
   --  Write Buffer to the block at Address

   procedure Write_Set
     (Path   : String;
      Extent : File_Extent;
      Done   : out Boolean);
   --  Write the file at Path to the blocks of Extent, if it fits

   procedure Write_Steps
     (Partition : Boot_Partition;
      Written   : out Boolean);
   --  Write_Update, with Transfer as the invocation buffer

   -------------
   -- Byte_At --
   -------------

   function Byte_At (Offset : Byte_Offset) return Byte_Offset is
   begin
      Load (Block_Address_Type (Offset / Block_Bytes));
      return Byte_Offset
        (Cache (Storage_Offset (Offset mod Block_Bytes) + 1));
   end Byte_At;

   --------------------
   -- Cluster_Offset --
   --------------------

   function Cluster_Offset (Cluster : Cluster_Number) return Byte_Offset is
   begin
      return Data_Offset + Byte_Offset (Cluster - 2) * Cluster_Bytes;
   end Cluster_Offset;

   ----------
   -- Find --
   ----------

   procedure Find
     (Partition : out Boot_Partition;
      Found     : out Boolean)
   is
   begin
      Lend_Buffer;
      Find_Layout (Partition, Found);
      Return_Buffer;

      if Found then
         Rose.Console_IO.Put
           ("restore: hd0 boots Rose "
            & Partition.Version (1 .. Partition.Version_Last)
            & " from slot ");
         Rose.Console_IO.Put
           (if Partition.Current = Slot_A then 'a' else 'b');
         Rose.Console_IO.New_Line;
      elsif Failed then
         Rose.Console_IO.Put_Line
           ("restore: cannot read hd0's boot partition");
      else
         Rose.Console_IO.Put_Line
           ("restore: hd0's boot partition has one boot set, not two, as");
         Rose.Console_IO.Put_Line
           ("restore: on a disk installed before Rose could be updated.");
         Rose.Console_IO.Put_Line
           ("restore: It can only be installed again");
      end if;
   end Find;

   ----------------
   -- Find_Entry --
   ----------------

   procedure Find_Entry
     (Directory : Cluster_Number;
      Name      : Short_Name;
      First     : out Cluster_Number;
      Size      : out Byte_Offset;
      Found     : out Boolean)
   is
      Cluster      : Cluster_Number := Directory;
      Entry_Offset : Byte_Offset;
      Area_End     : Byte_Offset;
      Clusters     : Cluster_Number := 1;
   begin
      First := 0;
      Size := 0;
      Found := False;

      if Directory = 0 then
         Entry_Offset := Root_Offset;
         Area_End := Root_Offset + 32 * Root_Entries;
      else
         Entry_Offset := Cluster_Offset (Cluster);
         Area_End := Entry_Offset + Cluster_Bytes;
      end if;

      loop
         exit when Failed;

         if Entry_Offset >= Area_End then
            exit when Directory = 0;
            Cluster := Next_Cluster (Cluster);
            Clusters := Clusters + 1;
            exit when Cluster < 2
              or else Cluster >= End_Of_Chain
              or else Clusters > Cluster_Count;
            Entry_Offset := Cluster_Offset (Cluster);
            Area_End := Entry_Offset + Cluster_Bytes;
         end if;

         declare
            First_Byte : constant Byte_Offset := Byte_At (Entry_Offset);
            Attributes : constant Byte_Offset := Byte_At (Entry_Offset + 11);
         begin
            exit when First_Byte = 0;

            --  Skip deleted entries, and the parts of long names.

            if First_Byte /= 16#E5#
              and then Attributes /= 16#0F#
              and then Matches (Entry_Offset, Name)
            then
               First := Cluster_Number (Word_16_At (Entry_Offset + 26));
               Size := Word_32_At (Entry_Offset + 28);
               Found := not Failed;
               exit;
            end if;
         end;

         Entry_Offset := Entry_Offset + 32;
      end loop;
   end Find_Entry;

   -----------------
   -- Find_Layout --
   -----------------

   procedure Find_Layout
     (Partition : out Boot_Partition;
      Found     : out Boolean)
   is
      use type Rose.Capabilities.Capability;
      Block_Size : Block_Size_Type;
      Boot_Dir   : Cluster_Number;
      Grub_Dir   : Cluster_Number;
      First      : Cluster_Number;
      Size       : Byte_Offset;
   begin
      Partition := (others => <>);
      Found := False;
      Failed := False;
      Cache_Valid := False;

      if Boot_Partition_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Open (Device, Boot_Partition_Cap);
      Get_Parameters (Device, Device_Blocks, Block_Size);

      if Block_Size /= Block_Bytes then
         return;
      end if;

      Read_Geometry (Found);
      if not Found then
         return;
      end if;

      Find_Entry (0, "BOOT       ", Boot_Dir, Size, Found);
      if not Found then
         return;
      end if;

      Find_Entry (Boot_Dir, "A       TAR", First, Size, Found);
      if not Found then
         return;
      end if;
      Get_Extent (First, Size, Partition.Slots (Slot_A), Found);
      if not Found then
         return;
      end if;

      Find_Entry (Boot_Dir, "B       TAR", First, Size, Found);
      if not Found then
         return;
      end if;
      Get_Extent (First, Size, Partition.Slots (Slot_B), Found);
      if not Found then
         return;
      end if;

      Find_Entry (Boot_Dir, "GRUB       ", Grub_Dir, Size, Found);
      if not Found then
         return;
      end if;

      Find_Entry (Grub_Dir, "GRUB    CFG", First, Size, Found);
      if not Found then
         return;
      end if;
      Get_Extent (First, Size, Partition.Menu, Found);
      if not Found or else Partition.Menu.Count /= 1 then
         Found := False;
         return;
      end if;

      Read_Menu_Line (Partition, Found);
   end Find_Layout;

   ----------------
   -- Get_Extent --
   ----------------

   procedure Get_Extent
     (First  : Cluster_Number;
      Size   : Byte_Offset;
      Extent : out File_Extent;
      Found  : out Boolean)
   is
      Start    : Byte_Offset;
      Cluster  : Cluster_Number := First;
      Clusters : Cluster_Number;
   begin
      Extent := (others => <>);
      Found := False;

      if First < 2
        or else First >= Cluster_Count + 2
        or else Size = 0
        or else Size mod Block_Bytes /= 0
        or else Size mod Cluster_Bytes /= 0
      then
         return;
      end if;

      Start := Cluster_Offset (First);
      if Start mod Block_Bytes /= 0 then
         return;
      end if;

      Clusters := Cluster_Number (Size / Cluster_Bytes);
      for I in 2 .. Clusters loop
         if Next_Cluster (Cluster) /= Cluster + 1 then
            return;
         end if;
         Cluster := Cluster + 1;
      end loop;

      if Next_Cluster (Cluster) < End_Of_Chain or else Failed then
         return;
      end if;

      Extent :=
        (First => Block_Address_Type (Start / Block_Bytes),
         Count => Block_Address_Type (Size / Block_Bytes));
      Found := Extent.First + Extent.Count <= Device_Blocks;
   end Get_Extent;

   -----------------
   -- Lend_Buffer --
   -----------------

   procedure Lend_Buffer is
   begin
      Saved_Address := Rose.System_Calls.Buffer_Address;
      Saved_Length := Rose.System_Calls.Buffer_Length;
      Rose.System_Calls.Use_Buffer (Transfer'Address, Transfer'Length);
   end Lend_Buffer;

   ----------
   -- Load --
   ----------

   procedure Load (Address : Block_Address_Type) is
   begin
      if Cache_Valid and then Cached = Address then
         return;
      end if;

      if Address >= Device_Blocks then
         Failed := True;
      else
         Read_Blocks (Device, Address, 1, Cache);
         if Rose.Interfaces.Block_Device.Client.Has_Error then
            Failed := True;
         end if;
      end if;

      if Failed then
         Cache := (others => 0);
      end if;

      Cached := Address;
      Cache_Valid := True;
   end Load;

   -------------
   -- Matches --
   -------------

   function Matches
     (Offset : Byte_Offset;
      Name   : Short_Name)
      return Boolean
   is
   begin
      for I in Name'Range loop
         if Byte_At (Offset + Byte_Offset (I - 1))
           /= Character'Pos (Name (I))
         then
            return False;
         end if;
      end loop;
      return True;
   end Matches;

   ------------------
   -- Next_Cluster --
   ------------------

   function Next_Cluster (Cluster : Cluster_Number) return Cluster_Number is
   begin
      if Cluster < 2 or else Cluster >= Cluster_Count + 2 then
         return 0;
      end if;
      return Cluster_Number
        (Word_16_At (FAT_Offset + 2 * Byte_Offset (Cluster)));
   end Next_Cluster;

   ------------------
   -- Prepare_Menu --
   ------------------

   procedure Prepare_Menu
     (Path      : String;
      Partition : Boot_Partition;
      Ready     : out Boolean)
   is
      use type Rose.Directories.File_Size;
      use Rose.Interfaces.Releasable_Reader.Client;
      Reader  : Releasable_Reader_Client;
      Filled  : Storage_Count := 0;
      Version : String renames
                  Partition.Version (1 .. Partition.Version_Last);
      Next    : Storage_Offset := 1;
      Last    : Storage_Count := 0;
   begin
      Ready := False;

      if not Rose.Directories.Exists (Path)
        or else Rose.Directories.Size (Path) /= Block_Bytes
      then
         Rose.Console_IO.Put_Line
           ("restore: " & Path & " is missing, or isn't one block");
         return;
      end if;

      Rose.Directories.Open (Reader, Path);
      loop
         declare
            Count : Storage_Count;
         begin
            Read (Reader, Buffer (Filled + 1 .. Buffer'Last), Count);
            exit when Rose.Interfaces.Releasable_Reader.Client.Has_Error
              or else Count = 0;
            Filled := Filled + Count;
            exit when Filled = Buffer'Last;
         end;
      end loop;
      Release (Reader);

      if Filled /= Buffer'Last then
         Rose.Console_IO.Put_Line ("restore: cannot read " & Path);
         return;
      end if;

      --  Copy the menu, with the version in place of the placeholder.
      --  The menu is padded with line feeds, so a longer version takes
      --  the place of some of them.

      Menu_Block := (others => Line_Feed);

      while Next <= Buffer'Last loop
         declare
            Found : Boolean := Next + Placeholder'Length - 1 <= Buffer'Last;
         begin
            if Found then
               for I in Placeholder'Range loop
                  if Buffer (Next + Storage_Offset (I - Placeholder'First))
                    /= Character'Pos (Placeholder (I))
                  then
                     Found := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Found then
               if Last + Version'Length > Menu_Block'Last then
                  Rose.Console_IO.Put_Line
                    ("restore: " & Path & " has no room for the version");
                  return;
               end if;
               for Ch of Version loop
                  Last := Last + 1;
                  Menu_Block (Last) := Character'Pos (Ch);
               end loop;
               Next := Next + Placeholder'Length;
            else
               if Last < Menu_Block'Last then
                  Last := Last + 1;
                  Menu_Block (Last) := Buffer (Next);
               elsif Buffer (Next) /= Line_Feed then
                  Rose.Console_IO.Put_Line
                    ("restore: " & Path & " has no room for the version");
                  return;
               end if;
               Next := Next + 1;
            end if;
         end;
      end loop;

      Ready := True;
   end Prepare_Menu;

   -------------------
   -- Read_Geometry --
   -------------------

   procedure Read_Geometry (Found : out Boolean) is
      Sector_Bytes        : constant Byte_Offset := Word_16_At (11);
      Sectors_Per_Cluster : constant Byte_Offset := Byte_At (13);
      Reserved            : constant Byte_Offset := Word_16_At (14);
      FAT_Count           : constant Byte_Offset := Byte_At (16);
      FAT_Sectors         : constant Byte_Offset := Word_16_At (22);
      Total               : Byte_Offset := Word_16_At (19);
   begin
      Found := False;
      Root_Entries := Word_16_At (17);

      if Total = 0 then
         Total := Word_32_At (32);
      end if;

      if Failed
        or else Sector_Bytes /= 512
        or else Sectors_Per_Cluster = 0
        or else FAT_Count = 0
        or else Total >= Max_Sectors
        or else Block_Address_Type ((Total + 7) / 8) > Device_Blocks
      then
         return;
      end if;

      Cluster_Bytes := Sectors_Per_Cluster * 512;
      FAT_Offset := Reserved * 512;
      Root_Offset := FAT_Offset + FAT_Count * FAT_Sectors * 512;
      Data_Offset := Root_Offset + (Root_Entries * 32 + 511) / 512 * 512;

      if Data_Offset >= Total * 512 then
         return;
      end if;

      Cluster_Count :=
        Cluster_Number ((Total * 512 - Data_Offset) / Cluster_Bytes);

      Found := Cluster_Count >= 4085 and then Cluster_Count < 65525;
   end Read_Geometry;

   --------------------
   -- Read_Menu_Line --
   --------------------

   procedure Read_Menu_Line
     (Partition : in out Boot_Partition;
      Found     : out Boolean)
   is
      Start : constant Byte_Offset :=
                Byte_Offset (Partition.Menu.First) * Block_Bytes;
      Next  : Byte_Offset := Start + Set_Prefix'Length + 2;
   begin
      Found := False;

      for I in Set_Prefix'Range loop
         if Byte_At (Start + Byte_Offset (I - 1))
           /= Character'Pos (Set_Prefix (I))
         then
            return;
         end if;
      end loop;

      case Byte_At (Start + Set_Prefix'Length) is
         when Character'Pos ('a') =>
            Partition.Current := Slot_A;
         when Character'Pos ('b') =>
            Partition.Current := Slot_B;
         when others =>
            return;
      end case;

      if Byte_At (Start + Set_Prefix'Length + 1) /= Character'Pos (' ') then
         return;
      end if;

      Partition.Version_Last := 0;
      while Next < Start + Block_Bytes loop
         declare
            Ch : constant Byte_Offset := Byte_At (Next);
         begin
            exit when Ch = Byte_Offset (Line_Feed);
            if Ch < 32 or else Ch > 126
              or else Partition.Version_Last = Max_Version_Length
            then
               return;
            end if;
            Partition.Version_Last := Partition.Version_Last + 1;
            Partition.Version (Partition.Version_Last) :=
              Character'Val (Ch);
         end;
         Next := Next + 1;
      end loop;

      Found := Partition.Version_Last > 0 and then not Failed;
   end Read_Menu_Line;

   -------------------
   -- Return_Buffer --
   -------------------

   procedure Return_Buffer is
   begin
      Rose.System_Calls.Use_Buffer (Saved_Address, Saved_Length);
   end Return_Buffer;

   ----------------
   -- Word_16_At --
   ----------------

   function Word_16_At (Offset : Byte_Offset) return Byte_Offset is
   begin
      return Byte_At (Offset) + 256 * Byte_At (Offset + 1);
   end Word_16_At;

   ----------------
   -- Word_32_At --
   ----------------

   function Word_32_At (Offset : Byte_Offset) return Byte_Offset is
   begin
      return Word_16_At (Offset) + 65536 * Word_16_At (Offset + 2);
   end Word_32_At;

   -----------------
   -- Write_Block --
   -----------------

   procedure Write_Block
     (Address : Block_Address_Type;
      Done    : out Boolean)
   is
   begin
      Write_Blocks (Device, Address, 1, Buffer);
      Done := not Rose.Interfaces.Block_Device.Client.Has_Error;
      if Cache_Valid and then Cached = Address then
         Cache_Valid := False;
      end if;
   end Write_Block;

   ---------------
   -- Write_Set --
   ---------------

   procedure Write_Set
     (Path   : String;
      Extent : File_Extent;
      Done   : out Boolean)
   is
      use type Rose.Directories.File_Size;
      use Rose.Interfaces.Releasable_Reader.Client;
      Reader  : Releasable_Reader_Client;
      Filled  : Storage_Count := 0;
      Written : Block_Address_Type := 0;
   begin
      Done := False;

      if not Rose.Directories.Exists (Path) then
         Rose.Console_IO.Put_Line ("restore: cannot find " & Path);
         return;
      end if;

      if Rose.Directories.Size (Path)
        > Rose.Directories.File_Size (Extent.Count) * Block_Bytes
      then
         Rose.Console_IO.Put_Line
           ("restore: " & Path & " is bigger than a slot");
         return;
      end if;

      Rose.Directories.Open (Reader, Path);

      loop
         declare
            Count : Storage_Count;
         begin
            Read (Reader, Buffer (Filled + 1 .. Buffer'Last), Count);
            exit when Rose.Interfaces.Releasable_Reader.Client.Has_Error
              or else Count = 0;
            Filled := Filled + Count;
         end;

         if Filled = Buffer'Last then
            Write_Block (Extent.First + Written, Done);
            Written := Written + 1;
            Filled := 0;
            exit when not Done or else Written = Extent.Count;
         end if;
      end loop;

      if Filled > 0 and then Written < Extent.Count then
         Buffer (Filled + 1 .. Buffer'Last) := (others => 0);
         Write_Block (Extent.First + Written, Done);
         Written := Written + 1;
      end if;

      Release (Reader);

      Done := Done and then Written > 0;

      if Done then
         Rose.Console_IO.Put ("restore: wrote the boot set: ");
         Rose.Console_IO.Put (Integer (Written));
         Rose.Console_IO.Put_Line (" blocks");
      end if;
   end Write_Set;

   -----------------
   -- Write_Steps --
   -----------------

   procedure Write_Steps
     (Partition : Boot_Partition;
      Written   : out Boolean)
   is
      Other  : constant Slot_Name :=
                 (if Partition.Current = Slot_A then Slot_B else Slot_A);
      Letter : constant Character :=
                 (if Other = Slot_A then 'a' else 'b');
      Old    : String renames
                 Partition.Version (1 .. Partition.Version_Last);
      Done   : Boolean;
   begin
      Written := False;

      --  The menu is ready before anything is written, so that a menu
      --  that can't be written leaves the disk as it was.

      Prepare_Menu
        (Boot_Directory & "grub-" & Letter & ".cfg", Partition, Done);
      if not Done then
         Rose.Console_IO.Put_Line ("restore: hd0 is unchanged");
         return;
      end if;

      Rose.Console_IO.Put_Line
        ("restore: writing Rose " & Rose.Version.Version_String
         & " to slot " & Letter);

      Write_Set
        (Boot_Directory & "boot-set.tar", Partition.Slots (Other), Done);
      if not Done then
         Rose.Console_IO.Put_Line
           ("restore: the boot set was not written; hd0 still boots Rose "
            & Old);
         return;
      end if;

      --  The menu makes the new set current.

      Buffer := Menu_Block;
      Write_Block (Partition.Menu.First, Done);
      if not Done then
         Rose.Console_IO.Put_Line
           ("restore: the menu was not written; hd0 may not boot");
         return;
      end if;

      Rose.Console_IO.Put_Line
        ("restore: hd0 boots Rose " & Rose.Version.Version_String
         & ", and its menu has Previous: Rose " & Old);
      Written := True;
   end Write_Steps;

   ------------------
   -- Write_Update --
   ------------------

   procedure Write_Update
     (Partition : Boot_Partition;
      Written   : out Boolean)
   is
   begin
      Lend_Buffer;
      Write_Steps (Partition, Written);
      Return_Buffer;
   end Write_Update;

end Restore.Boot_Sets;
