with Rose.Checkpoints;
with Rose.Limits;
with Rose.Allocators;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Trace;
with Rose.System_Calls.Server;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Server;
with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Region;
with Rose.Interfaces.Region.Server;
with Rose.Interfaces.Stream_Reader;

package body Store.Devices is

   use type Rose.Objects.Object_Id;
   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;

   Max_Backing_Stores  : constant := 16;
   Max_Regions         : constant := 256;
   Max_Banks           : constant := 64;
   Block_Size_Bits     : constant := 2;
   Store_Size_Bits     : constant := 21;
   Minimum_Block_Pages : constant := 2 ** Block_Size_Bits;
   Minimum_Block_Size  : constant :=
                           Minimum_Block_Pages * Rose.Limits.Page_Size;
   Max_Open_Streams    : constant := 10;

   Root_Bank : constant := 1;
   --  The bank that every other is below.

   Max_Store_Pages : constant := 2 ** (Store_Size_Bits - 1);
   --  The pages that Allocator can hand out, on all the backing stores
   --  together: 4 GiB.  Its tree of 2 ** (Store_Size_Bits - Block_Size_Bits)
   --  blocks has half that many leaves.

   Next_Page : Rose.Objects.Object_Id := Rose.Objects.Page_Object_Id'First;
   --  The first page id that no region has had.  A region's pages are
   --  the next ones, which are never used again, so a page id is
   --  persistent whatever happens to the region (#305).

   Max_Mapped_Blocks : constant := 2 ** 14;
   --  The blocks that can have space on a device at once: 256 MiB.

   type Map_Entry is
      record
         Page  : Rose.Objects.Object_Id;
         Next  : Rose.Words.Word_32;
         Block : Rose.Words.Word_32;
         Fresh : Rose.Words.Word_32;
      end record;
   --  A block of a region that has space on a device, found by the id of
   --  its first page.  Block is the index that Allocator gave it, and
   --  bit I of Fresh is set while page I of the block has not been
   --  written, so reads as zeroes without a read from the device (#203).
   --  Page is zero for an entry that isn't in use, and Next links the
   --  entries of a bucket, or the unused ones.

   type Map_Array is array (1 .. Max_Mapped_Blocks) of Map_Entry;
   type Bucket_Array is
     array (0 .. Max_Mapped_Blocks - 1) of Rose.Words.Word_32;

   Map          : Map_Array :=
                    (others => (Page => 0, Next => 0, Block => 0, Fresh => 0));
   Buckets      : Bucket_Array := (others => 0);
   Map_High     : Natural := 0;
   Map_Count    : Natural := 0;
   Free_Entries : Rose.Words.Word_32 := 0;
   --  A block of a region has space on a device from the first write to
   --  one of its pages (#305), and the map says where.  Map_High is the
   --  highest entry used, Map_Count the entries in use, and Free_Entries
   --  the unused ones below Map_High.  The pages of a block that isn't in
   --  the map are all fresh.  The tables are written when the store
   --  starts, so every page of them is mapped before it is used: a page
   --  fault in the store while the memory server is waiting for it would
   --  deadlock them both.

   type Backing_Store_Record is
      record
         Allocated       : Boolean := False;
         Client          : Rose.Interfaces.Block_Device.Client
           .Block_Device_Client;
         Base            : Rose.Objects.Object_Id := 0;
         Bound           : Rose.Objects.Object_Id := 0;
         Device_Bound    : Rose.Objects.Object_Id := 0;
         Allocator_Base  : Positive;
         Allocator_Bound : Positive;
         Reads           : Natural := 0;
         Read_Ticks      : Natural := 0;
         Writes          : Natural := 0;
         Write_Ticks     : Natural := 0;
         --  Pages read and written, and the time they took (#196).
         Zero_Reads      : Natural := 0;
         --  Reads of fresh pages, answered without the device.
      end record;

   type Backing_Store_Array is
     array (1 .. Max_Backing_Stores) of Backing_Store_Record;

   Backing_Stores      : Backing_Store_Array;
   Backing_Store_Count : Natural := 0;

   type Region_Record is
      record
         In_Use         : Boolean := False;
         Base, Bound    : Rose.Objects.Object_Id := 0;
         Access_Cap     : Rose.Capabilities.Capability := 0;
         Root           : Natural := 0;
         Mapped_Blocks  : Natural := 0;
         Reads          : Natural := 0;
         Zero_Reads     : Natural := 0;
         Writes         : Natural := 0;
         Bank           : Natural := 0;
      end record;
   --  A region.  Root is the region that its page ids were reserved for,
   --  which is the region itself unless it is a subregion, and
   --  Mapped_Blocks the blocks of its pages that have space on a device,
   --  which only a root region keeps.  Bank is the bank that the region
   --  is charged to, which is its parent's for a subregion.

   type Region_Array is
     array (1 .. Max_Regions) of Region_Record;

   Regions      : Region_Array;
   Region_Count : Natural := 0;
   --  The highest region index used. A released region's index is used
   --  again.

   type Bank_Record is
      record
         In_Use       : Boolean := False;
         Parent       : Natural := 0;
         Access_Cap   : Rose.Capabilities.Capability := 0;
         Page_Limit   : Rose.Words.Word_64 := 0;
         Region_Limit : Rose.Words.Word := 0;
         Pages        : Rose.Words.Word_64 := 0;
         Regions      : Rose.Words.Word := 0;
      end record;
   --  A bank (ADR 0014).  Parent is zero for the root bank.  A limit of
   --  zero is no limit.  Pages and Regions are what has been reserved
   --  from the bank and the banks below it: its pages are the blocks that
   --  have space on a device, and a subregion counts as a region.

   type Bank_Array is
     array (1 .. Max_Banks) of Bank_Record;

   Banks : Bank_Array;

   Table_Page   : System.Storage_Elements.Storage_Array
                    (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   Table_Header : Rose.Checkpoints.Store_Table_Header
     with Import, Address => Table_Page'Address;
   Region_Table : Rose.Checkpoints.Region_Page
     with Import, Address => Table_Page'Address;
   Map_Table    : Rose.Checkpoints.Map_Page
     with Import, Address => Table_Page'Address;
   --  The page in which Checkpoint builds the region table.

   Restore_Region_Count : Natural := 0;
   Restore_Last_Region  : Natural := 0;
   Restore_First_Map    : Natural := 0;
   Restore_Last_Map     : Natural := 0;
   Restore_Entry_Count  : Natural := 0;
   --  While a restore gives the tables back: the regions they hold, the
   --  last table page that holds regions, the table pages that hold the
   --  map (zero if there are none), and the entries of the map still to
   --  come.

   function New_Region return Natural;
   --  The index of a region that is not in use, or zero if there is none.

   procedure Create_Root_Bank;
   --  Make the root bank, if it doesn't exist yet.

   function Is_Within
     (Bank     : Natural;
      Ancestor : Positive)
      return Boolean;
   --  True if Bank is Ancestor, or is below it.

   function Within_Limits
     (Bank    : Positive;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word)
      return Boolean;
   --  True if charging Pages and Regions to Bank would keep it, and every
   --  bank above it, within its limits.

   procedure Charge
     (Bank    : Natural;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word);
   --  Charge Pages and Regions to Bank and every bank above it.

   procedure Uncharge
     (Bank    : Natural;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word);

   function Reserve_From
     (Bank : Positive;
      Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability;

   function Find_Entry
     (Page : Rose.Objects.Object_Id)
      return Natural;
   --  The map entry of the block that Page is in, or zero if the block
   --  has no space on a device.

   function Insert_Entry
     (Page  : Rose.Objects.Object_Id;
      Block : Rose.Words.Word_32;
      Fresh : Rose.Words.Word_32)
      return Natural;
   --  Add the block that Page is in to the map, or return zero if the
   --  map is full.

   function Map_Block
     (Root : in out Region_Record;
      Page : Rose.Objects.Object_Id)
      return Natural;
   --  Give the block that Page is in space on a device, charge it to the
   --  bank of Root, the region that has the page, and add it to the map.
   --  Zero if there is no space or the bank is full.

   procedure Remove_Entry (Index : Positive);
   --  Take an entry out of the map.

   function Device_Page
     (Entry_Index : Positive;
      Page        : Rose.Objects.Object_Id;
      Store       : out Positive)
      return Rose.Interfaces.Block_Device.Block_Address_Type;
   --  The page of the device that holds Page, which is in the block of
   --  an entry of the map, and the backing store that the device is.

   function Is_Fresh
     (Page : Rose.Objects.Object_Id)
      return Boolean;
   --  True if Page has not been written.

   function Page_Bit
     (Page : Rose.Objects.Object_Id)
      return Rose.Words.Word_32
   is (Rose.Words.Word_32'(2) ** Natural (Page mod Minimum_Block_Pages));
   --  The bit of a map entry's Fresh that is for Page.

   function Is_Zero
     (Data : System.Storage_Elements.Storage_Array)
      return Boolean;
   --  True if every element of Data is zero.

   procedure Restore_Entry
     (Page  : Rose.Objects.Object_Id;
      Block : Rose.Words.Word_32;
      Fresh : Rose.Words.Word_32);
   --  Give back the entry of a block that had space on a device.

   Stream_Buffer_Length : constant := 4096;

   type Region_Stream_Record is
      record
         Open           : Boolean := False;
         Cap            : Rose.Capabilities.Capability := 0;
         Region     : Positive;
         Current_Page   : Rose.Objects.Page_Object_Id;
         Current_Offset : System.Storage_Elements.Storage_Offset;
         Stream_Length  : System.Storage_Elements.Storage_Count;
         Current_Buffer : System.Storage_Elements.Storage_Array
           (1 .. Stream_Buffer_Length);
         Buffer_Length  : System.Storage_Elements.Storage_Count;
         Buffer_Offset  : System.Storage_Elements.Storage_Offset;
      end record;

   type Region_Stream_Array is
     array (Rose.Objects.Capability_Identifier range 1 .. Max_Open_Streams)
     of Region_Stream_Record;

   Region_Streams : Region_Stream_Array;

   package Space_Allocators is
     new Rose.Allocators (Store_Size_Bits - Block_Size_Bits);

   Allocator : Space_Allocators.Store;

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word;
      Base       : out Rose.Objects.Object_Id;
      Bound      : out Rose.Objects.Object_Id);

   -----------------------
   -- Add_Backing_Store --
   -----------------------

   procedure Add_Backing_Store
     (Id     : in Rose.Objects.Capability_Identifier;
      Device : in Rose.Capabilities.Capability)
   is
      use type Rose.Objects.Capability_Identifier;
      use Rose.Interfaces.Block_Device, Rose.Interfaces.Block_Device.Client;
      Client : Block_Device_Client;
      Block_Count : Block_Address_Type;
      Block_Size  : Block_Size_Type;
   begin
      if Id /= 0 then
         Rose.Console_IO.Put
           ("storage: invalid id: ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.New_Line;
      end if;

      Open (Client, Device);
      Get_Parameters (Client, Block_Count, Block_Size);
      if Block_Size /= Rose.Limits.Page_Size then
         Rose.Console_IO.Put ("storage: incorrect block size: expected ");
         Rose.Console_IO.Put (Natural (Rose.Limits.Page_Size));
         Rose.Console_IO.Put (" but found ");
         Rose.Console_IO.Put (Natural (Block_Size));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Rose.Console_IO.Put ("storage: add backing store: size = ");
      Rose.Console_IO.Put (Natural (Block_Count) / 1024
                           * Natural (Block_Size)
                           / 1024);
      Rose.Console_IO.Put ("M");
      Rose.Console_IO.New_Line;

      declare
         use Rose.Objects;
         Base, Bound : Rose.Objects.Object_Id;
      begin
         New_Store (Client,
                    Rose.Words.Word (Block_Count),
                    Base, Bound);

         --  The store's page ids run to a power of two, but only the
         --  blocks that lie wholly on the device can be allocated: a page
         --  beyond it would be read and written past the end of the
         --  partition (#321).

         Bound := Base + Object_Id (Block_Count);

         if Bound - Page_Object_Id'First > Max_Store_Pages then
            Rose.Console_IO.Put ("storage: only the first ");
            Rose.Console_IO.Put
              (Natural ((Max_Store_Pages - (Base - Page_Object_Id'First))
               / 256));
            Rose.Console_IO.Put_Line ("M of this backing store can be used");
            Bound := Page_Object_Id'First + Max_Store_Pages;
         end if;

         Space_Allocators.Add_Range
           (Allocator => Allocator,
            Base      =>
              Natural
                ((Base - Page_Object_Id'First) / Minimum_Block_Pages) + 1,
            Bound     =>
              Natural ((Bound - Page_Object_Id'First) / Minimum_Block_Pages)
            + 1);
      end;

   end Add_Backing_Store;

   ------------
   -- Charge --
   ------------

   procedure Charge
     (Bank    : Natural;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word)
   is
      Index : Natural := Bank;
   begin
      while Index in 1 .. Max_Banks loop
         Banks (Index).Pages := Banks (Index).Pages + Pages;
         Banks (Index).Regions := Banks (Index).Regions + Regions;
         Index := Banks (Index).Parent;
      end loop;
   end Charge;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Id          : Rose.Objects.Capability_Identifier;
      Append_Page : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
      use Rose.Checkpoints;
      Count       : Natural := 0;
      Slot        : Natural := 0;
      Map_Pages   : Natural;
      Entry_Slot  : Natural := 0;

      procedure Append;
      --  Append Table_Page to the log, and clear it.

      ------------
      -- Append --
      ------------

      procedure Append is
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Append_Page);
         Rose.System_Calls.Send_Buffer
           (Params, Rose.Limits.Page_Size, Table_Page'Address, False);
         Rose.System_Calls.Invoke_Capability (Params);
         Table_Page := (others => 0);
      end Append;

   begin
      for Region of Regions (1 .. Region_Count) loop
         if Region.In_Use then
            Count := Count + 1;
         end if;
      end loop;

      Map_Pages := (Map_Count + Entries_Per_Page - 1) / Entries_Per_Page;

      Table_Page := (others => 0);
      Table_Header.Magic := Store_Table_Magic;
      Table_Header.Version := Store_Table_Version;
      Table_Header.Next_Page := Rose.Words.Word_64 (Next_Page);
      Table_Header.Page_Count :=
        Rose.Words.Word_32
          (1 + (Count + Regions_Per_Page - 1) / Regions_Per_Page
           + Map_Pages);
      Table_Header.Region_Count := Rose.Words.Word_32 (Count);
      Table_Header.Map_Page_Count := Rose.Words.Word_32 (Map_Pages);
      Table_Header.Map_Entry_Count := Rose.Words.Word_32 (Map_Count);
      Append;

      for Index in 1 .. Region_Count loop
         declare
            Region : Region_Record renames Regions (Index);
         begin
            if Region.In_Use then
               Slot := Slot + 1;
               Region_Table.Regions (Slot) :=
                 Saved_Region'
                   (Index  => Rose.Words.Word_32 (Index),
                    Root   => Rose.Words.Word_32 (Region.Root),
                    Base   => Rose.Words.Word_64 (Region.Base),
                    Bound  => Rose.Words.Word_64 (Region.Bound),
                    Blocks => Rose.Words.Word_32 (Region.Mapped_Blocks),
                    Reserved => 0);
               if Slot = Regions_Per_Page then
                  Append;
                  Slot := 0;
               end if;
            end if;
         end;
      end loop;

      if Slot > 0 then
         Append;
      end if;

      --  Then the map.

      for Index in 1 .. Map_High loop
         if Map (Index).Page /= 0 then
            Entry_Slot := Entry_Slot + 1;
            Map_Table.Entries (Entry_Slot) :=
              Saved_Map_Entry'
                (Page  => Rose.Words.Word_64 (Map (Index).Page),
                 Block => Map (Index).Block,
                 Fresh => Map (Index).Fresh);
            if Entry_Slot = Entries_Per_Page then
               Append;
               Entry_Slot := 0;
            end if;
         end if;
      end loop;

      if Entry_Slot > 0 then
         Append;
      end if;

      --  A new cap comes with each checkpoint.

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Append_Page);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Checkpoint;

   ----------------------
   -- Create_Root_Bank --
   ----------------------

   procedure Create_Root_Bank is
   begin
      if not Banks (Root_Bank).In_Use then
         Banks (Root_Bank) :=
           (In_Use     => True,
            Access_Cap =>
              Rose.System_Calls.Server.Create_Endpoint
                (Create_Endpoint_Cap,
                 Rose.Interfaces.Bank.Bank_Interface,
                 Rose.Objects.Capability_Identifier (Root_Bank)),
            others     => <>);
      end if;
   end Create_Root_Bank;

   ---------------------
   -- Create_Sub_Bank --
   ---------------------

   function Create_Sub_Bank
     (Id           : Rose.Objects.Capability_Identifier;
      Page_Limit   : Rose.Words.Word_64;
      Region_Limit : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      Parent : constant Natural := Natural (Id);
   begin
      if Parent not in 1 .. Max_Banks or else not Banks (Parent).In_Use then
         return 0;
      end if;

      for Index in Root_Bank + 1 .. Max_Banks loop
         if not Banks (Index).In_Use then
            Banks (Index) :=
              (In_Use       => True,
               Parent       => Parent,
               Access_Cap   =>
                 Rose.System_Calls.Server.Create_Endpoint
                   (Create_Endpoint_Cap,
                    Rose.Interfaces.Bank.Bank_Interface,
                    Rose.Objects.Capability_Identifier (Index)),
               Page_Limit   => Page_Limit,
               Region_Limit => Region_Limit,
               others       => <>);
            return Banks (Index).Access_Cap;
         end if;
      end loop;

      Rose.Console_IO.Put_Line ("store: out of banks");
      return 0;
   end Create_Sub_Bank;

   ----------------------
   -- Create_Subregion --
   ----------------------

   function Create_Subregion
     (Id      : in Rose.Objects.Capability_Identifier;
      Base    : Rose.Objects.Object_Id;
      Bound   : Rose.Objects.Object_Id;
      Flags   : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Flags);
      use Rose.Objects;
      Parent : Region_Record renames Regions (Positive (Id));
   begin

      if Natural (Id) > Region_Count then
         Rose.Console_IO.Put ("storage: id ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.Put (" too large (max is ");
         Rose.Console_IO.Put (Region_Count);
         Rose.Console_IO.Put (")");
         Rose.Console_IO.New_Line;
      end if;


      if Base < Parent.Base then
         Rose.Console_IO.Put_Line ("storage: invalid request: base too small");
         Rose.Console_IO.Put ("storage: requested base: ");
         Rose.Console_IO.Put (Base);
         Rose.Console_IO.New_Line;
         Rose.Console_IO.Put ("storage: parent base: ");
         Rose.Console_IO.Put (Parent.Base);
         Rose.Console_IO.New_Line;
      end if;

      if Base >= Parent.Bound then
         Rose.Console_IO.Put_Line ("storage: invalid request: base too large");
         Rose.Console_IO.Put ("storage: requested base: ");
         Rose.Console_IO.Put (Base);
         Rose.Console_IO.New_Line;
         Rose.Console_IO.Put ("storage: parent bound: ");
         Rose.Console_IO.Put (Parent.Bound);
         Rose.Console_IO.New_Line;
      end if;

      if Bound <= Parent.Base then
         Rose.Console_IO.Put_Line
           ("storage: invalid request: bound too small");
         Rose.Console_IO.Put ("storage: requested bound: ");
         Rose.Console_IO.Put (Bound);
         Rose.Console_IO.New_Line;
         Rose.Console_IO.Put ("storage: parent base: ");
         Rose.Console_IO.Put (Parent.Base);
         Rose.Console_IO.New_Line;
      end if;

      if Bound > Parent.Bound then
         Rose.Console_IO.Put_Line
           ("storage: invalid request: bound too large");
         Rose.Console_IO.Put ("storage: requested bound: ");
         Rose.Console_IO.Put (Bound);
         Rose.Console_IO.New_Line;
         Rose.Console_IO.Put ("storage: parent bound: ");
         Rose.Console_IO.Put (Parent.Bound);
         Rose.Console_IO.New_Line;
      end if;

      if Base > Bound then
         Rose.Console_IO.Put_Line ("storage: invalid request: base > bound");
         Rose.Console_IO.Put ("storage: requested base: ");
         Rose.Console_IO.Put (Base);
         Rose.Console_IO.New_Line;
         Rose.Console_IO.Put ("storage: requested bound: ");
         Rose.Console_IO.Put (Bound);
         Rose.Console_IO.New_Line;
      end if;

      if not Within_Limits (Parent.Bank, 0, 1) then
         Rose.Console_IO.Put_Line ("storage: bank region limit reached");
         return 0;
      end if;

      declare
         Index : constant Natural := New_Region;
      begin
         if Index = 0 then
            Rose.Console_IO.Put ("storage: out of regions (max is ");
            Rose.Console_IO.Put (Natural'(Max_Regions));
            Rose.Console_IO.Put (")");
            Rose.Console_IO.New_Line;
            return 0;
         end if;

         declare
            Child : Region_Record renames Regions (Index);
         begin
            Child := (Parent with delta
                        Base => Base, Bound => Bound,
                        Mapped_Blocks => 0,
                        Reads => 0, Zero_Reads => 0, Writes => 0,
                        Access_Cap =>
                          Rose.System_Calls.Server.Create_Endpoint
                            (Create_Endpoint_Cap,
                             Rose.Interfaces.Region.Region_Interface,
                             Capability_Identifier (Index)));

            Charge (Child.Bank, 0, 1);

            return Child.Access_Cap;
         end;
      end;

   end Create_Subregion;

   ------------------
   -- Destroy_Bank --
   ------------------

   procedure Destroy_Bank
     (Id : Rose.Objects.Capability_Identifier)
   is
      Bank   : constant Natural := Natural (Id);
      Doomed : array (1 .. Max_Banks) of Boolean := (others => False);
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Bank not in Root_Bank + 1 .. Max_Banks
        or else not Banks (Bank).In_Use
      then
         return;
      end if;

      --  Decide which banks go first: a bank's parent link is gone once
      --  the bank is.

      for Index in Doomed'Range loop
         Doomed (Index) :=
           Banks (Index).In_Use and then Is_Within (Index, Bank);
      end loop;

      for Index in 1 .. Region_Count loop
         if Regions (Index).In_Use
           and then Regions (Index).Bank in Doomed'Range
           and then Doomed (Regions (Index).Bank)
         then
            Release (Rose.Objects.Capability_Identifier (Index));
         end if;
      end loop;

      for Index in Doomed'Range loop
         if Doomed (Index) then
            Rose.System_Calls.Initialize_Send
              (Params, Rose.System_Calls.Delete_Cap_Capability);
            Rose.System_Calls.Send_Cap (Params, Banks (Index).Access_Cap);
            Rose.System_Calls.Invoke_Capability (Params);

            Rose.Interfaces.Bank.Server.Remove_Instance
              (Rose.Objects.Capability_Identifier (Index));
            Banks (Index) := (others => <>);
         end if;
      end loop;
   end Destroy_Bank;

   -----------------
   -- Device_Page --
   -----------------

   function Device_Page
     (Entry_Index : Positive;
      Page        : Rose.Objects.Object_Id;
      Store       : out Positive)
      return Rose.Interfaces.Block_Device.Block_Address_Type
   is
      Block : constant Natural := Natural (Map (Entry_Index).Block);
   begin
      Store := 1;
      while Backing_Stores (Store).Allocator_Bound <= Block loop
         Store := Store + 1;
      end loop;

      return Rose.Interfaces.Block_Device.Block_Address_Type
        ((Block - Backing_Stores (Store).Allocator_Base)
         * Minimum_Block_Pages
         + Natural (Page mod Minimum_Block_Pages));
   end Device_Page;

   ----------------
   -- Find_Entry --
   ----------------

   function Find_Entry
     (Page : Rose.Objects.Object_Id)
      return Natural
   is
      Key   : constant Rose.Objects.Object_Id :=
                Page - Page mod Minimum_Block_Pages;
      Index : Rose.Words.Word_32 :=
                Buckets (Natural ((Key / Minimum_Block_Pages)
                                  mod Max_Mapped_Blocks));
   begin
      while Index /= 0 loop
         if Map (Natural (Index)).Page = Key then
            return Natural (Index);
         end if;

         Index := Map (Natural (Index)).Next;
      end loop;

      return 0;
   end Find_Entry;

   -----------------
   -- Find_Region --
   -----------------

   function Find_Region
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : Rose.Objects.Object_Id;
      Bound : Rose.Objects.Object_Id)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      for Region of Regions (1 .. Region_Count) loop
         if Region.In_Use
           and then Region.Base = Base
           and then Region.Bound = Bound
         then
            return Region.Access_Cap;
         end if;
      end loop;

      Rose.Console_IO.Put ("storage: no region for pages ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Base));
      Rose.Console_IO.New_Line;
      return Rose.Capabilities.Null_Capability;
   end Find_Region;

   ---------
   -- Get --
   ---------

   procedure Get
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data :    out System.Storage_Elements.Storage_Array)
   is
      Region_Index : constant Natural := Natural (Id);
   begin
      if Region_Index not in 1 .. Region_Count
        or else Page < Regions (Region_Index).Base
        or else Page >= Regions (Region_Index).Bound
      then
         Rose.Console_IO.Put ("store: get: invalid region/page: ");
         Rose.Console_IO.Put (Region_Index);
         Rose.Console_IO.Put ("/");
         Rose.Console_IO.Put (Page);
         Rose.Console_IO.New_Line;
         Data := (others => 0);
         return;
      end if;

      declare
         Region      : Region_Record renames Regions (Region_Index);
         Entry_Index : constant Natural := Find_Entry (Page);
         Start       : constant Natural := Rose.Trace.Ticks;
         Store       : Positive;
         Address     : Rose.Interfaces.Block_Device.Block_Address_Type;
      begin
         if Entry_Index = 0 or else Is_Fresh (Page) then
            Data := (others => 0);
            Region.Zero_Reads := Region.Zero_Reads + 1;
            return;
         end if;

         Address := Device_Page (Entry_Index, Page, Store);

         Rose.Interfaces.Block_Device.Client.Read_Blocks
           (Item   => Backing_Stores (Store).Client,
            Start  => Address,
            Count  => 1,
            Blocks => Data);
         Backing_Stores (Store).Reads := Backing_Stores (Store).Reads + 1;
         Backing_Stores (Store).Read_Ticks :=
           Backing_Stores (Store).Read_Ticks + (Rose.Trace.Ticks - Start);
         Region.Reads := Region.Reads + 1;
      end;

   end Get;

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : out Rose.Objects.Object_Id;
      Bound : out Rose.Objects.Object_Id)
   is
      Region_Index : constant Natural := Natural (Id);
   begin
      if Region_Index not in 1 .. Region_Count then
         Rose.Console_IO.Put ("store: get-range: no such region: ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.New_Line;
         Base := 0;
         Bound := 0;
         return;
      end if;

      Base := Regions (Region_Index).Base;
      Bound := Regions (Region_Index).Bound;

   end Get_Range;

   -------------------
   -- Get_Root_Bank --
   -------------------

   function Get_Root_Bank
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      Create_Root_Bank;
      return Banks (Root_Bank).Access_Cap;
   end Get_Root_Bank;

   ---------------
   -- Get_Usage --
   ---------------

   procedure Get_Usage
     (Id           : in     Rose.Objects.Capability_Identifier;
      Pages        :    out Rose.Words.Word_64;
      Regions      :    out Rose.Words.Word;
      Page_Limit   :    out Rose.Words.Word_64;
      Region_Limit :    out Rose.Words.Word)
   is
      Bank : constant Natural := Natural (Id);
   begin
      Pages := 0;
      Regions := 0;
      Page_Limit := 0;
      Region_Limit := 0;

      if Bank in 1 .. Max_Banks and then Banks (Bank).In_Use then
         Pages := Banks (Bank).Pages;
         Regions := Banks (Bank).Regions;
         Page_Limit := Banks (Bank).Page_Limit;
         Region_Limit := Banks (Bank).Region_Limit;
      end if;
   end Get_Usage;

   ------------------
   -- Insert_Entry --
   ------------------

   function Insert_Entry
     (Page  : Rose.Objects.Object_Id;
      Block : Rose.Words.Word_32;
      Fresh : Rose.Words.Word_32)
      return Natural
   is
      Key    : constant Rose.Objects.Object_Id :=
                 Page - Page mod Minimum_Block_Pages;
      Bucket : constant Natural :=
                 Natural ((Key / Minimum_Block_Pages) mod Max_Mapped_Blocks);
      Index  : Natural;
   begin
      if Free_Entries /= 0 then
         Index := Natural (Free_Entries);
         Free_Entries := Map (Index).Next;
      elsif Map_High < Max_Mapped_Blocks then
         Map_High := Map_High + 1;
         Index := Map_High;
      else
         return 0;
      end if;

      Map (Index) :=
        (Page  => Key,
         Next  => Buckets (Bucket),
         Block => Block,
         Fresh => Fresh);
      Buckets (Bucket) := Rose.Words.Word_32 (Index);
      Map_Count := Map_Count + 1;
      return Index;
   end Insert_Entry;

   --------------
   -- Is_Fresh --
   --------------

   function Is_Fresh
     (Page : Rose.Objects.Object_Id)
      return Boolean
   is
      Index : constant Natural := Find_Entry (Page);
   begin
      return Index = 0
        or else (Map (Index).Fresh
                 and Page_Bit (Page)) /= 0;
   end Is_Fresh;

   ---------------
   -- Is_Within --
   ---------------

   function Is_Within
     (Bank     : Natural;
      Ancestor : Positive)
      return Boolean
   is
      Index : Natural := Bank;
   begin
      while Index in 1 .. Max_Banks loop
         if Index = Ancestor then
            return True;
         end if;
         Index := Banks (Index).Parent;
      end loop;

      return False;
   end Is_Within;

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero
     (Data : System.Storage_Elements.Storage_Array)
      return Boolean
   is
      use type System.Storage_Elements.Storage_Element;
   begin
      for Item of Data loop
         if Item /= 0 then
            return False;
         end if;
      end loop;

      return True;
   end Is_Zero;

   ---------------
   -- Map_Block --
   ---------------

   function Map_Block
     (Root : in out Region_Record;
      Page : Rose.Objects.Object_Id)
      return Natural
   is
      Block       : Natural;
      Entry_Index : Natural;
   begin
      if not Within_Limits (Root.Bank, Minimum_Block_Pages, 0) then
         Rose.Console_IO.Put_Line ("store: bank limit reached");
         return 0;
      end if;

      Block := Space_Allocators.Allocate (Allocator, 1);

      if Block = 0 then
         Rose.Console_IO.Put_Line ("store: out of space");
         return 0;
      end if;

      Entry_Index :=
        Insert_Entry
          (Page, Rose.Words.Word_32 (Block),
           Rose.Words.Word_32'(2) ** Minimum_Block_Pages - 1);

      if Entry_Index = 0 then
         Space_Allocators.Deallocate (Allocator, Block, Block + 1);
         Rose.Console_IO.Put_Line ("store: out of space for the block map");
         return 0;
      end if;

      Charge (Root.Bank, Minimum_Block_Pages, 0);
      Root.Mapped_Blocks := Root.Mapped_Blocks + 1;
      return Entry_Index;
   end Map_Block;

   ----------------
   -- New_Region --
   ----------------

   function New_Region return Natural is
   begin
      for Index in 1 .. Region_Count loop
         if not Regions (Index).In_Use then
            Regions (Index).In_Use := True;
            return Index;
         end if;
      end loop;

      if Region_Count < Max_Regions then
         Region_Count := Region_Count + 1;
         Regions (Region_Count).In_Use := True;
         return Region_Count;
      end if;

      return 0;
   end New_Region;

   ---------------
   -- New_Store --
   ---------------

   procedure New_Store
     (Client     : Rose.Interfaces.Block_Device.Client.Block_Device_Client;
      Page_Count : Rose.Words.Word;
      Base       : out Rose.Objects.Object_Id;
      Bound      : out Rose.Objects.Object_Id)
   is
      use Rose.Objects;
      Index : Natural := 0;
      Found : Boolean := False;
      Request_Page_Count : Object_Id := 1;
   begin
      while Request_Page_Count < Object_Id (Page_Count) loop
         Request_Page_Count := Request_Page_Count * 2;
      end loop;

      if Backing_Store_Count = 0 then
         Backing_Store_Count := 1;
         Backing_Stores (1) := Backing_Store_Record'
           (Allocated       => False,
            Client          => <>,
            Base            => Rose.Objects.Page_Object_Id'First,
            Bound           => Rose.Objects.Page_Object_Id'Last,
            Device_Bound    => Rose.Objects.Page_Object_Id'First,
            Allocator_Base  => 1,
            Allocator_Bound => Positive'Last,
            others          => <>);
      end if;

      while Index < Backing_Store_Count loop
         Index := Index + 1;
         declare
            Store : Backing_Store_Record renames
                      Backing_Stores (Index);
         begin
            if not Store.Allocated
              and then Store.Bound - Store.Base >= Request_Page_Count
            then
               Found := True;
               exit;
            end if;
         end;
      end loop;

      if not Found then
         Rose.Console_IO.Put ("no room for new backing store of size ");
         Rose.Console_IO.Put (Natural (Request_Page_Count) / 1024 / 1024);
         Rose.Console_IO.New_Line;
         Base := 0;
         Bound := 0;

         return;
      end if;

      declare
         Store : Backing_Store_Record renames
                   Backing_Stores (Index);
      begin
         if Backing_Store_Count < Max_Backing_Stores
           and then Store.Bound - Store.Base > Request_Page_Count
         then
            Backing_Store_Count := Backing_Store_Count + 1;
            declare
               New_Free_Store : Backing_Store_Record renames
                                  Backing_Stores (Backing_Store_Count);
            begin
               New_Free_Store.Base := Store.Base + Request_Page_Count;
               New_Free_Store.Allocator_Base :=
                 Store.Allocator_Base
                   + Natural (Request_Page_Count) / Minimum_Block_Pages;
               New_Free_Store.Bound := Store.Bound;
               New_Free_Store.Allocated := False;
               Store.Allocator_Bound := New_Free_Store.Allocator_Base;
            end;
         end if;

         Store.Bound := Store.Base + Request_Page_Count;
         Store.Device_Bound := Store.Base + Object_Id (Page_Count);
         Store.Client := Client;
         Store.Allocated := True;

         Base := Store.Base;
         Bound := Store.Bound;

         Rose.Console_IO.Put ("new store: [");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Base));
         Rose.Console_IO.Put (", ");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Store.Bound));
         Rose.Console_IO.Put (")");
         Rose.Console_IO.New_Line;
      end;

   end New_Store;

   ---------
   -- Put --
   ---------

   procedure Put
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data : in     System.Storage_Elements.Storage_Array)
   is
      Region_Index : constant Natural := Natural (Id);
   begin
      if Region_Index not in 1 .. Region_Count
        or else Page < Regions (Region_Index).Base
        or else Page >= Regions (Region_Index).Bound
      then
         return;
      end if;

      if Regions (Region_Index).Root not in 1 .. Region_Count
        or else not Regions (Regions (Region_Index).Root).In_Use
      then
         return;
      end if;

      declare
         Region      : Region_Record renames Regions (Region_Index);
         Root        : Region_Record renames Regions (Region.Root);
         Entry_Index : Natural := Find_Entry (Page);
         Start       : constant Natural := Rose.Trace.Ticks;
         Store       : Positive;
         Address     : Rose.Interfaces.Block_Device.Block_Address_Type;
      begin
         if Entry_Index = 0 then

            --  A page of zeroes is what an unwritten page reads as.

            if Is_Zero (Data) then
               return;
            end if;

            Entry_Index := Map_Block (Root, Page);
            if Entry_Index = 0 then
               return;
            end if;
         end if;

         Address := Device_Page (Entry_Index, Page, Store);

         Rose.Interfaces.Block_Device.Client.Write_Blocks
           (Item   => Backing_Stores (Store).Client,
            Start  => Address,
            Count  => 1,
            Blocks => Data);
         Map (Entry_Index).Fresh :=
           Map (Entry_Index).Fresh and not Page_Bit (Page);
         Backing_Stores (Store).Writes := Backing_Stores (Store).Writes + 1;
         Backing_Stores (Store).Write_Ticks :=
           Backing_Stores (Store).Write_Ticks + (Rose.Trace.Ticks - Start);
         Region.Writes := Region.Writes + 1;
      end;

   end Put;

   ----------
   -- Read --
   ----------

   procedure Read
     (Id      : in Rose.Objects.Capability_Identifier;
      Storage : out System.Storage_Elements.Storage_Array;
      Last    : out System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
      Rec  : Region_Stream_Record renames
               Region_Streams (Id);
   begin
      if not Rec.Open
        or else Rec.Current_Offset >= Rec.Stream_Length
      then
         Rose.System_Calls.Server.Rescind_Cap (Rec.Cap);
         Rec.Open := False;
         Last := 0;
         return;
      end if;

      Last := Storage'First - 1;
      while Last < Storage'Last
        and then Rec.Current_Offset <= Rec.Stream_Length
      loop
         Last := Last + 1;
         if Rec.Buffer_Offset > Rec.Buffer_Length then
            Rec.Current_Page := Rec.Current_Page + 1;
            Get (Rose.Objects.Capability_Identifier (Rec.Region),
                 Rec.Current_Page, Rec.Current_Buffer);
            Rec.Buffer_Offset := 1;
         end if;

         Storage (Last) := Rec.Current_Buffer (Rec.Buffer_Offset);
         Rec.Buffer_Offset := Rec.Buffer_Offset + 1;
         Rec.Current_Offset := Rec.Current_Offset + 1;
      end loop;

   end Read;

   -----------------
   -- Read_Stream --
   -----------------

   function Read_Stream
     (Id      : in Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use Rose.Objects;
      Stream_Id : Capability_Identifier := 0;
   begin
      for I in Region_Streams'Range loop
         if not Region_Streams (I).Open then
            Stream_Id := I;
            exit;
         end if;
      end loop;

      if Stream_Id = 0 then
         return 0;
      end if;

      declare
         use type System.Storage_Elements.Storage_Offset;
         use Rose.Capabilities;
         Current_Cap : constant Capability :=
                         Region_Streams (Stream_Id).Cap;
         Stream_Rec  : Region_Stream_Record renames
                         Region_Streams (Stream_Id);
         Region  : Region_Record renames
                         Regions (Positive (Id));
      begin
         Stream_Rec :=
           Region_Stream_Record'
             (Open           => True,
              Cap            => Current_Cap,
              Region     => Positive (Id),
              Current_Page   => Region.Base,
              Stream_Length  =>
                System.Storage_Elements.Storage_Count
                  (Region.Bound - Region.Base)
                    * Rose.Limits.Page_Size,
              Current_Offset => 0,
              Buffer_Offset  => 0,
              Current_Buffer => (others => 0),
              Buffer_Length  => 0);
         if Stream_Rec.Cap = 0 then
            Stream_Rec.Cap :=
              Rose.System_Calls.Server.Create_Endpoint
                (1, Rose.Interfaces.Stream_Reader.Stream_Reader_Interface,
                 Stream_Id);
         end if;

         return Stream_Rec.Cap;
      end;

   end Read_Stream;

   -------------
   -- Release --
   -------------

   procedure Release
     (Id : Rose.Objects.Capability_Identifier)
   is
      Index : constant Natural := Natural (Id);
   begin
      if Index not in 1 .. Region_Count
        or else not Regions (Index).In_Use
      then
         return;
      end if;

      declare
         Region : Region_Record renames Regions (Index);
         Params : aliased Rose.Invocation.Invocation_Record;
         Freed  : Rose.Words.Word_64 := 0;
      begin
         Rose.Trace.Event
           ("store: region released: reads ", Region.Reads,
            ", writes ", Region.Writes);
         Rose.Trace.Event
           ("store: region released: zero reads ", Region.Zero_Reads);
         for Store of Backing_Stores (1 .. Backing_Store_Count) loop
            Rose.Trace.Event
              ("store: total reads ", Store.Reads,
               ", ticks ", Store.Read_Ticks);
            Rose.Trace.Event
              ("store: total writes ", Store.Writes,
               ", ticks ", Store.Write_Ticks);
         end loop;

         --  A root region's blocks that have space on a device give it back.

         if Region.Root = Index and then Region.Mapped_Blocks > 0 then
            for Entry_Index in 1 .. Map_High loop
               if Map (Entry_Index).Page >= Region.Base
                 and then Map (Entry_Index).Page < Region.Bound
               then
                  Space_Allocators.Deallocate
                    (Allocator,
                     Natural (Map (Entry_Index).Block),
                     Natural (Map (Entry_Index).Block) + 1);
                  Remove_Entry (Entry_Index);
                  Freed := Freed + 1;
               end if;
            end loop;
         end if;

         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Region.Access_Cap);
         Rose.System_Calls.Invoke_Capability (Params);

         Uncharge (Region.Bank, Freed * Minimum_Block_Pages, 1);

         Rose.Interfaces.Region.Server.Remove_Instance (Id);
         Region := (others => <>);
      end;
   end Release;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry (Index : Positive) is
      Bucket : constant Natural :=
                 Natural ((Map (Index).Page / Minimum_Block_Pages)
                          mod Max_Mapped_Blocks);
      This   : constant Rose.Words.Word_32 := Rose.Words.Word_32 (Index);
      Link   : Rose.Words.Word_32 := Buckets (Bucket);
   begin
      if Link = This then
         Buckets (Bucket) := Map (Index).Next;
      else
         while Link /= 0 and then Map (Natural (Link)).Next /= This loop
            Link := Map (Natural (Link)).Next;
         end loop;

         if Link /= 0 then
            Map (Natural (Link)).Next := Map (Index).Next;
         end if;
      end if;

      Map (Index) := (Page => 0, Next => Free_Entries, others => 0);
      Free_Entries := This;
      Map_Count := Map_Count - 1;
   end Remove_Entry;

   ------------------
   -- Reserve_From --
   ------------------

   function Reserve_From
     (Bank : Positive;
      Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      Blocks       : constant Rose.Words.Word_64 :=
                       (if Size = 0 then 1
                        else (Size + Minimum_Block_Size - 1)
                          / Minimum_Block_Size);
      Pages        : constant Rose.Words.Word_64 :=
                       Blocks * Minimum_Block_Pages;
      Region_Index : Natural;
   begin

      --  The region's pages are the next page ids.  Nothing is allocated
      --  on a device until a page is written (#305), so a reservation is
      --  limited by the bank's region limit, and by the page ids, but not
      --  by how much space the devices have.

      if not Within_Limits (Bank, 0, 1) then
         Rose.Console_IO.Put_Line ("store: bank limit reached");
         return 0;
      end if;

      if Pages > Rose.Words.Word_64 (Rose.Objects.Page_Object_Id'Last
                                     - Next_Page)
      then
         Rose.Console_IO.Put_Line ("store: out of page ids");
         return 0;
      end if;

      Region_Index := New_Region;

      if Region_Index = 0 then
         Rose.Console_IO.Put_Line ("store: out of regions");
         return 0;
      end if;

      declare
         use Rose.Objects;
         Reserved : Region_Record renames
                      Regions (Region_Index);
         Base     : constant Object_Id := Next_Page;
         Cap      : constant Rose.Capabilities.Capability :=
              Rose.System_Calls.Server.Create_Endpoint
                (Create_Endpoint_Cap,
                 Rose.Interfaces.Region.Region_Interface,
                 Capability_Identifier (Region_Index));

      begin
         Next_Page := Base + Object_Id (Pages);

         Reserved := Region_Record'
           (In_Use         => True,
            Base           => Base,
            Bound          => Next_Page,
            Access_Cap     => Cap,
            Root           => Region_Index,
            Mapped_Blocks  => 0,
            Reads          => 0,
            Zero_Reads     => 0,
            Writes         => 0,
            Bank           => Bank);

         Charge (Bank, 0, 1);

         return Cap;
      end;
   end Reserve_From;

   -----------------------
   -- Reserve_From_Bank --
   -----------------------

   function Reserve_From_Bank
     (Id   : Rose.Objects.Capability_Identifier;
      Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      Bank : constant Natural := Natural (Id);
   begin
      if Bank not in 1 .. Max_Banks or else not Banks (Bank).In_Use then
         return 0;
      end if;

      return Reserve_From (Bank, Size);
   end Reserve_From_Bank;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
   begin
      Create_Root_Bank;
      return Reserve_From (Root_Bank, Size);
   end Reserve_Storage;

   -------------------
   -- Restore_Entry --
   -------------------

   procedure Restore_Entry
     (Page  : Rose.Objects.Object_Id;
      Block : Rose.Words.Word_32;
      Fresh : Rose.Words.Word_32)
   is
      Owner       : Natural := 0;
      Entry_Index : Natural;
   begin
      for Index in 1 .. Region_Count loop
         if Regions (Index).In_Use
           and then Regions (Index).Root = Index
           and then Page >= Regions (Index).Base
           and then Page < Regions (Index).Bound
         then
            Owner := Index;
            exit;
         end if;
      end loop;

      if Owner = 0 then
         Rose.Console_IO.Put ("storage: restore: no region for page ");
         Rose.Console_IO.Put (Page);
         Rose.Console_IO.New_Line;
         return;
      end if;

      if Natural (Block) = 0
        or else not Space_Allocators.Allocate_At
          (Allocator, Natural (Block), 1)
      then
         Rose.Console_IO.Put ("storage: restore: space for page ");
         Rose.Console_IO.Put (Page);
         Rose.Console_IO.Put_Line (" is not free");
         return;
      end if;

      Entry_Index := Insert_Entry (Page, Block, Fresh);

      if Entry_Index = 0 then
         Space_Allocators.Deallocate
           (Allocator, Natural (Block), Natural (Block) + 1);
         Rose.Console_IO.Put_Line ("storage: restore: the block map is full");
         return;
      end if;

      --  Banks aren't saved yet (ADR 0014), so the blocks are charged to
      --  the root bank, like the regions.

      Charge (Root_Bank, Minimum_Block_Pages, 0);
      Regions (Owner).Mapped_Blocks := Regions (Owner).Mapped_Blocks + 1;
   end Restore_Entry;

   --------------------
   -- Restore_Region --
   --------------------

   procedure Restore_Region
     (Id          : Rose.Objects.Capability_Identifier;
      Index       : Natural;
      Base        : Rose.Objects.Object_Id;
      Bound       : Rose.Objects.Object_Id;
      Root        : Natural)
   is
      pragma Unreferenced (Id);
      use Rose.Objects;
   begin
      Create_Root_Bank;

      if Index not in Regions'Range or else Regions (Index).In_Use then
         Rose.Console_IO.Put ("storage: restore: region ");
         Rose.Console_IO.Put (Index);
         Rose.Console_IO.Put_Line (" is not free");
         return;
      end if;

      Regions (Index) :=
        Region_Record'
          (In_Use         => True,
           Base           => Base,
           Bound          => Bound,
           Access_Cap     =>
             Rose.System_Calls.Server.Create_Endpoint
               (Create_Endpoint_Cap,
                Rose.Interfaces.Region.Region_Interface,
                Capability_Identifier (Index)),
           Root           => (if Root in Regions'Range then Root else Index),
           Mapped_Blocks  => 0,
           Reads          => 0,
           Zero_Reads     => 0,
           Writes         => 0,
           Bank           => Root_Bank);

      --  Banks aren't saved yet (ADR 0014), so every region is charged to
      --  the root bank.

      Charge (Root_Bank, 0, 1);

      Region_Count := Natural'Max (Region_Count, Index);
   end Restore_Region;

   ------------------------
   -- Restore_Table_Page --
   ------------------------

   procedure Restore_Table_Page
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Natural;
      Page  : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      use Rose.Checkpoints;
   begin
      if Page'Length /= Rose.Limits.Page_Size then
         Rose.Console_IO.Put ("storage: restore: table page ");
         Rose.Console_IO.Put (Index);
         Rose.Console_IO.Put_Line (" is not a page");
         return;
      end if;

      if Index = 1 then
         declare
            Header : Store_Table_Header
              with Import, Address => Page'Address;
         begin
            Restore_Region_Count := 0;
            Restore_Last_Region := 0;
            Restore_First_Map := 0;
            Restore_Last_Map := 0;
            Restore_Entry_Count := 0;

            if Header.Magic = Store_Table_Magic
              and then Header.Version = Store_Table_Version
              and then Natural (Header.Region_Count) <= Max_Regions
            then
               Next_Page :=
                 Rose.Objects.Object_Id'Max
                   (Next_Page, Rose.Objects.Object_Id (Header.Next_Page));
               Restore_Region_Count := Natural (Header.Region_Count);
               Restore_Last_Region :=
                 1 + (Restore_Region_Count + Regions_Per_Page - 1)
                 / Regions_Per_Page;
               Restore_First_Map := Restore_Last_Region + 1;
               Restore_Last_Map :=
                 Restore_Last_Region + Natural (Header.Map_Page_Count);
               Restore_Entry_Count := Natural (Header.Map_Entry_Count);
            end if;
         end;

      elsif Index in Restore_First_Map .. Restore_Last_Map then
         declare
            Table : Map_Page
              with Import, Address => Page'Address;
            Last  : constant Natural :=
                      Natural'Min (Restore_Entry_Count, Entries_Per_Page);
         begin
            for I in 1 .. Last loop
               Restore_Entry
                 (Page  => Rose.Objects.Object_Id (Table.Entries (I).Page),
                  Block => Table.Entries (I).Block,
                  Fresh => Table.Entries (I).Fresh);
            end loop;

            Restore_Entry_Count := Restore_Entry_Count - Last;

            if Index = Restore_Last_Map then
               Rose.Console_IO.Put ("storage: restore: blocks: ");
               Rose.Console_IO.Put (Map_Count);
               Rose.Console_IO.New_Line;
            end if;
         end;
      end if;
   end Restore_Table_Page;

   --------------
   -- Uncharge --
   --------------

   procedure Uncharge
     (Bank    : Natural;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word)
   is
      Index : Natural := Bank;
   begin
      while Index in 1 .. Max_Banks loop
         Banks (Index).Pages :=
           (if Banks (Index).Pages > Pages
            then Banks (Index).Pages - Pages else 0);
         Banks (Index).Regions :=
           (if Banks (Index).Regions > Regions
            then Banks (Index).Regions - Regions else 0);
         Index := Banks (Index).Parent;
      end loop;
   end Uncharge;

   -------------------
   -- Within_Limits --
   -------------------

   function Within_Limits
     (Bank    : Positive;
      Pages   : Rose.Words.Word_64;
      Regions : Rose.Words.Word)
      return Boolean
   is
      Index : Natural := Bank;
   begin
      while Index in 1 .. Max_Banks loop
         declare
            This : Bank_Record renames Banks (Index);
         begin
            if (This.Page_Limit /= 0
                and then This.Pages + Pages > This.Page_Limit)
              or else
                (This.Region_Limit /= 0
                 and then This.Regions + Regions > This.Region_Limit)
            then
               return False;
            end if;

            Index := This.Parent;
         end;
      end loop;

      return True;
   end Within_Limits;

end Store.Devices;
