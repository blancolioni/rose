with Rose.Checkpoints;
with Rose.Limits;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Server;
with Rose.Trace;
with Rose.System_Calls.Server;

with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Storage.Bank.Server;
with Rose.Interfaces.Storage.Dependent;
with Rose.Interfaces.Storage.Dependent.Server;
with Rose.Interfaces.Storage.Block_Array.Client;
with Rose.Interfaces.Storage.Region;
with Rose.Interfaces.Storage.Region.Server;
with Rose.Interfaces.Streams.Reader;

with Store.Device_Pages;
with Store.Page_Bitmaps;
with Store.Page_Trees;

package body Store.Devices is

   use type Rose.Objects.Object_Id;
   use type Rose.Words.Word_8;
   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;

   Max_Backing_Stores  : constant := 16;
   Max_Regions         : constant := Rose.Checkpoints.Max_Store_Slots;
   Max_Banks           : constant := Rose.Checkpoints.Max_Store_Slots;
   --  A bank for each process (ADR 0015, #750), user and session (#450).
   --  A slot fits in the low byte of a cap's identifier (#967).
   Max_Open_Streams    : constant := 10;

   Root_Bank : constant := 1;
   --  The bank that every other is below.

   Max_Store_Pages : constant := 2 ** 20;
   --  The device pages of all the backing stores together: 4 GiB (ADR
   --  0038, section 11).

   Tree_Cache_Pages : constant := 64;
   --  The pages of the block map's tree kept in memory (section 7).

   Tree_Reserve : constant := Tree_Cache_Pages + 8;
   --  A write that needs a new device page is refused when no more than
   --  this many are free, so that a checkpoint can always write the tree
   --  pages in the cache, and the path to each.

   Next_Page : Rose.Objects.Object_Id := Rose.Objects.Page_Object_Id'First;
   --  The first page id that no region has had.  A region's pages are
   --  the next ones, which are never used again, so a page id is
   --  persistent whatever happens to the region (#305).

   use Store.Device_Pages;

   --  A page of a region has space on a device from its first write
   --  (#305).  The block map, a tree on the backing stores, says where,
   --  and the free map says which device pages are in use (ADR 0038).
   --  Their tables are in the store's bss, which the store writes when it
   --  starts, so every page of them is mapped before it is used: a page
   --  fault in the store while the memory server is waiting for it would
   --  deadlock them both.

   package Space is
     new Store.Page_Bitmaps (Max_Pages => Max_Store_Pages);

   procedure Read_Tree_Page
     (Page : Device_Page;
      Data : out Page_Words);

   procedure Write_Tree_Page
     (Page : Device_Page;
      Data : Page_Words);

   package Tree is
     new Store.Page_Trees
       (Cache_Size => Tree_Cache_Pages,
        Read       => Read_Tree_Page,
        Write      => Write_Tree_Page,
        Allocate   => Space.Allocate,
        Free       => Space.Free,
        Is_Born    => Space.Is_Born);

   function Key_Of
     (Page : Rose.Objects.Object_Id)
      return Tree.Key_Type
   is (Tree.Key_Type (Page - Rose.Objects.Page_Object_Id'First));
   --  The block map's key for a page id.

   type Backing_Store_Record is
      record
         Client          : Rose.Interfaces.Storage.Block_Array.Client
           .Block_Array_Client;
         First           : Device_Page := No_Page;
         Count           : Rose.Words.Word_32 := 0;
         Image_Pages     : Natural := 0;
         Reads           : Natural := 0;
         Read_Ticks      : Natural := 0;
         Writes          : Natural := 0;
         Write_Ticks     : Natural := 0;
         --  Pages read and written, and the time they took (#196).
      end record;
   --  A backing store holds the device pages First .. First + Count - 1,
   --  its block I being device page First + I.  First is a multiple of a
   --  page of the free map's image, and its first 2 * Image_Pages blocks
   --  hold the two copies of its part of the image (ADR 0038, section 4):
   --  copy C's page J is block C * Image_Pages + J.

   type Backing_Store_Array is
     array (1 .. Max_Backing_Stores) of Backing_Store_Record;

   Backing_Stores      : Backing_Store_Array;
   Backing_Store_Count : Natural := 0;

   Next_First : Device_Page := 0;
   --  The device page that the next backing store starts at.

   Device_Buffer : System.Storage_Elements.Storage_Array
                     (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   Image_Buffer  : Page_Words
     with Import, Address => Device_Buffer'Address;
   --  A page of the free map's image on its way to or from a device.

   function Store_Of (Page : Device_Page) return Natural;
   --  The backing store that holds Page, or zero.

   procedure Read_Device
     (Page : Device_Page;
      Data : out System.Storage_Elements.Storage_Array);

   procedure Write_Device
     (Page : Device_Page;
      Data : System.Storage_Elements.Storage_Array);

   procedure Sync_Devices;
   --  Return when everything written to the backing stores is on the
   --  medium, and not only in a disk's write cache (#505).

   procedure Write_Images;
   --  Write the pages of the free map's image that the copy being
   --  written doesn't have yet.

   procedure Restore_Space
     (Root   : Device_Page;
      Height : Positive;
      Copy   : Space.Copy_Index);
   --  Make the free map what copy Copy of its image says, and the block
   --  map the tree whose root is Root.

   procedure Add_Space (Store : Positive);
   --  Make the pages of a backing store that don't hold the free map's
   --  image free.

   type Region_Record is
      record
         In_Use         : Boolean := False;
         Base, Bound    : Rose.Objects.Object_Id := 0;
         Access_Cap     : Rose.Capabilities.Capability := 0;
         Root           : Natural := 0;
         Mapped_Pages   : Natural := 0;
         Reads          : Natural := 0;
         Zero_Reads     : Natural := 0;
         Writes         : Natural := 0;
         Bank           : Natural := 0;
      end record;
   --  A region.  Root is the region that its page ids were reserved for,
   --  which is the region itself unless it is a subregion, and
   --  Mapped_Pages the pages that have space on a device, which only a
   --  root region keeps.  Bank is the bank that the region
   --  is charged to, which is its parent's for a subregion.

   type Region_Array is
     array (1 .. Max_Regions) of Region_Record;

   Regions      : Region_Array;
   Region_Count : Natural := 0;
   --  The highest region index used. A released region's index is used
   --  again, with the next generation.

   Region_Generations : Rose.Checkpoints.Store_Generations :=
                          (others => 0);
   Bank_Generations   : Rose.Checkpoints.Store_Generations :=
                          (others => 0);
   --  The generation of each region and bank slot (#967).  A cap to a
   --  region or bank carries its slot and the slot's generation as its
   --  identifier, so a cap to one that has gone doesn't reach the next
   --  one in the slot.  A slot whose generation is Retired_Generation is
   --  never used again.

   function Region_Id
     (Index : Positive)
      return Rose.Objects.Capability_Identifier;
   --  The identifier of the caps to region Index.

   function Region_Slot
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;
   --  The region whose caps carry Id, or zero if Id is not the current
   --  generation of its slot: a region that has been released.

   function Bank_Id
     (Index : Positive)
      return Rose.Objects.Capability_Identifier;

   function Bank_Slot
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;
   --  As Region_Id and Region_Slot, for banks.

   procedure Next_Generation (Generation : in out Rose.Words.Word_8);
   --  A region or bank has gone from the slot of Generation.

   type Bank_Record is
      record
         In_Use       : Boolean := False;
         Parent       : Natural := 0;
         Access_Cap   : Rose.Capabilities.Capability := 0;
         Page_Limit   : Rose.Words.Word_64 := 0;
         Region_Limit : Rose.Words.Word := 0;
         Pages        : Rose.Words.Word_64 := 0;
         Regions      : Rose.Words.Word := 0;
         Owner        : Rose.Objects.Object_Id := 0;
         Transient    : Boolean := False;
      end record;
   --  A bank (ADR 0014).  Owner is the process whose own bank it is (ADR
   --  0015, #750), or zero.  A Transient bank is a transient process's
   --  own (#524), which a restore destroys, since the process wasn't
   --  saved.  Parent is zero for the root bank.  A limit of
   --  zero is no limit.  Pages and Regions are what has been reserved
   --  from the bank and the banks below it: its pages are the blocks that
   --  have space on a device, and a subregion counts as a region.

   type Bank_Array is
     array (1 .. Max_Banks) of Bank_Record;

   Banks : Bank_Array;

   Max_Dependents : constant := 256;

   type Dependent_Record is
      record
         In_Use     : Boolean := False;
         Bank       : Natural := 0;
         Signal     : Rose.Capabilities.Capability := 0;
         Access_Cap : Rose.Capabilities.Capability := 0;
         Destroyed  : Boolean := False;
      end record;
   --  An object that a server keeps on behalf of Bank's holder (ADR 0014,
   --  section 3).  Signal is the server's signal cap, which is raised
   --  when the bank is destroyed, and deleted then.  Destroyed stays set
   --  until the server releases the record.  Dependents aren't saved in
   --  a checkpoint: a restored server adds them again.

   type Dependent_Array is
     array (1 .. Max_Dependents) of Dependent_Record;

   Dependents : Dependent_Array;

   Ended_Queue : array (1 .. Max_Banks) of Rose.Objects.Object_Id :=
                   (others => 0);
   Ended_Count : Natural := 0;
   Ended_Cap   : Rose.Capabilities.Capability := 0;
   --  The processes to end because their own banks were destroyed (ADR
   --  0015, #750), which mem takes one at a time, and the cap of mem's
   --  Processes_Ended, which the store sends to without waiting when it
   --  queues one.  The queue isn't saved: mem empties it as soon as it is
   --  told.

   procedure Destroy_Bank_Tree
     (Bank   : Natural;
      Ending : Rose.Objects.Object_Id);
   --  Destroy Bank and the banks below it, and queue the processes that
   --  own them to be ended, apart from Ending, which has ended already.

   procedure Ring_Ended;
   --  If processes are queued to be ended, tell mem, by a send that
   --  doesn't wait.

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability);
   --  Delete one of the storage server's own caps, if it isn't null.

   procedure Free_Dependent (Index : Positive);
   --  Delete dependent Index's caps, and make its record free.

   Table_Page   : System.Storage_Elements.Storage_Array
                    (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   Table_Header : Rose.Checkpoints.Store_Table_Header
     with Import, Address => Table_Page'Address;
   Region_Table : Rose.Checkpoints.Region_Page
     with Import, Address => Table_Page'Address;
   Bank_Table   : Rose.Checkpoints.Bank_Page
     with Import, Address => Table_Page'Address;
   --  The page in which Checkpoint builds the region table.

   Restore_Region_Count : Natural := 0;
   Restore_Last_Region  : Natural := 0;
   Restore_Regions_Left : Natural := 0;
   Restore_First_Bank   : Natural := 0;
   Restore_Last_Bank    : Natural := 0;
   Restore_Banks_Left   : Natural := 0;
   --  While a restore gives the tables back: the regions they hold, the
   --  last table page that holds regions, the regions whose banks are
   --  still to come, the table pages that hold the banks, and the banks
   --  still to come.

   procedure Restore_Banks (Page : System.Storage_Elements.Storage_Array);
   --  Recreate the banks in a table page of them, each with the index
   --  that the caps for it carry.

   procedure Finish_Bank_Restore;
   --  After the last bank: put a region whose bank wasn't restored, or a
   --  bank whose parent wasn't, under the root bank, and say so, then
   --  charge each bank again for its regions and their pages.

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

   function Is_Zero
     (Data : System.Storage_Elements.Storage_Array)
      return Boolean;
   --  True if every element of Data is zero.

   Stream_Buffer_Length : constant := 4096;

   type Region_Stream_Record is
      record
         Open           : Boolean := False;
         Cap            : Rose.Capabilities.Capability := 0;
         Region         : Rose.Objects.Capability_Identifier := 0;
         --  The identifier of the region that the stream reads.
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

   -----------------------
   -- Add_Backing_Store --
   -----------------------

   procedure Add_Backing_Store
     (Id     : in Rose.Objects.Capability_Identifier;
      Device : in Rose.Capabilities.Capability)
   is
      use type Rose.Objects.Capability_Identifier;
      use Rose.Interfaces.Storage.Block_Array, Rose.Interfaces.Storage
        .Block_Array.Client;
      Client : Block_Array_Client;
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

      if Backing_Store_Count = Max_Backing_Stores then
         Rose.Console_IO.Put_Line ("storage: too many backing stores");
         return;
      end if;

      --  Only the device pages up to Max_Store_Pages can be used, and
      --  only the blocks that lie wholly on the device: a page beyond it
      --  would be read and written past the end of the partition (#321).

      declare
         Room  : constant Rose.Words.Word_64 :=
                   Rose.Words.Word_64 (Max_Store_Pages)
                   - Rose.Words.Word_64 (Next_First);
         Count : Rose.Words.Word_64 := Rose.Words.Word_64 (Block_Count);
         Image : Natural;
      begin
         if Next_First >= Max_Store_Pages then
            Count := 0;
         elsif Count > Room then
            Count := Room;
            Rose.Console_IO.Put ("storage: only the first ");
            Rose.Console_IO.Put (Natural (Count / 256));
            Rose.Console_IO.Put_Line ("M of this backing store can be used");
         end if;

         Image := Natural ((Count + Space.Pages_Per_Image_Page - 1)
                           / Space.Pages_Per_Image_Page);

         if Count <= Rose.Words.Word_64 (2 * Image) then
            Rose.Console_IO.Put_Line
              ("storage: no room on this backing store");
            return;
         end if;

         Backing_Store_Count := Backing_Store_Count + 1;
         Backing_Stores (Backing_Store_Count) :=
           (Client      => Client,
            First       => Next_First,
            Count       => Rose.Words.Word_32 (Count),
            Image_Pages => Image,
            others      => <>);

         --  The next backing store starts on a page of the image of its
         --  own, so that each holds its own part of the image.

         Next_First := Next_First
           + Device_Page (Image * Space.Pages_Per_Image_Page);
      end;

      Add_Space (Backing_Store_Count);

   end Add_Backing_Store;

   -------------------
   -- Add_Dependent --
   -------------------

   function Add_Dependent
     (Id     : Rose.Objects.Capability_Identifier;
      Signal : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      Bank : constant Natural := Bank_Slot (Id);
   begin
      if Bank not in Banks'Range or else not Banks (Bank).In_Use then
         Delete_Cap (Signal);
         return 0;
      end if;

      for Index in Dependents'Range loop
         if not Dependents (Index).In_Use then
            Dependents (Index) :=
              (In_Use     => True,
               Bank       => Bank,
               Signal     => Signal,
               Access_Cap =>
                 Rose.System_Calls.Server.Create_Endpoint
                   (Create_Endpoint_Cap,
                    Rose.Interfaces.Storage.Dependent.Dependent_Interface,
                    Rose.Objects.Capability_Identifier (Index)),
               Destroyed  => False);
            return Dependents (Index).Access_Cap;
         end if;
      end loop;

      Rose.Console_IO.Put_Line ("store: out of dependents");
      Delete_Cap (Signal);
      return 0;
   end Add_Dependent;

   ---------------
   -- Add_Space --
   ---------------

   procedure Add_Space (Store : Positive) is
      This : Backing_Store_Record renames Backing_Stores (Store);
   begin

      --  Device page 0 is the first page of the first backing store's
      --  image, so it is never free, and stands for no page.

      Space.Add_Range
        (First => This.First + Device_Page (2 * This.Image_Pages),
         Bound => This.First + Device_Page (This.Count));
   end Add_Space;

   -------------
   -- Bank_Id --
   -------------

   function Bank_Id
     (Index : Positive)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      return Rose.Objects.Capability_Identifier (Index)
        + 256 * Rose.Objects.Capability_Identifier (Bank_Generations (Index));
   end Bank_Id;

   ----------------
   -- Bank_Index --
   ----------------

   function Bank_Index
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word
   is
   begin

      --  The identifier, generation and all, which Create_Process_Bank
      --  takes back.

      return Rose.Words.Word (Id);
   end Bank_Index;

   ---------------
   -- Bank_Slot --
   ---------------

   function Bank_Slot
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
      use type Rose.Objects.Capability_Identifier;
      Index : constant Natural := Natural (Id mod 256);
   begin
      if Index in 1 .. Max_Banks
        and then Natural (Id / 256) = Natural (Bank_Generations (Index))
      then
         return Index;
      else
         return 0;
      end if;
   end Bank_Slot;

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
      Bank_Count  : Natural := 0;
      Bank_Pages  : Natural;
      Bank_Slot   : Natural := 0;
      Root        : Device_Page;
      Height      : Positive;
      Written     : Boolean;

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
      Create_Root_Bank;

      --  The tree's pages and the free map's image go to the devices
      --  first, since the table names them (ADR 0038, section 8).

      Tree.Checkpoint (Root, Height, Written);

      if Written then
         Space.Checkpoint;
         Write_Images;

         --  The table names the tree's root and the image copy, so they
         --  are on the medium before the table goes to the log (ADR
         --  0038, section 8).

         Sync_Devices;
      else
         Rose.Console_IO.Put_Line
           ("storage: checkpoint: no space to write the block map; "
            & "this checkpoint can't be restored");
      end if;

      for Region of Regions (1 .. Region_Count) loop
         if Region.In_Use then
            Count := Count + 1;
         end if;
      end loop;

      for Bank of Banks loop
         if Bank.In_Use then
            Bank_Count := Bank_Count + 1;
         end if;
      end loop;

      Bank_Pages := (Bank_Count + Banks_Per_Page - 1) / Banks_Per_Page;

      Table_Page := (others => 0);
      Table_Header.Magic := Store_Table_Magic;
      Table_Header.Version := (if Written then Store_Table_Version else 0);
      Table_Header.Next_Page := Rose.Words.Word_64 (Next_Page);
      Table_Header.Page_Count :=
        Rose.Words.Word_32
          (1 + (Count + Regions_Per_Page - 1) / Regions_Per_Page
           + Bank_Pages);
      Table_Header.Region_Count := Rose.Words.Word_32 (Count);
      Table_Header.Tree_Height := Rose.Words.Word_32 (Height);
      Table_Header.Image_Copy := Rose.Words.Word_32 (Space.Image_Copy);
      Table_Header.Bank_Count := Rose.Words.Word_32 (Bank_Count);
      Table_Header.Bank_Page_Count := Rose.Words.Word_32 (Bank_Pages);
      Table_Header.Tree_Root := Rose.Words.Word_64 (Root);
      Table_Header.Region_Generations := Region_Generations;
      Table_Header.Bank_Generations := Bank_Generations;
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
                    Pages  => Rose.Words.Word_32 (Region.Mapped_Pages),
                    Bank   => Rose.Words.Word_32 (Region.Bank));
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

      --  Then the banks.

      for Index in Banks'Range loop
         if Banks (Index).In_Use then
            Bank_Slot := Bank_Slot + 1;
            Bank_Table.Banks (Bank_Slot) :=
              Saved_Bank'
                (Index        => Rose.Words.Word_32 (Index),
                 Parent       => Rose.Words.Word_32 (Banks (Index).Parent),
                 Page_Limit   => Banks (Index).Page_Limit,
                 Region_Limit =>
                   Rose.Words.Word_32 (Banks (Index).Region_Limit),
                 Owner        => Rose.Words.Word_32 (Banks (Index).Owner),
                 Flags        =>
                   (if Banks (Index).Transient
                    then Rose.Checkpoints.Bank_Transient else 0),
                 Reserved     => 0);
            if Bank_Slot = Banks_Per_Page then
               Append;
               Bank_Slot := 0;
            end if;
         end if;
      end loop;

      if Bank_Slot > 0 then
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

   ---------------
   -- Committed --
   ---------------

   procedure Committed
     (Id : Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin
      Space.Committed;
   end Committed;

   -------------------------
   -- Create_Process_Bank --
   -------------------------

   function Create_Process_Bank
     (Id        : Rose.Objects.Capability_Identifier;
      Parent    : Rose.Words.Word;
      Process   : Rose.Objects.Object_Id;
      Transient : Boolean)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      use type Rose.Capabilities.Capability;
      Cap : constant Rose.Capabilities.Capability :=
              Create_Sub_Bank
                (Rose.Objects.Capability_Identifier (Parent), 0, 0);
   begin
      if Cap /= 0 then
         for Bank of Banks loop
            if Bank.In_Use and then Bank.Access_Cap = Cap then
               Bank.Owner := Process;
               Bank.Transient := Transient;
               exit;
            end if;
         end loop;
      end if;
      return Cap;
   end Create_Process_Bank;

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
                 Rose.Interfaces.Storage.Bank.Bank_Interface,
                 Bank_Id (Root_Bank)),
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
      Parent : constant Natural := Bank_Slot (Id);
   begin
      if Parent not in 1 .. Max_Banks or else not Banks (Parent).In_Use then
         return 0;
      end if;

      for Index in Root_Bank + 1 .. Max_Banks loop
         if not Banks (Index).In_Use
           and then Bank_Generations (Index)
             /= Rose.Checkpoints.Retired_Generation
         then
            Banks (Index) :=
              (In_Use       => True,
               Parent       => Parent,
               Access_Cap   =>
                 Rose.System_Calls.Server.Create_Endpoint
                   (Create_Endpoint_Cap,
                    Rose.Interfaces.Storage.Bank.Bank_Interface,
                    Bank_Id (Index)),
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
      Parent_Index : constant Natural := Region_Slot (Id);
      Parent       : Region_Record renames
                       Regions (Natural'Max (Parent_Index, 1));
      --  Only used once Parent_Index is known to be a region.
   begin

      if Parent_Index not in 1 .. Region_Count
        or else not Regions (Parent_Index).In_Use
      then
         Rose.Console_IO.Put ("storage: subregion: no region ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.New_Line;
         return 0;
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
                        Mapped_Pages  => 0,
                        Reads => 0, Zero_Reads => 0, Writes => 0,
                        Access_Cap =>
                          Rose.System_Calls.Server.Create_Endpoint
                            (Create_Endpoint_Cap,
                             Rose.Interfaces.Storage.Region.Region_Interface,
                             Region_Id (Index)));

            Charge (Child.Bank, 0, 1);

            return Child.Access_Cap;
         end;
      end;

   end Create_Subregion;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability) is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Delete_Cap;

   -------------------------
   -- Dependent_Destroyed --
   -------------------------

   function Dependent_Destroyed
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      Index : constant Natural := Natural (Id);
   begin
      return Index in Dependents'Range
        and then Dependents (Index).In_Use
        and then Dependents (Index).Destroyed;
   end Dependent_Destroyed;

   ------------------
   -- Destroy_Bank --
   ------------------

   procedure Destroy_Bank
     (Id : Rose.Objects.Capability_Identifier)
   is
   begin
      Destroy_Bank_Tree (Bank_Slot (Id), Ending => 0);
   end Destroy_Bank;

   -----------------------
   -- Destroy_Bank_Tree --
   -----------------------

   procedure Destroy_Bank_Tree
     (Bank   : Natural;
      Ending : Rose.Objects.Object_Id)
   is
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
            Release (Region_Id (Index));
         end if;
      end loop;

      --  The dependents of the doomed banks are told, by a signal that
      --  never waits, and can be asked which they are until their
      --  servers release them.  A signal that fails belongs to a server
      --  that has ended, and its registration goes with it.

      for Index in Dependents'Range loop
         declare
            Item : Dependent_Record renames Dependents (Index);
         begin
            if Item.In_Use
              and then not Item.Destroyed
              and then Item.Bank in Doomed'Range
              and then Doomed (Item.Bank)
            then
               Item.Destroyed := True;
               Rose.System_Calls.Initialize_Send (Params, Item.Signal);
               Rose.System_Calls.Invoke_Capability (Params);
               if Params.Control.Flags (Rose.Invocation.Error) then
                  Free_Dependent (Index);
               else
                  Delete_Cap (Item.Signal);
                  Item.Signal := 0;
               end if;
            end if;
         end;
      end loop;

      --  A process whose own bank is going ends too (ADR 0015).  Mem ends
      --  it, when it is told.

      for Index in Doomed'Range loop
         if Doomed (Index)
           and then Banks (Index).Owner /= 0
           and then Banks (Index).Owner /= Ending
         then
            if Ended_Count < Ended_Queue'Last then
               Ended_Count := Ended_Count + 1;
               Ended_Queue (Ended_Count) := Banks (Index).Owner;
            else
               Rose.Console_IO.Put_Line
                 ("store: too many processes to end at once");
            end if;
         end if;
      end loop;

      Ring_Ended;

      for Index in Doomed'Range loop
         if Doomed (Index) then
            Rose.System_Calls.Initialize_Send
              (Params, Rose.System_Calls.Delete_Cap_Capability);
            Rose.System_Calls.Send_Cap (Params, Banks (Index).Access_Cap);
            Rose.System_Calls.Invoke_Capability (Params);

            Rose.Interfaces.Storage.Bank.Server.Remove_Instance
              (Bank_Id (Index));
            Banks (Index) := (others => <>);

            --  The slot's next bank has another identifier, so the caps
            --  that clients still hold don't reach it (#967).

            Next_Generation (Bank_Generations (Index));
         end if;
      end loop;
   end Destroy_Bank_Tree;

   -----------------
   -- End_Process --
   -----------------

   procedure End_Process
     (Id      : Rose.Objects.Capability_Identifier;
      Process : Rose.Objects.Object_Id)
   is
      pragma Unreferenced (Id);
   begin
      if Process = 0 then
         return;
      end if;

      for Index in Banks'Range loop
         if Banks (Index).In_Use and then Banks (Index).Owner = Process then
            Destroy_Bank_Tree (Index, Ending => Process);
            return;
         end if;
      end loop;
   end End_Process;

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

   -------------------------
   -- Finish_Bank_Restore --
   -------------------------

   procedure Finish_Bank_Restore is
      Bank_Total : Natural := 0;
   begin
      for Index in Banks'Range loop
         if Banks (Index).In_Use
           and then Index /= Root_Bank
           and then (Banks (Index).Parent not in Banks'Range
                     or else not Banks (Banks (Index).Parent).In_Use
                     or else Banks (Index).Parent = Index)
         then
            Rose.Console_IO.Put ("storage: restore: bank ");
            Rose.Console_IO.Put (Index);
            Rose.Console_IO.Put (": its parent ");
            Rose.Console_IO.Put (Banks (Index).Parent);
            Rose.Console_IO.Put_Line
              (" was not restored; it is now under the root bank");
            Banks (Index).Parent := Root_Bank;
         end if;
      end loop;

      for Bank of Banks loop
         Bank.Pages := 0;
         Bank.Regions := 0;
         if Bank.In_Use then
            Bank_Total := Bank_Total + 1;
         end if;
      end loop;

      for Index in 1 .. Region_Count loop
         declare
            Region : Region_Record renames Regions (Index);
         begin
            if Region.In_Use then
               if Region.Bank not in Banks'Range
                 or else not Banks (Region.Bank).In_Use
               then
                  Rose.Console_IO.Put ("storage: restore: region ");
                  Rose.Console_IO.Put (Index);
                  Rose.Console_IO.Put (": its bank ");
                  Rose.Console_IO.Put (Region.Bank);
                  Rose.Console_IO.Put_Line
                    (" was not restored; it is now charged to the root "
                     & "bank");
                  Region.Bank := Root_Bank;
               end if;

               Charge
                 (Region.Bank,
                  Rose.Words.Word_64 (Region.Mapped_Pages),
                  1);
            end if;
         end;
      end loop;

      Rose.Console_IO.Put ("storage: restore: banks: ");
      Rose.Console_IO.Put (Bank_Total);
      Rose.Console_IO.New_Line;

      --  A transient process wasn't saved, so its bank goes, with what
      --  is below it (#524). The processes whose banks are among them
      --  are queued to end, which mem does once it gives the store its
      --  Set_Ended cap, at every boot. The transient process itself is
      --  gone already.

      declare
         Destroyed : Natural := 0;
      begin
         for Index in Banks'Range loop
            if Banks (Index).In_Use and then Banks (Index).Transient then
               Destroy_Bank_Tree (Index, Ending => Banks (Index).Owner);
               Destroyed := Destroyed + 1;
            end if;
         end loop;

         Rose.Console_IO.Put ("storage: restore: transient banks: ");
         Rose.Console_IO.Put (Destroyed);
         Rose.Console_IO.New_Line;
      end;
   end Finish_Bank_Restore;

   --------------------
   -- Free_Dependent --
   --------------------

   procedure Free_Dependent (Index : Positive) is
      Item : Dependent_Record renames Dependents (Index);
   begin
      Delete_Cap (Item.Signal);
      Delete_Cap (Item.Access_Cap);
      Rose.Interfaces.Storage.Dependent.Server.Remove_Instance
        (Rose.Objects.Capability_Identifier (Index));
      Item := (others => <>);
   end Free_Dependent;

   ---------
   -- Get --
   ---------

   procedure Get
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data :    out System.Storage_Elements.Storage_Array)
   is
      Region_Index : constant Natural := Region_Slot (Id);
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
         Region   : Region_Record renames Regions (Region_Index);
         Location : Device_Page;
         Success  : Boolean;
      begin
         Tree.Find (Key_Of (Page), Location, Success);

         if not Success then
            Rose.Console_IO.Put_Line
              ("store: get: no space to read the block map");
         end if;

         if Location = No_Page then
            Data := (others => 0);
            Region.Zero_Reads := Region.Zero_Reads + 1;
            return;
         end if;

         Read_Device (Location, Data);
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
      Region_Index : constant Natural := Region_Slot (Id);
   begin
      if Region_Index not in 1 .. Region_Count
        or else not Regions (Region_Index).In_Use
      then
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
      Bank : constant Natural := Bank_Slot (Id);
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

   ---------------
   -- Hand_Over --
   ---------------

   procedure Hand_Over
     (Id          : Rose.Objects.Capability_Identifier;
      Old_Process : Rose.Objects.Object_Id;
      New_Process : Rose.Objects.Object_Id)
   is
      pragma Unreferenced (Id);

      procedure Refuse (Reason : String);
      --  Say why nothing changed.

      ------------
      -- Refuse --
      ------------

      procedure Refuse (Reason : String) is
      begin
         Rose.Console_IO.Put ("storage: hand over ");
         Rose.Console_IO.Put (Old_Process);
         Rose.Console_IO.Put (" to ");
         Rose.Console_IO.Put (New_Process);
         Rose.Console_IO.Put (": ");
         Rose.Console_IO.Put_Line (Reason);
      end Refuse;

      Old_Bank : Natural := 0;
      New_Bank : Natural := 0;
      Pages    : Rose.Words.Word_64 := 0;
      Regions  : Rose.Words.Word := 0;
   begin
      if Old_Process = 0 or else New_Process = 0 then
         Refuse ("no process");
         return;
      elsif Old_Process = New_Process then
         Refuse ("the same process");
         return;
      end if;

      for Index in Banks'Range loop
         if Banks (Index).In_Use then
            if Banks (Index).Owner = Old_Process then
               Old_Bank := Index;
            elsif Banks (Index).Owner = New_Process then
               New_Bank := Index;
            end if;
         end if;
      end loop;

      if Old_Bank = 0 or else New_Bank = 0 then
         Refuse ("a process has no bank");
         return;
      elsif Banks (Old_Bank).Parent /= Banks (New_Bank).Parent then
         Refuse ("their banks aren't siblings");
         return;
      end if;

      --  What the sub-banks of the old bank hold.  Moving them changes
      --  only the two banks' totals, since every bank above them is above
      --  both, so only the new bank's own limits can be broken.

      for Bank of Banks loop
         if Bank.In_Use and then Bank.Parent = Old_Bank then
            Pages := Pages + Bank.Pages;
            Regions := Regions + Bank.Regions;
         end if;
      end loop;

      declare
         Target : Bank_Record renames Banks (New_Bank);
      begin
         if (Target.Page_Limit /= 0
             and then Target.Pages + Pages > Target.Page_Limit)
           or else
             (Target.Region_Limit /= 0
              and then Target.Regions + Regions > Target.Region_Limit)
         then
            Refuse ("the new bank's limits are too low");
            return;
         end if;
      end;

      Banks (Old_Bank).Owner := New_Process;
      Banks (New_Bank).Owner := Old_Process;

      for Bank of Banks loop
         if Bank.In_Use and then Bank.Parent = Old_Bank then
            Bank.Parent := New_Bank;
         end if;
      end loop;

      Banks (Old_Bank).Pages := Banks (Old_Bank).Pages - Pages;
      Banks (Old_Bank).Regions := Banks (Old_Bank).Regions - Regions;
      Banks (New_Bank).Pages := Banks (New_Bank).Pages + Pages;
      Banks (New_Bank).Regions := Banks (New_Bank).Regions + Regions;
   end Hand_Over;

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

   ----------------
   -- New_Region --
   ----------------

   function New_Region return Natural is
   begin
      for Index in 1 .. Max_Regions loop
         if not Regions (Index).In_Use
           and then Region_Generations (Index)
             /= Rose.Checkpoints.Retired_Generation
         then
            Regions (Index).In_Use := True;
            Region_Count := Natural'Max (Region_Count, Index);
            return Index;
         end if;
      end loop;

      return 0;
   end New_Region;

   ---------------------
   -- Next_Generation --
   ---------------------

   procedure Next_Generation (Generation : in out Rose.Words.Word_8) is
   begin
      if Generation < Rose.Checkpoints.Retired_Generation then
         Generation := Generation + 1;
      end if;
   end Next_Generation;

   ---------------
   -- Owns_Bank --
   ---------------

   function Owns_Bank
     (Identifier : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      Index : constant Natural := Bank_Slot (Identifier);
   begin
      return Index /= 0 and then Banks (Index).In_Use;
   end Owns_Bank;

   ----------------
   -- Owns_Region --
   ----------------

   function Owns_Region
     (Identifier : Rose.Objects.Capability_Identifier)
      return Boolean
   is
      Index : constant Natural := Region_Slot (Identifier);
   begin
      return Index /= 0 and then Regions (Index).In_Use;
   end Owns_Region;

   -----------------
   -- Parent_Bank --
   -----------------

   function Parent_Bank
     (Id      : Rose.Objects.Capability_Identifier;
      Process : Rose.Objects.Object_Id)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if Process /= 0 then
         for Bank of Banks loop
            if Bank.In_Use and then Bank.Owner = Process then
               if Bank.Parent in Banks'Range
                 and then Banks (Bank.Parent).In_Use
               then
                  return Banks (Bank.Parent).Access_Cap;
               end if;
               return 0;
            end if;
         end loop;
      end if;
      return 0;
   end Parent_Bank;

   ------------------
   -- Process_Bank --
   ------------------

   function Process_Bank
     (Id      : Rose.Objects.Capability_Identifier;
      Process : Rose.Objects.Object_Id)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if Process /= 0 then
         for Bank of Banks loop
            if Bank.In_Use and then Bank.Owner = Process then
               return Bank.Access_Cap;
            end if;
         end loop;
      end if;
      return 0;
   end Process_Bank;

   ---------
   -- Put --
   ---------

   procedure Put
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data : in     System.Storage_Elements.Storage_Array)
   is
      Region_Index : constant Natural := Region_Slot (Id);

      procedure Fail (Message : String);
      --  The page isn't written: say why, and the caller's Put fails
      --  (#945).

      ----------
      -- Fail --
      ----------

      procedure Fail (Message : String) is
      begin
         Rose.Console_IO.Put_Line (Message);
         Rose.Server.Fail;
      end Fail;

   begin
      if Region_Index not in 1 .. Region_Count
        or else Page < Regions (Region_Index).Base
        or else Page >= Regions (Region_Index).Bound
      then
         Rose.Console_IO.Put ("store: put: invalid region/page: ");
         Rose.Console_IO.Put (Region_Index);
         Rose.Console_IO.Put ("/");
         Rose.Console_IO.Put (Page);
         Rose.Console_IO.New_Line;
         Rose.Server.Fail;
         return;
      end if;

      if Regions (Region_Index).Root not in 1 .. Region_Count
        or else not Regions (Regions (Region_Index).Root).In_Use
      then
         Fail ("store: put: region released");
         return;
      end if;

      if Data'Length /= Rose.Limits.Page_Size then
         Fail ("store: put: not a page");
         return;
      end if;

      declare
         Region   : Region_Record renames Regions (Region_Index);
         Root     : Region_Record renames Regions (Region.Root);
         Key      : constant Tree.Key_Type := Key_Of (Page);
         Words    : constant Page_Words
           with Import, Address => Data'Address;
         Location : Device_Page;
         Added    : Boolean;
         Removed  : Boolean;
         Success  : Boolean;
      begin
         Tree.Find (Key, Location, Success);

         if not Success then
            Fail ("store: out of space");
            return;
         end if;

         --  A page of zeroes is what a page with no space reads as, so it
         --  gives its space back (ADR 0038, section 2).

         if Is_Zero (Data) then
            if Location /= No_Page then
               Tree.Remove (Key, Removed, Success);
               if Removed then
                  Uncharge (Root.Bank, 1, 0);
                  Root.Mapped_Pages := Root.Mapped_Pages - 1;
               end if;
            end if;
            Region.Writes := Region.Writes + 1;
            return;
         end if;

         if Location = No_Page
           and then not Within_Limits (Root.Bank, 1, 0)
         then
            Fail ("store: bank limit reached");
            return;
         end if;

         --  A page that a checkpoint names is copied, and a new one needs
         --  space; the last few pages are kept for the tree's pages when
         --  a checkpoint writes them.

         if (Location = No_Page or else not Space.Is_Born (Location))
           and then Space.Free_Count <= Tree_Reserve
         then
            Fail ("store: out of space");
            return;
         end if;

         Tree.Put (Key, Words, Added, Success);

         if not Success then
            Fail ("store: out of space");
            return;
         end if;

         if Added then
            Charge (Root.Bank, 1, 0);
            Root.Mapped_Pages := Root.Mapped_Pages + 1;
         end if;

         Region.Writes := Region.Writes + 1;
      end;

   end Put;

   ----------
   -- Read --
   ----------

   procedure Read
     (Id      : in Rose.Objects.Capability_Identifier;
      Storage : out System.Storage_Elements.Storage_Array;
      Last    : out System.Storage_Elements.Storage_Offset)
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
            Get (Rec.Region, Rec.Current_Page, Rec.Current_Buffer);
            Rec.Buffer_Offset := 1;
         end if;

         Storage (Last) := Rec.Current_Buffer (Rec.Buffer_Offset);
         Rec.Buffer_Offset := Rec.Buffer_Offset + 1;
         Rec.Current_Offset := Rec.Current_Offset + 1;
      end loop;

   end Read;

   -----------------
   -- Read_Device --
   -----------------

   procedure Read_Device
     (Page : Device_Page;
      Data : out System.Storage_Elements.Storage_Array)
   is
      Store : constant Natural := Store_Of (Page);
      Start : constant Natural := Rose.Trace.Ticks;
   begin
      if Store = 0 then
         Rose.Console_IO.Put ("store: no backing store has page ");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Page));
         Rose.Console_IO.New_Line;
         Data := (others => 0);
         return;
      end if;

      declare
         This : Backing_Store_Record renames Backing_Stores (Store);
      begin
         Rose.Interfaces.Storage.Block_Array.Client.Read_Blocks
           (Item   => This.Client,
            Start  => Rose.Interfaces.Storage.Block_Array.Block_Address_Type
              (Page - This.First),
            Count  => 1,
            Blocks => Data);
         This.Reads := This.Reads + 1;
         This.Read_Ticks := This.Read_Ticks + (Rose.Trace.Ticks - Start);
      end;
   end Read_Device;

   -----------------
   -- Read_Stream --
   -----------------

   function Read_Stream
     (Id      : in Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use Rose.Objects;
      Index     : constant Natural := Region_Slot (Id);
      Stream_Id : Capability_Identifier := 0;
   begin
      if Index not in 1 .. Region_Count
        or else not Regions (Index).In_Use
      then
         return 0;
      end if;

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
                         Regions (Index);
      begin
         Stream_Rec :=
           Region_Stream_Record'
             (Open           => True,
              Cap            => Current_Cap,
              Region         => Id,
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
                (1, Rose.Interfaces.Streams.Reader.Reader_Interface,
                 Stream_Id);
         end if;

         return Stream_Rec.Cap;
      end;

   end Read_Stream;

   --------------------
   -- Read_Tree_Page --
   --------------------

   procedure Read_Tree_Page
     (Page : Device_Page;
      Data : out Page_Words)
   is
      Bytes : System.Storage_Elements.Storage_Array
                (1 .. Rose.Limits.Page_Size)
        with Import, Address => Data'Address;
   begin
      Read_Device (Page, Bytes);
   end Read_Tree_Page;

   ---------------
   -- Region_Id --
   ---------------

   function Region_Id
     (Index : Positive)
      return Rose.Objects.Capability_Identifier
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      return Rose.Objects.Capability_Identifier (Index)
        + 256
          * Rose.Objects.Capability_Identifier (Region_Generations (Index));
   end Region_Id;

   -----------------
   -- Region_Slot --
   -----------------

   function Region_Slot
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
      use type Rose.Objects.Capability_Identifier;
      Index : constant Natural := Natural (Id mod 256);
   begin
      if Index in 1 .. Max_Regions
        and then Natural (Id / 256) = Natural (Region_Generations (Index))
      then
         return Index;
      else
         return 0;
      end if;
   end Region_Slot;

   -------------
   -- Release --
   -------------

   procedure Release
     (Id : Rose.Objects.Capability_Identifier)
   is
      Index : constant Natural := Region_Slot (Id);
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

         --  A root region's pages that have space on a device give it
         --  back.  Release visits only the parts of the tree that hold
         --  some (ADR 0038, section 9).

         if Region.Root = Index and then Region.Mapped_Pages > 0 then
            declare
               Count   : Natural;
               Success : Boolean;
            begin
               Tree.Release
                 (Key_Of (Region.Base), Key_Of (Region.Bound),
                  Count, Success);
               if not Success then
                  Rose.Console_IO.Put_Line
                    ("store: release: no space to change the block map");
               end if;
               Freed := Rose.Words.Word_64 (Count);
            end;
         end if;

         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Region.Access_Cap);
         Rose.System_Calls.Invoke_Capability (Params);

         Uncharge (Region.Bank, Freed, 1);
         Region := (others => <>);
      end;

      --  The caps that the server made for the region's interfaces go,
      --  and the slot's next region has another identifier, so the caps
      --  that clients still hold don't reach it (#967).

      Rose.Interfaces.Storage.Region.Server.Remove_Instance (Id);
      Next_Generation (Region_Generations (Index));
   end Release;

   -----------------------
   -- Release_Dependent --
   -----------------------

   procedure Release_Dependent
     (Id : Rose.Objects.Capability_Identifier)
   is
      Index : constant Natural := Natural (Id);
   begin
      if Index in Dependents'Range and then Dependents (Index).In_Use then
         Free_Dependent (Index);
      end if;
   end Release_Dependent;

   ------------------
   -- Reserve_From --
   ------------------

   function Reserve_From
     (Bank : Positive;
      Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      Pages        : constant Rose.Words.Word_64 :=
                       (if Size = 0 then 1
                        else (Size + Rose.Limits.Page_Size - 1)
                          / Rose.Limits.Page_Size);
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
                 Rose.Interfaces.Storage.Region.Region_Interface,
                 Region_Id (Region_Index));

      begin
         Next_Page := Base + Object_Id (Pages);

         Reserved := Region_Record'
           (In_Use         => True,
            Base           => Base,
            Bound          => Next_Page,
            Access_Cap     => Cap,
            Root           => Region_Index,
            Mapped_Pages   => 0,
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
      Bank : constant Natural := Bank_Slot (Id);
   begin
      if Bank not in 1 .. Max_Banks or else not Banks (Bank).In_Use then
         return 0;
      end if;

      return Reserve_From (Bank, Size);
   end Reserve_From_Bank;

   -------------------
   -- Restore_Banks --
   -------------------

   procedure Restore_Banks (Page : System.Storage_Elements.Storage_Array) is
      use Rose.Checkpoints;
      Table : Bank_Page
        with Import, Address => Page'Address;
      Last  : constant Natural :=
                Natural'Min (Restore_Banks_Left, Banks_Per_Page);
   begin
      for I in 1 .. Last loop
         declare
            Saved : Saved_Bank renames Table.Banks (I);
            Index : constant Natural := Natural (Saved.Index);
         begin
            if Index not in Banks'Range
              or else (Index /= Root_Bank and then Banks (Index).In_Use)
            then
               Rose.Console_IO.Put ("storage: restore: bank ");
               Rose.Console_IO.Put (Index);
               Rose.Console_IO.Put_Line (" is not free");
            elsif Index = Root_Bank then

               --  Restore_Region made the root bank already.

               Create_Root_Bank;
               Banks (Root_Bank).Page_Limit := Saved.Page_Limit;
               Banks (Root_Bank).Region_Limit :=
                 Rose.Words.Word (Saved.Region_Limit);
            else
               Banks (Index) :=
                 (In_Use       => True,
                  Parent       => Natural (Saved.Parent),
                  Access_Cap   =>
                    Rose.System_Calls.Server.Create_Endpoint
                      (Create_Endpoint_Cap,
                       Rose.Interfaces.Storage.Bank.Bank_Interface,
                       Bank_Id (Index)),
                  Page_Limit   => Saved.Page_Limit,
                  Region_Limit => Rose.Words.Word (Saved.Region_Limit),
                  Owner        => Rose.Objects.Object_Id (Saved.Owner),
                  Transient    =>
                    (Saved.Flags and Bank_Transient) /= 0,
                  others       => <>);
            end if;
         end;
      end loop;

      Restore_Banks_Left := Restore_Banks_Left - Last;
   end Restore_Banks;

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
   begin
      Create_Root_Bank;

      if Index not in Regions'Range or else Regions (Index).In_Use then
         Rose.Console_IO.Put ("storage: restore: region ");
         Rose.Console_IO.Put (Index);
         Rose.Console_IO.Put_Line (" is not free");
         return;
      end if;

      --  The region's cap is made when the table's header comes, with the
      --  slot's generation (Restore_Table_Page).

      Regions (Index) :=
        Region_Record'
          (In_Use         => True,
           Base           => Base,
           Bound          => Bound,
           Access_Cap     => Rose.Capabilities.Null_Capability,
           Root           => (if Root in Regions'Range then Root else Index),
           Mapped_Pages   => 0,
           Reads          => 0,
           Zero_Reads     => 0,
           Writes         => 0,
           Bank           => Root_Bank);

      --  The region's bank comes with the table pages, and the banks are
      --  charged once they are all restored (Finish_Bank_Restore).

      Region_Count := Natural'Max (Region_Count, Index);
   end Restore_Region;

   -------------------
   -- Restore_Space --
   -------------------

   procedure Restore_Space
     (Root   : Device_Page;
      Height : Positive;
      Copy   : Space.Copy_Index)
   is
   begin

      --  The free map is what the checkpoint's copy of its image says,
      --  on each backing store, which have been added again in the same
      --  order (ADR 0038, section 4).  Nothing written before the
      --  restore is kept.

      Space.Reset;

      for Store in 1 .. Backing_Store_Count loop
         Add_Space (Store);
      end loop;

      for Store of Backing_Stores (1 .. Backing_Store_Count) loop
         for J in 0 .. Store.Image_Pages - 1 loop
            Read_Device
              (Store.First
               + Device_Page (Natural (Copy) * Store.Image_Pages + J),
               Device_Buffer);
            Space.Load_Image
              (Copy  => Copy,
               Index => Natural (Store.First) / Space.Pages_Per_Image_Page
                 + J + 1,
               Data  => Image_Buffer);
         end loop;
      end loop;

      Tree.Restore (Root, Height);

      Rose.Console_IO.Put ("storage: restore: free pages: ");
      Rose.Console_IO.Put (Space.Free_Count);
      Rose.Console_IO.New_Line;
   end Restore_Space;

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
      use type Rose.Capabilities.Capability;
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
            Restore_Regions_Left := 0;
            Restore_First_Bank := 0;
            Restore_Last_Bank := 0;
            Restore_Banks_Left := 0;

            if Header.Magic = Store_Table_Magic
              and then Header.Version = Store_Table_Version
              and then Natural (Header.Region_Count) <= Max_Regions
              and then Natural (Header.Bank_Count) in 1 .. Max_Banks
              and then Header.Tree_Height in 1 .. Tree.Max_Height
              and then Header.Image_Copy <= 1
              and then Header.Tree_Root < Max_Store_Pages
            then
               Next_Page :=
                 Rose.Objects.Object_Id'Max
                   (Next_Page, Rose.Objects.Object_Id (Header.Next_Page));
               Restore_Region_Count := Natural (Header.Region_Count);
               Restore_Last_Region :=
                 1 + (Restore_Region_Count + Regions_Per_Page - 1)
                 / Regions_Per_Page;
               Restore_Regions_Left := Restore_Region_Count;
               Restore_First_Bank := Restore_Last_Region + 1;
               Restore_Last_Bank :=
                 Restore_Last_Region + Natural (Header.Bank_Page_Count);
               Restore_Banks_Left := Natural (Header.Bank_Count);
               Region_Generations := Header.Region_Generations;
               Bank_Generations := Header.Bank_Generations;

               Restore_Space
                 (Root   => Device_Page (Header.Tree_Root),
                  Height => Positive (Header.Tree_Height),
                  Copy   => Space.Copy_Index (Header.Image_Copy));
            elsif Header.Magic = Store_Table_Magic
              and then Header.Version /= Store_Table_Version
            then
               Rose.Console_IO.Put
                 ("storage: restore: the tables are version ");
               Rose.Console_IO.Put (Natural (Header.Version));
               Rose.Console_IO.Put (", and this store reads version ");
               Rose.Console_IO.Put (Natural'(Store_Table_Version));
               Rose.Console_IO.New_Line;
            else
               Rose.Console_IO.Put_Line
                 ("storage: restore: the table header is not one I read");
            end if;
         end;

         --  Restore_Region recreated the regions before the header came,
         --  and now their slots' generations are known (#967).

         for Region in 1 .. Region_Count loop
            if Regions (Region).In_Use
              and then Regions (Region).Access_Cap
                = Rose.Capabilities.Null_Capability
            then
               Regions (Region).Access_Cap :=
                 Rose.System_Calls.Server.Create_Endpoint
                   (Create_Endpoint_Cap,
                    Rose.Interfaces.Storage.Region.Region_Interface,
                    Region_Id (Region));
            end if;
         end loop;

      elsif Index in 2 .. Restore_Last_Region then

         --  The regions are recreated already: set each one's bank, and
         --  the pages of it that have space, which charge the bank.

         declare
            Table : Region_Page
              with Import, Address => Page'Address;
            Last  : constant Natural :=
                      Natural'Min (Restore_Regions_Left, Regions_Per_Page);
         begin
            for I in 1 .. Last loop
               declare
                  Saved  : Saved_Region renames Table.Regions (I);
                  Region : constant Natural := Natural (Saved.Index);
               begin
                  if Region in 1 .. Region_Count
                    and then Regions (Region).In_Use
                  then
                     Regions (Region).Bank := Natural (Saved.Bank);
                     Regions (Region).Mapped_Pages := Natural (Saved.Pages);
                  end if;
               end;
            end loop;

            Restore_Regions_Left := Restore_Regions_Left - Last;
         end;

      elsif Index in Restore_First_Bank .. Restore_Last_Bank then
         Restore_Banks (Page);

         if Index = Restore_Last_Bank then
            Finish_Bank_Restore;
         end if;
      end if;
   end Restore_Table_Page;

   ----------------
   -- Ring_Ended --
   ----------------

   procedure Ring_Ended is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Ended_Count > 0 and then Ended_Cap /= 0 then
         Rose.System_Calls.Initialize_Send (Params, Ended_Cap);
         Params.Control.Flags (Rose.Invocation.Block) := False;
         Params.Control.Flags (Rose.Invocation.Create_Reply_Cap) := False;
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Ring_Ended;

   ---------------
   -- Set_Ended --
   ---------------

   procedure Set_Ended
     (Id    : Rose.Objects.Capability_Identifier;
      Ended : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
   begin
      Delete_Cap (Ended_Cap);
      Ended_Cap := Ended;
      Ring_Ended;
   end Set_Ended;

   --------------
   -- Store_Of --
   --------------

   function Store_Of (Page : Device_Page) return Natural is
   begin
      for Store in 1 .. Backing_Store_Count loop
         declare
            This : Backing_Store_Record renames Backing_Stores (Store);
         begin
            if Page >= This.First
              and then Page - This.First < Device_Page (This.Count)
            then
               return Store;
            end if;
         end;
      end loop;
      return 0;
   end Store_Of;

   ------------------
   -- Sync_Devices --
   ------------------

   procedure Sync_Devices is
   begin
      for Store of Backing_Stores (1 .. Backing_Store_Count) loop
         Rose.Interfaces.Storage.Block_Array.Client.Flush (Store.Client);
      end loop;
   end Sync_Devices;

   ----------------
   -- Take_Ended --
   ----------------

   function Take_Ended
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Object_Id
   is
      pragma Unreferenced (Id);
   begin
      if Ended_Count = 0 then
         return 0;
      end if;

      Ended_Count := Ended_Count - 1;
      return Ended_Queue (Ended_Count + 1);
   end Take_Ended;

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

   ------------------
   -- Write_Device --
   ------------------

   procedure Write_Device
     (Page : Device_Page;
      Data : System.Storage_Elements.Storage_Array)
   is
      Store : constant Natural := Store_Of (Page);
      Start : constant Natural := Rose.Trace.Ticks;
   begin
      if Store = 0 then
         Rose.Console_IO.Put ("store: no backing store has page ");
         Rose.Console_IO.Put (Rose.Words.Word_64 (Page));
         Rose.Console_IO.New_Line;
         return;
      end if;

      declare
         This : Backing_Store_Record renames Backing_Stores (Store);
      begin
         Rose.Interfaces.Storage.Block_Array.Client.Write_Blocks
           (Item   => This.Client,
            Start  => Rose.Interfaces.Storage.Block_Array.Block_Address_Type
              (Page - This.First),
            Count  => 1,
            Blocks => Data);
         This.Writes := This.Writes + 1;
         This.Write_Ticks := This.Write_Ticks + (Rose.Trace.Ticks - Start);
      end;
   end Write_Device;

   ------------------
   -- Write_Images --
   ------------------

   procedure Write_Images is
      Copy : constant Space.Copy_Index := Space.Image_Copy;
   begin
      for Store of Backing_Stores (1 .. Backing_Store_Count) loop
         for J in 0 .. Store.Image_Pages - 1 loop
            declare
               Index : constant Space.Image_Page_Index :=
                         Natural (Store.First) / Space.Pages_Per_Image_Page
                         + J + 1;
            begin
               if Space.Is_Dirty (Copy, Index) then
                  Space.Get_Image (Index, Image_Buffer);
                  Write_Device
                    (Store.First
                     + Device_Page (Natural (Copy) * Store.Image_Pages + J),
                     Device_Buffer);
                  Space.Set_Written (Copy, Index);
               end if;
            end;
         end loop;
      end loop;
   end Write_Images;

   ---------------------
   -- Write_Tree_Page --
   ---------------------

   procedure Write_Tree_Page
     (Page : Device_Page;
      Data : Page_Words)
   is
      Bytes : constant System.Storage_Elements.Storage_Array
                (1 .. Rose.Limits.Page_Size)
        with Import, Address => Data'Address;
   begin
      Write_Device (Page, Bytes);
   end Write_Tree_Page;

begin

   --  Every device page is in use until a backing store is added, and
   --  the block map is empty until a checkpoint is restored.

   Space.Reset;
   Tree.Create;
end Store.Devices;
