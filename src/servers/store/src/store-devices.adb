with Rose.Checkpoints;
with Rose.Limits;
with Rose.Allocators;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Trace;
with Rose.System_Calls.Server;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Region;
with Rose.Interfaces.Region.Server;
with Rose.Interfaces.Stream_Reader;

package body Store.Devices is

   Max_Backing_Stores : constant := 16;
   Max_Regions        : constant := 256;
   Bank_Size_Bits     : constant := 2;
   Store_Size_Bits    : constant := 21;
   Minimum_Bank_Pages : constant := 2 ** Bank_Size_Bits;
   Minimum_Bank_Size  : constant := Minimum_Bank_Pages * Rose.Limits.Page_Size;
   Max_Open_Streams   : constant := 10;

   Max_Store_Pages : constant := 2 ** (Store_Size_Bits - 1);
   --  The pages that Allocator can hand out, on all the backing stores
   --  together: 4 GiB.  Its tree of 2 ** (Store_Size_Bits - Bank_Size_Bits)
   --  blocks has half that many banks as leaves.

   Max_Fresh_Pages : constant := Max_Store_Pages;
   --  Every page that Allocator can hand out, on any backing store.

   type Fresh_Page_Array is
     array (0 .. Max_Fresh_Pages - 1) of Boolean
     with Pack;

   Fresh : Fresh_Page_Array;
   --  True for a page that has been reserved and not written since, which
   --  reads as zeroes without a read from the device (#203), indexed from
   --  Page_Object_Id'First. Everything else is read. Only the pages of
   --  regions are kept: Reserve_Storage marks a new region's pages fresh,
   --  Restore_Region marks a restored region's pages not fresh, and
   --  Restore_Table_Page then puts back the ones a checkpoint saved as
   --  fresh (#327). Other pages may hold anything.

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
         Backing_Device : Natural := 0;
         Base, Bound    : Rose.Objects.Object_Id := 0;
         Access_Cap     : Rose.Capabilities.Capability := 0;
         Alloc_Index    : Natural := 0;
         Alloc_Banks    : Natural := 0;
         Reads          : Natural := 0;
         Zero_Reads     : Natural := 0;
         Writes         : Natural := 0;
         Restored       : Boolean := False;
      end record;
   --  A region, with the banks it was allocated, which a subregion does
   --  not have: Alloc_Index is zero.  Restored is True for a region that
   --  Restore_Region recreated, whose fresh pages Restore_Table_Page
   --  puts back.

   type Region_Array is
     array (1 .. Max_Regions) of Region_Record;

   Regions      : Region_Array;
   Region_Count : Natural := 0;
   --  The highest region index used. A released region's index is used
   --  again.

   Table_Page   : System.Storage_Elements.Storage_Array
                    (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   Table_Header : Rose.Checkpoints.Store_Table_Header
     with Import, Address => Table_Page'Address;
   Region_Table : Rose.Checkpoints.Region_Page
     with Import, Address => Table_Page'Address;
   Fresh_Table  : Rose.Checkpoints.Fresh_Page
     with Import, Address => Table_Page'Address;
   --  The page in which Checkpoint builds the region table.

   Restore_Region_Count : Natural := 0;
   Restore_Last_Region  : Natural := 0;
   Restore_First_Fresh  : Natural := 0;
   Restore_Last_Fresh   : Natural := 0;
   --  While a restore gives the tables back: the regions they hold, the
   --  last table page that holds regions, and the table pages that hold
   --  fresh pages (zero if there are none).

   type Saved_Space_Record is
      record
         Index       : Natural := 0;
         Base, Bound : Rose.Objects.Object_Id := 0;
      end record;

   type Saved_Space_Array is
     array (1 .. Max_Regions) of Saved_Space_Record;

   Saved_Spaces      : Saved_Space_Array;
   Saved_Space_Count : Natural := 0;
   --  The regions with space of their own in the tables a restore gives
   --  back, in the order their fresh pages are saved.

   Restore_Space     : Natural := 0;
   Restore_Next_Page : Rose.Objects.Object_Id := 0;
   --  The next page whose freshness a restore gives back, and the space
   --  it is in; Restore_Space is zero before the first.

   function New_Region return Natural;
   --  The index of a region that is not in use, or zero if there is none.

   function Is_Fresh
     (Page : Rose.Objects.Object_Id)
      return Boolean;
   --  True if Page has been reserved and not written since.

   procedure Set_Fresh
     (Base, Bound : Rose.Objects.Object_Id;
      Value       : Boolean);
   --  Mark the pages Base .. Bound - 1 fresh or not.

   procedure Next_Restored_Page
     (Page  : out Rose.Objects.Object_Id;
      Space : out Natural);
   --  The next page of the saved regions with space of their own, in the
   --  order Checkpoint saves them, and its index in Saved_Spaces; zero if
   --  there are no more.

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
     new Rose.Allocators (Store_Size_Bits - Bank_Size_Bits);

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
         --  banks that lie wholly on the device can be allocated: a page
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
              Natural ((Base - Page_Object_Id'First) / Minimum_Bank_Pages) + 1,
            Bound     =>
              Natural ((Bound - Page_Object_Id'First) / Minimum_Bank_Pages)
            + 1);
      end;

   end Add_Backing_Store;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Id          : Rose.Objects.Capability_Identifier;
      Append_Page : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
      use Rose.Checkpoints;
      use type Rose.Objects.Object_Id;
      Count       : Natural := 0;
      Slot        : Natural := 0;
      Fresh_Bits  : Natural := 0;
      Fresh_Pages : Natural;
      Bit         : Natural := 0;

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
            if Region.Alloc_Index > 0 then
               Fresh_Bits := Fresh_Bits + Natural (Region.Bound - Region.Base);
            end if;
         end if;
      end loop;

      Fresh_Pages :=
        (Fresh_Bits + Fresh_Bits_Per_Page - 1) / Fresh_Bits_Per_Page;

      Table_Page := (others => 0);
      Table_Header.Magic := Store_Table_Magic;
      Table_Header.Version := Store_Table_Version;
      Table_Header.Page_Count :=
        Rose.Words.Word_32
          (1 + (Count + Regions_Per_Page - 1) / Regions_Per_Page
           + Fresh_Pages);
      Table_Header.Region_Count := Rose.Words.Word_32 (Count);
      Table_Header.Fresh_Page_Count := Rose.Words.Word_32 (Fresh_Pages);
      Append;

      for Index in 1 .. Region_Count loop
         declare
            Region : Region_Record renames Regions (Index);
         begin
            if Region.In_Use then
               Slot := Slot + 1;
               Region_Table.Regions (Slot) :=
                 Saved_Region'
                   (Index       => Rose.Words.Word_32 (Index),
                    Device      => Rose.Words.Word_32 (Region.Backing_Device),
                    Base        => Rose.Words.Word_64 (Region.Base),
                    Bound       => Rose.Words.Word_64 (Region.Bound),
                    Alloc_Index => Rose.Words.Word_32 (Region.Alloc_Index),
                    Alloc_Banks => Rose.Words.Word_32 (Region.Alloc_Banks));
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

      --  Then the fresh pages of each region with space of its own.

      for Region of Regions (1 .. Region_Count) loop
         if Region.In_Use and then Region.Alloc_Index > 0 then
            for Page in Region.Base .. Region.Bound - 1 loop
               Bit := Bit + 1;
               Fresh_Table (Bit) := Is_Fresh (Page);
               if Bit = Fresh_Bits_Per_Page then
                  Append;
                  Bit := 0;
               end if;
            end loop;
         end if;
      end loop;

      if Bit > 0 then
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
                        Alloc_Index => 0, Alloc_Banks => 0,
                        Reads => 0, Zero_Reads => 0, Writes => 0,
                        Restored => False,
                        Access_Cap =>
                          Rose.System_Calls.Server.Create_Endpoint
                            (Create_Endpoint_Cap,
                             Rose.Interfaces.Region.Region_Interface,
                             Capability_Identifier (Index)));

            return Child.Access_Cap;
         end;
      end;

   end Create_Subregion;

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
      use type Rose.Objects.Object_Id;
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
      use type Rose.Objects.Object_Id;
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
         Region : Region_Record renames Regions (Region_Index);
         Store      : Backing_Store_Record renames
                        Backing_Stores (Region.Backing_Device);
         Start      : constant Natural := Rose.Trace.Ticks;
      begin
         if Is_Fresh (Page) then
            Data := (others => 0);
            Store.Zero_Reads := Store.Zero_Reads + 1;
            Region.Zero_Reads := Region.Zero_Reads + 1;
            return;
         end if;

         Rose.Interfaces.Block_Device.Client.Read_Blocks
           (Item   => Store.Client,
            Start  =>
              Rose.Interfaces.Block_Device.Block_Address_Type
                (Page - Store.Base),
            Count  => 1,
            Blocks => Data);
         Store.Reads := Store.Reads + 1;
         Store.Read_Ticks := Store.Read_Ticks + (Rose.Trace.Ticks - Start);
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

   --------------
   -- Is_Fresh --
   --------------

   function Is_Fresh
     (Page : Rose.Objects.Object_Id)
      return Boolean
   is
      use Rose.Objects;
   begin
      return Page >= Page_Object_Id'First
        and then Page - Page_Object_Id'First < Max_Fresh_Pages
        and then Fresh (Natural (Page - Page_Object_Id'First));
   end Is_Fresh;

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
                   + Natural (Request_Page_Count) / Minimum_Bank_Pages;
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

   ------------------------
   -- Next_Restored_Page --
   ------------------------

   procedure Next_Restored_Page
     (Page  : out Rose.Objects.Object_Id;
      Space : out Natural)
   is
      use type Rose.Objects.Object_Id;
   begin
      while Restore_Space = 0
        or else (Restore_Space <= Saved_Space_Count
                 and then Restore_Next_Page
                   >= Saved_Spaces (Restore_Space).Bound)
      loop
         Restore_Space := Restore_Space + 1;
         exit when Restore_Space > Saved_Space_Count;
         Restore_Next_Page := Saved_Spaces (Restore_Space).Base;
      end loop;

      if Restore_Space > Saved_Space_Count then
         Page := 0;
         Space := 0;
         return;
      end if;

      Page := Restore_Next_Page;
      Space := Restore_Space;
      Restore_Next_Page := Restore_Next_Page + 1;
   end Next_Restored_Page;

   ---------
   -- Put --
   ---------

   procedure Put
     (Id   : in     Rose.Objects.Capability_Identifier;
      Page : in     Rose.Objects.Object_Id;
      Data : in     System.Storage_Elements.Storage_Array)
   is
      use type Rose.Objects.Object_Id;
      Region_Index : constant Natural := Natural (Id);
   begin
      if Region_Index not in 1 .. Region_Count
        or else Page < Regions (Region_Index).Base
        or else Page >= Regions (Region_Index).Bound
      then
         return;
      end if;

      declare
         Region : Region_Record renames Regions (Region_Index);
         Store      : Backing_Store_Record renames
                        Backing_Stores (Region.Backing_Device);
         Start      : constant Natural := Rose.Trace.Ticks;
      begin
         Rose.Interfaces.Block_Device.Client.Write_Blocks
           (Item   => Store.Client,
            Start  =>
              Rose.Interfaces.Block_Device.Block_Address_Type
                (Page - Store.Base),
            Count  => 1,
            Blocks => Data);
         Set_Fresh (Page, Page + 1, False);
         Store.Writes := Store.Writes + 1;
         Store.Write_Ticks :=
           Store.Write_Ticks + (Rose.Trace.Ticks - Start);
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
      use type Rose.Objects.Object_Id;
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
      begin
         Rose.Trace.Event
           ("store: region released: reads ", Region.Reads,
            ", writes ", Region.Writes);
         Rose.Trace.Event
           ("store: region released: zero reads ", Region.Zero_Reads);
         if Region.Backing_Device in 1 .. Backing_Store_Count then
            declare
               Store : Backing_Store_Record renames
                 Backing_Stores (Region.Backing_Device);
            begin
               Rose.Trace.Event
                 ("store: total reads ", Store.Reads,
                  ", ticks ", Store.Read_Ticks);
               Rose.Trace.Event
                 ("store: total zero reads ", Store.Zero_Reads);
               Rose.Trace.Event
                 ("store: total writes ", Store.Writes,
                  ", ticks ", Store.Write_Ticks);
            end;
         end if;

         if Region.Alloc_Index > 0 then
            Space_Allocators.Deallocate
              (Allocator, Region.Alloc_Index,
               Region.Alloc_Index + Region.Alloc_Banks);
         end if;

         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Region.Access_Cap);
         Rose.System_Calls.Invoke_Capability (Params);

         Rose.Interfaces.Region.Server.Remove_Instance (Id);
         Region := (others => <>);
      end;
   end Release;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability
   is
      Alloc_Index  : Natural;
      Alloc_Size   : Natural := Natural (Size);
      Region_Index : constant Natural := New_Region;
   begin
      if Region_Index = 0 then
         Rose.Console_IO.Put_Line ("store: out of regions");
         return 0;
      end if;

      if Alloc_Size < Minimum_Bank_Size then
         Alloc_Size := Minimum_Bank_Size;
      elsif Alloc_Size mod Minimum_Bank_Size /= 0 then
         Alloc_Size := (Alloc_Size / Minimum_Bank_Size + 1)
           * Minimum_Bank_Size;
      end if;

      Alloc_Index :=
        Space_Allocators.Allocate
          (Allocator, Alloc_Size / Minimum_Bank_Size);

      if Alloc_Index = 0 then
         Rose.Console_IO.Put ("store: cannot allocate ");
         Rose.Console_IO.Put (Alloc_Size);
         Rose.Console_IO.Put (" bytes");
         Rose.Console_IO.New_Line;
         return 0;
      end if;

      declare
         use Rose.Objects;
         New_Bank : Region_Record renames
                      Regions (Region_Index);
         Store    : Positive := 1;
         Base     : Object_Id;
         Bound    : Object_Id;
         Cap      : constant Rose.Capabilities.Capability :=
              Rose.System_Calls.Server.Create_Endpoint
                (Create_Endpoint_Cap,
                 Rose.Interfaces.Region.Region_Interface,
                 Capability_Identifier (Region_Index));

      begin
         while Backing_Stores (Store).Allocator_Bound <= Alloc_Index loop
            Store := Store + 1;
         end loop;

         Base :=
           Object_Id
             ((Alloc_Index - Backing_Stores (Store).Allocator_Base)
              * Minimum_Bank_Pages)
           + Page_Object_Id'First;
         Bound :=
           Base + Object_Id (Alloc_Size / Rose.Limits.Page_Size);

         New_Bank := Region_Record'
           (In_Use         => True,
            Backing_Device => Store,
            Base           => Base,
            Bound          => Bound,
            Access_Cap     => Cap,
            Alloc_Index    => Alloc_Index,
            Alloc_Banks    => Alloc_Size / Minimum_Bank_Size,
            Reads          => 0,
            Zero_Reads     => 0,
            Writes         => 0,
            Restored       => False);

         --  New storage holds nothing, whatever its device holds.

         Set_Fresh (Base, Bound, True);

         return Cap;
      end;
   end Reserve_Storage;

   --------------------
   -- Restore_Region --
   --------------------

   procedure Restore_Region
     (Id          : Rose.Objects.Capability_Identifier;
      Index       : Natural;
      Device      : Natural;
      Base        : Rose.Objects.Object_Id;
      Bound       : Rose.Objects.Object_Id;
      Alloc_Index : Natural;
      Alloc_Banks : Natural)
   is
      pragma Unreferenced (Id);
      use Rose.Objects;
   begin
      if Index not in Regions'Range or else Regions (Index).In_Use then
         Rose.Console_IO.Put ("storage: restore: region ");
         Rose.Console_IO.Put (Index);
         Rose.Console_IO.Put_Line (" is not free");
         return;
      end if;

      if Device not in 1 .. Backing_Store_Count then
         Rose.Console_IO.Put ("storage: restore: no backing store ");
         Rose.Console_IO.Put (Device);
         Rose.Console_IO.New_Line;
         return;
      end if;

      --  A subregion has no space of its own.

      if Alloc_Index > 0
        and then not Space_Allocators.Allocate_At
          (Allocator, Alloc_Index, Alloc_Banks)
      then
         Rose.Console_IO.Put ("storage: restore: space for region ");
         Rose.Console_IO.Put (Index);
         Rose.Console_IO.Put_Line (" is not free");
         return;
      end if;

      --  The region's pages are not fresh until Restore_Table_Page gives
      --  back the ones that were: the others hold what was written to
      --  them before the restart.

      if Alloc_Index > 0 then
         Set_Fresh (Base, Bound, False);
      end if;

      Regions (Index) :=
        Region_Record'
          (In_Use         => True,
           Backing_Device => Device,
           Base           => Base,
           Bound          => Bound,
           Access_Cap     =>
             Rose.System_Calls.Server.Create_Endpoint
               (Create_Endpoint_Cap,
                Rose.Interfaces.Region.Region_Interface,
                Capability_Identifier (Index)),
           Alloc_Index    => Alloc_Index,
           Alloc_Banks    => Alloc_Banks,
           Reads          => 0,
           Zero_Reads     => 0,
           Writes         => 0,
           Restored       => True);

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
      use type Rose.Objects.Object_Id;
      use type Rose.Words.Word_32, Rose.Words.Word_64;
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
            Restore_First_Fresh := 0;
            Restore_Last_Fresh := 0;
            Saved_Space_Count := 0;
            Restore_Space := 0;

            --  A checkpoint before version 2 has no fresh pages.

            if Header.Magic = Store_Table_Magic
              and then Header.Version >= 2
              and then Header.Fresh_Page_Count > 0
              and then Natural (Header.Region_Count) <= Max_Regions
            then
               Restore_Region_Count := Natural (Header.Region_Count);
               Restore_Last_Region :=
                 1 + (Restore_Region_Count + Regions_Per_Page - 1)
                 / Regions_Per_Page;
               Restore_First_Fresh := Restore_Last_Region + 1;
               Restore_Last_Fresh :=
                 Restore_Last_Region + Natural (Header.Fresh_Page_Count);
            end if;
         end;

      elsif Index in 2 .. Restore_Last_Region then

         --  The regions, to find the ones whose fresh pages are saved.

         declare
            Table : Region_Page
              with Import, Address => Page'Address;
            First : constant Natural := (Index - 2) * Regions_Per_Page + 1;
            Last  : constant Natural :=
                      Natural'Min (Restore_Region_Count,
                                   First + Regions_Per_Page - 1);
         begin
            for I in First .. Last loop
               declare
                  Region : Saved_Region renames
                             Table.Regions (I - First + 1);
               begin
                  if Region.Alloc_Index > 0 then
                     Saved_Space_Count := Saved_Space_Count + 1;
                     Saved_Spaces (Saved_Space_Count) :=
                       (Index => Natural (Region.Index),
                        Base  => Rose.Objects.Object_Id (Region.Base),
                        Bound => Rose.Objects.Object_Id (Region.Bound));
                  end if;
               end;
            end loop;
         end;

      elsif Index in Restore_First_Fresh .. Restore_Last_Fresh then
         declare
            Bits     : Fresh_Page
              with Import, Address => Page'Address;
            Next     : Rose.Objects.Object_Id;
            Space    : Natural;
            Restored : Natural := 0;
         begin
            for Bit of Bits loop
               Next_Restored_Page (Next, Space);
               exit when Space = 0;

               --  Only the pages of a region that was recreated as it
               --  was saved.

               declare
                  Saved : Saved_Space_Record renames Saved_Spaces (Space);
               begin
                  if Saved.Index in 1 .. Region_Count
                    and then Regions (Saved.Index).In_Use
                    and then Regions (Saved.Index).Restored
                    and then Regions (Saved.Index).Base = Saved.Base
                    and then Regions (Saved.Index).Bound = Saved.Bound
                  then
                     Set_Fresh (Next, Next + 1, Bit);
                     if Bit then
                        Restored := Restored + 1;
                     end if;
                  end if;
               end;
            end loop;

            Rose.Console_IO.Put ("storage: restore: fresh pages: ");
            Rose.Console_IO.Put (Restored);
            Rose.Console_IO.New_Line;
         end;
      end if;
   end Restore_Table_Page;

   ---------------
   -- Set_Fresh --
   ---------------

   procedure Set_Fresh
     (Base, Bound : Rose.Objects.Object_Id;
      Value       : Boolean)
   is
      use Rose.Objects;
   begin
      for Page in Base .. Bound - 1 loop
         if Page >= Page_Object_Id'First
           and then Page - Page_Object_Id'First < Max_Fresh_Pages
         then
            Fresh (Natural (Page - Page_Object_Id'First)) := Value;
         end if;
      end loop;
   end Set_Fresh;

end Store.Devices;
