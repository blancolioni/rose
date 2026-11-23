package body Store.Page_Trees is

   use type Rose.Words.Word_32;

   Index_Bits    : constant := 9;
   Index_Mask    : constant Rose.Words.Word_64 := 511;
   Location_Mask : constant Rose.Words.Word_64 := 16#FFFF_FFFF#;

   subtype Slot_Count is Natural range 0 .. Cache_Size;
   subtype Slot_Index is Slot_Count range 1 .. Cache_Size;

   type Slot_Record is
      record
         In_Use   : Boolean := False;
         Level    : Natural := 0;
         Base     : Key_Type := 0;
         Location : Device_Page := No_Page;
         Dirty    : Boolean := False;
         Parent   : Slot_Count := 0;
         Children : Natural := 0;
         Last_Use : Rose.Words.Word_32 := 0;
      end record;
   --  A page of the tree in the cache.  Level is 0 for a leaf, Base is
   --  the first of the keys that the page covers, and Location is its
   --  device page, or No_Page if it has never been written.  Dirty is
   --  set if the page has changed since it was read or written.  Parent
   --  is the slot of the page above it, which is always in the cache
   --  too, and Children is the number of its pages below it that are in
   --  the cache: a page with children is never evicted.  A new page that
   --  has never been written has no entry in its parent, so it is found
   --  by its slot.

   type Slot_Array is array (Slot_Index) of Slot_Record;
   type Page_Array is array (Slot_Index) of Page_Words;

   Slots     : Slot_Array;

   No_Slots  : constant Slot_Array := (others => (others => <>));
   --  What Slots is cleared to: a copy, not an aggregate, which would be
   --  built on the stack (#1004).
   Pages     : Page_Array;
   Root_Slot : Slot_Index := 1;
   Levels    : Positive := 1;
   Clock     : Rose.Words.Word_32 := 0;

   function Base_Of
     (Key   : Key_Type;
      Level : Natural)
      return Key_Type;
   --  The first key of the page at Level that covers Key.

   procedure Descend
     (Key     : Key_Type;
      Create  : Boolean;
      Leaf    : out Slot_Count;
      Success : out Boolean);
   --  The leaf that covers Key, which is 0 if there is none and Create
   --  is False.  If Create is True, the pages on the way are made.

   procedure Drop (Slot : Slot_Index);
   --  Take a page that has no children out of the cache.

   function Entry_Of
     (Key   : Key_Type;
      Level : Natural)
      return Word_Index;
   --  The entry for Key in the page at Level that covers it.

   function Find_Slot
     (Level : Natural;
      Base  : Key_Type)
      return Slot_Count;
   --  The slot of the page at Level whose first key is Base, or 0.

   function Fits (Key : Key_Type) return Boolean;
   --  True if the tree, at its height, covers Key.

   procedure Get_Child
     (Parent  : Slot_Index;
      Key     : Key_Type;
      Create  : Boolean;
      Child   : out Slot_Count;
      Success : out Boolean);
   --  The slot of the page below Parent that covers Key, read into the
   --  cache if it isn't there.  It is 0 if there is no such page and
   --  Create is False; if Create is True, a new empty page is made.

   procedure Grow (Success : out Boolean);
   --  Add a level above the root.

   function Is_Empty (Slot : Slot_Index) return Boolean;
   --  True if Slot's page has no entries.

   function Location_Of
     (Value : Rose.Words.Word_64)
      return Device_Page;
   --  The device page of an entry.

   procedure Prune (Start : Slot_Index);
   --  Free Start's page if it is empty, and then each page above it that
   --  that leaves empty, short of the root.

   function Span (Level : Natural) return Key_Type;
   --  The keys that a page at Level covers.

   procedure Take_Slot
     (Except  : Slot_Count;
      Slot    : out Slot_Index;
      Success : out Boolean);
   --  A free slot, evicting a page if there is none.  The page in slot
   --  Except is not evicted.  A dirty page is written when it is evicted,
   --  and that can fail.

   procedure Touch (Slot : Slot_Index);
   --  Slot has been used.

   procedure Write_Slot
     (Slot    : Slot_Index;
      Success : out Boolean);
   --  Write Slot's page: in place if its device page is born, or else to
   --  a new device page, which goes into its parent's entry.

   -------------
   -- Base_Of --
   -------------

   function Base_Of
     (Key   : Key_Type;
      Level : Natural)
      return Key_Type
   is (Shift_Left (Shift_Right (Key, Index_Bits * (Level + 1)),
                   Index_Bits * (Level + 1)));

   ------------------
   -- Cached_Pages --
   ------------------

   function Cached_Pages return Natural is
      Count : Natural := 0;
   begin
      for Slot of Slots loop
         if Slot.In_Use then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Cached_Pages;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Root    : out Device_Page;
      Height  : out Positive;
      Success : out Boolean)
   is
   begin
      Height := Levels;

      --  Writing a page that moves changes its parent, so each level is
      --  written before the one above it.

      for Level in 0 .. Levels - 1 loop
         for Slot in Slot_Index loop
            if Slots (Slot).In_Use
              and then Slots (Slot).Level = Level
              and then Slots (Slot).Dirty
            then
               Write_Slot (Slot, Success);
               if not Success then
                  Root := No_Page;
                  return;
               end if;
            end if;
         end loop;
      end loop;

      Root := Slots (Root_Slot).Location;
      Success := True;
   end Checkpoint;

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      Slots := No_Slots;
      Pages (1) := (others => 0);
      Slots (1) :=
        (In_Use   => True,
         Level    => 0,
         Base     => 0,
         Location => No_Page,
         Dirty    => True,
         Parent   => 0,
         Children => 0,
         Last_Use => 0);
      Root_Slot := 1;
      Levels := 1;
   end Create;

   -------------
   -- Descend --
   -------------

   procedure Descend
     (Key     : Key_Type;
      Create  : Boolean;
      Leaf    : out Slot_Count;
      Success : out Boolean)
   is
      Slot  : Slot_Index;
      Child : Slot_Count;
   begin
      Leaf := 0;

      if Key > Max_Key then
         Success := not Create;
         return;
      end if;

      while not Fits (Key) loop
         if not Create then
            Success := True;
            return;
         end if;

         Grow (Success);
         if not Success then
            return;
         end if;
      end loop;

      Slot := Root_Slot;
      Touch (Slot);

      while Slots (Slot).Level > 0 loop
         Get_Child (Slot, Key, Create, Child, Success);
         if not Success or else Child = 0 then
            return;
         end if;
         Slot := Child;
      end loop;

      Leaf := Slot;
      Success := True;
   end Descend;

   -----------------
   -- Dirty_Pages --
   -----------------

   function Dirty_Pages return Natural is
      Count : Natural := 0;
   begin
      for Slot of Slots loop
         if Slot.In_Use and then Slot.Dirty then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Dirty_Pages;

   ----------
   -- Drop --
   ----------

   procedure Drop (Slot : Slot_Index) is
      Parent : constant Slot_Count := Slots (Slot).Parent;
   begin
      if Parent /= 0 then
         Slots (Parent).Children := Slots (Parent).Children - 1;
      end if;
      Slots (Slot) := (others => <>);
   end Drop;

   --------------
   -- Entry_Of --
   --------------

   function Entry_Of
     (Key   : Key_Type;
      Level : Natural)
      return Word_Index
   is (Word_Index (Shift_Right (Key, Index_Bits * Level) and Index_Mask));

   ----------
   -- Find --
   ----------

   procedure Find
     (Key     : Key_Type;
      Page    : out Device_Page;
      Success : out Boolean)
   is
      Leaf : Slot_Count;
   begin
      Descend (Key, False, Leaf, Success);
      Page := (if Success and then Leaf /= 0
               then Location_Of (Pages (Leaf) (Entry_Of (Key, 0)))
               else No_Page);
   end Find;

   ---------------
   -- Find_Slot --
   ---------------

   function Find_Slot
     (Level : Natural;
      Base  : Key_Type)
      return Slot_Count
   is
   begin
      for Slot in Slot_Index loop
         if Slots (Slot).In_Use
           and then Slots (Slot).Level = Level
           and then Slots (Slot).Base = Base
         then
            return Slot;
         end if;
      end loop;
      return 0;
   end Find_Slot;

   ----------
   -- Fits --
   ----------

   function Fits (Key : Key_Type) return Boolean
   is (Shift_Right (Key, Index_Bits * Levels) = 0);

   ---------
   -- Get --
   ---------

   procedure Get
     (Key     : Key_Type;
      Data    : out Page_Words;
      Found   : out Boolean;
      Success : out Boolean)
   is
      Page : Device_Page;
   begin
      Find (Key, Page, Success);
      Found := Page /= No_Page;
      if Found then
         Read (Page, Data);
      else
         Data := (others => 0);
      end if;
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Parent  : Slot_Index;
      Key     : Key_Type;
      Create  : Boolean;
      Child   : out Slot_Count;
      Success : out Boolean)
   is
      Level : constant Natural := Slots (Parent).Level - 1;
      Base  : constant Key_Type := Base_Of (Key, Level);
      Index : constant Word_Index := Entry_Of (Key, Level + 1);
      Slot  : Slot_Index;
   begin
      Child := Find_Slot (Level, Base);

      if Child /= 0 then
         Touch (Child);
         Success := True;
         return;
      end if;

      if Location_Of (Pages (Parent) (Index)) = No_Page
        and then not Create
      then
         Success := True;
         return;
      end if;

      --  Evicting a page can move it, which changes another entry of
      --  Parent, but never this one, so it is read again afterwards.

      Take_Slot (Parent, Slot, Success);
      if not Success then
         return;
      end if;

      declare
         Location : constant Device_Page :=
                      Location_Of (Pages (Parent) (Index));
      begin
         if Location = No_Page then
            Pages (Slot) := (others => 0);
         else
            Read (Location, Pages (Slot));
         end if;

         Slots (Slot) :=
           (In_Use   => True,
            Level    => Level,
            Base     => Base,
            Location => Location,
            Dirty    => Location = No_Page,
            Parent   => Parent,
            Children => 0,
            Last_Use => 0);
      end;

      Slots (Parent).Children := Slots (Parent).Children + 1;
      Touch (Slot);
      Child := Slot;
   end Get_Child;

   ----------
   -- Grow --
   ----------

   procedure Grow (Success : out Boolean) is
      Old_Root : constant Slot_Index := Root_Slot;
      Slot     : Slot_Index;
   begin
      if Slots (Old_Root).Children = 0 and then Is_Empty (Old_Root) then

         --  There is nothing below the root, so it just moves up a level.

         if Slots (Old_Root).Location /= No_Page then
            Free (Slots (Old_Root).Location);
         end if;

         Slots (Old_Root).Level := Levels;
         Slots (Old_Root).Location := No_Page;
         Slots (Old_Root).Dirty := True;
         Levels := Levels + 1;
         Success := True;
         return;
      end if;

      Take_Slot (0, Slot, Success);
      if not Success then
         return;
      end if;

      Pages (Slot) := (others => 0);
      Pages (Slot) (0) := Rose.Words.Word_64 (Slots (Old_Root).Location);
      Slots (Slot) :=
        (In_Use   => True,
         Level    => Levels,
         Base     => 0,
         Location => No_Page,
         Dirty    => True,
         Parent   => 0,
         Children => 1,
         Last_Use => 0);
      Slots (Old_Root).Parent := Slot;
      Root_Slot := Slot;
      Levels := Levels + 1;
      Touch (Slot);
   end Grow;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Slot : Slot_Index) return Boolean is
   begin
      for Value of Pages (Slot) loop
         if Value /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;

   -----------------
   -- Location_Of --
   -----------------

   function Location_Of
     (Value : Rose.Words.Word_64)
      return Device_Page
   is (Device_Page (Value and Location_Mask));

   -----------
   -- Prune --
   -----------

   procedure Prune (Start : Slot_Index) is
      Slot : Slot_Index := Start;
   begin
      while Slot /= Root_Slot
        and then Slots (Slot).Children = 0
        and then Is_Empty (Slot)
      loop
         declare
            Parent : constant Slot_Index := Slots (Slot).Parent;
         begin
            if Slots (Slot).Location /= No_Page then
               Free (Slots (Slot).Location);
            end if;

            Pages (Parent) (Entry_Of (Slots (Slot).Base,
                                      Slots (Parent).Level)) := 0;
            Slots (Parent).Dirty := True;
            Drop (Slot);
            Slot := Parent;
         end;
      end loop;
   end Prune;

   ---------
   -- Put --
   ---------

   procedure Put
     (Key     : Key_Type;
      Data    : Page_Words;
      Added   : out Boolean;
      Success : out Boolean)
   is
      Leaf : Slot_Count;
   begin
      Added := False;
      Descend (Key, True, Leaf, Success);
      if not Success or else Leaf = 0 then
         Success := False;
         return;
      end if;

      declare
         Index  : constant Word_Index := Entry_Of (Key, 0);
         Old    : constant Device_Page := Location_Of (Pages (Leaf) (Index));
         Target : Device_Page := Old;
      begin
         if Old = No_Page or else not Is_Born (Old) then
            Allocate (Target);

            if Target = No_Page then

               --  The pages that Descend made for Key are empty.

               Prune (Leaf);
               Success := False;
               return;
            end if;

            Pages (Leaf) (Index) := Rose.Words.Word_64 (Target);
            Slots (Leaf).Dirty := True;

            if Old = No_Page then
               Added := True;
            else
               Free (Old);
            end if;
         end if;

         Write (Target, Data);
      end;
   end Put;

   -------------
   -- Release --
   -------------

   procedure Release
     (First   : Key_Type;
      Bound   : Key_Type;
      Freed   : out Natural;
      Success : out Boolean)
   is
      Last  : constant Key_Type := Key_Type'Min (Bound, Max_Key + 1);
      Key   : Key_Type := First;
      Slot  : Slot_Index;
      Child : Slot_Count;
   begin
      Freed := 0;
      Success := True;

      --  Each time round, go down from the root towards Key.  If a page
      --  on the way isn't there, skip the keys it would cover; at a leaf,
      --  free the entries from Key on, and prune it.

      while Key < Last and then Fits (Key) loop
         Slot := Root_Slot;

         loop
            if Slots (Slot).Level = 0 then
               declare
                  Next : constant Key_Type := Slots (Slot).Base + 512;
                  Page : Device_Page;
               begin
                  while Key < Last and then Key < Next loop
                     Page := Location_Of (Pages (Slot) (Entry_Of (Key, 0)));
                     if Page /= No_Page then
                        Free (Page);
                        Pages (Slot) (Entry_Of (Key, 0)) := 0;
                        Slots (Slot).Dirty := True;
                        Freed := Freed + 1;
                     end if;
                     Key := Key + 1;
                  end loop;
               end;

               Prune (Slot);
               exit;
            end if;

            Get_Child (Slot, Key, False, Child, Success);
            if not Success then
               return;
            end if;

            if Child = 0 then
               Key := Base_Of (Key, Slots (Slot).Level - 1)
                 + Span (Slots (Slot).Level - 1);
               exit;
            end if;

            Slot := Child;
         end loop;
      end loop;
   end Release;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Key     : Key_Type;
      Removed : out Boolean;
      Success : out Boolean)
   is
      Leaf : Slot_Count;
   begin
      Removed := False;
      Descend (Key, False, Leaf, Success);
      if not Success or else Leaf = 0 then
         return;
      end if;

      declare
         Index : constant Word_Index := Entry_Of (Key, 0);
         Page  : constant Device_Page := Location_Of (Pages (Leaf) (Index));
      begin
         if Page /= No_Page then
            Free (Page);
            Pages (Leaf) (Index) := 0;
            Slots (Leaf).Dirty := True;
            Removed := True;
            Prune (Leaf);
         end if;
      end;
   end Remove;

   -------------
   -- Restore --
   -------------

   procedure Restore
     (Root   : Device_Page;
      Height : Positive)
   is
   begin
      if Root = No_Page then
         Create;
         return;
      end if;

      Slots := No_Slots;
      Read (Root, Pages (1));
      Slots (1) :=
        (In_Use   => True,
         Level    => Height - 1,
         Base     => 0,
         Location => Root,
         Dirty    => False,
         Parent   => 0,
         Children => 0,
         Last_Use => 0);
      Root_Slot := 1;
      Levels := Height;
   end Restore;

   ----------
   -- Span --
   ----------

   function Span (Level : Natural) return Key_Type
   is (Shift_Left (1, Index_Bits * (Level + 1)));

   ---------------
   -- Take_Slot --
   ---------------

   procedure Take_Slot
     (Except  : Slot_Count;
      Slot    : out Slot_Index;
      Success : out Boolean)
   is
      Victim : Slot_Count := 0;
   begin
      for Free_Slot in Slot_Index loop
         if not Slots (Free_Slot).In_Use then
            Slot := Free_Slot;
            Success := True;
            return;
         end if;
      end loop;

      --  The least recently used page that has no children, preferring
      --  one that hasn't changed.

      for Candidate in Slot_Index loop
         declare
            This : Slot_Record renames Slots (Candidate);
         begin
            if Candidate /= Root_Slot
              and then Candidate /= Except
              and then This.Children = 0
              and then
                (Victim = 0
                 or else (Slots (Victim).Dirty and then not This.Dirty)
                 or else (Slots (Victim).Dirty = This.Dirty
                          and then This.Last_Use < Slots (Victim).Last_Use))
            then
               Victim := Candidate;
            end if;
         end;
      end loop;

      if Victim = 0 then
         Slot := Slot_Index'First;
         Success := False;
         return;
      end if;

      if Slots (Victim).Dirty then
         Write_Slot (Victim, Success);
         if not Success then
            Slot := Victim;
            return;
         end if;
      end if;

      Drop (Victim);
      Slot := Victim;
      Success := True;
   end Take_Slot;

   -----------
   -- Touch --
   -----------

   procedure Touch (Slot : Slot_Index) is
   begin
      Clock := Clock + 1;
      Slots (Slot).Last_Use := Clock;
   end Touch;

   -----------------
   -- Tree_Height --
   -----------------

   function Tree_Height return Positive is (Levels);

   ----------------
   -- Write_Slot --
   ----------------

   procedure Write_Slot
     (Slot    : Slot_Index;
      Success : out Boolean)
   is
      This   : Slot_Record renames Slots (Slot);
      Target : Device_Page := This.Location;
   begin
      if Target = No_Page or else not Is_Born (Target) then
         Allocate (Target);
         if Target = No_Page then
            Success := False;
            return;
         end if;

         if This.Location /= No_Page then
            Free (This.Location);
         end if;

         This.Location := Target;

         if This.Parent /= 0 then
            Pages (This.Parent) (Entry_Of (This.Base,
                                           Slots (This.Parent).Level)) :=
              Rose.Words.Word_64 (Target);
            Slots (This.Parent).Dirty := True;
         end if;
      end if;

      Write (Target, Pages (Slot));
      This.Dirty := False;
      Success := True;
   end Write_Slot;

end Store.Page_Trees;
