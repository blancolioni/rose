--  The tests of the store's block map and free map (ADR 0038, #504):
--  Store.Page_Trees over Store.Page_Bitmaps, on a backing store in
--  memory.  Besides each test's own checks:
--
--  * every write to the device must be to a page that is born, which
--    no checkpoint names (Test_Device);
--
--  * a crash restores the last committed checkpoint from the device,
--    with its image of the free map, and everything it named must read
--    back as it was;
--
--  * after a checkpoint and its commit, with nothing changed between,
--    the pages in use must be exactly those that the tree names.

with Ada.Command_Line;
with Ada.Text_IO;

with Rose.Words;

with Store.Device_Pages;

with Test_Device;
with Test_Space;
with Test_Tree;

procedure Test_Page_Maps is

   use type Rose.Words.Word_64;
   use Store.Device_Pages;

   subtype Key_Type is Test_Tree.Key_Type;

   Checks   : Natural := 0;
   Failures : Natural := 0;

   type Saved_State is
      record
         Root   : Device_Page := No_Page;
         Height : Positive := 1;
         Copy   : Test_Space.Copy_Index := 0;
         Valid  : Boolean := False;
      end record;
   --  What a checkpoint's log entry holds for the store.

   Committed_State : Saved_State;
   Pending_State   : Saved_State;

   procedure Check (Condition : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 25 then
            Ada.Text_IO.Put_Line ("FAIL: " & Message);
         end if;
      end if;
   end Check;

   function Data_For (Key : Key_Type; Version : Rose.Words.Word_64)
                      return Page_Words
   is
      Data : Page_Words := (others => 0);
   begin
      Data (0) := Key;
      Data (1) := Version;
      Data (511) := Key xor Version xor 16#5A5A_A5A5_5A5A_A5A5#;
      return Data;
   end Data_For;

   function Used_Count return Natural is
     (Test_Device.Page_Count - Test_Space.Free_Count);
   --  The device's pages that are in use.

   --  Write the image pages that changed to the copy that the checkpoint
   --  writes, as the store would.

   procedure Write_Image (Copy : Test_Space.Copy_Index) is
      Data : Page_Words;
   begin
      for Index in Test_Space.Image_Page_Index loop
         if Test_Space.Is_Dirty (Copy, Index) then
            Test_Space.Get_Image (Index, Data);
            Test_Device.Write_Image (Natural (Copy), Index, Data);
            Test_Space.Set_Written (Copy, Index);
         end if;
      end loop;
   end Write_Image;

   procedure Take_Checkpoint is
      State   : Saved_State;
      Success : Boolean;
   begin
      Test_Tree.Checkpoint (State.Root, State.Height, Success);
      Check (Success, "a checkpoint writes the tree");
      Check (Test_Tree.Dirty_Pages = 0, "a checkpoint leaves no page dirty");
      Test_Space.Checkpoint;
      State.Copy := Test_Space.Image_Copy;
      Write_Image (State.Copy);
      State.Valid := Success;
      Pending_State := State;
   end Take_Checkpoint;

   procedure Commit is
   begin
      if Pending_State.Valid then
         Test_Space.Committed;
         Committed_State := Pending_State;
         Pending_State.Valid := False;
      end if;
   end Commit;

   --  Restart from the last committed checkpoint, as a restore does.

   procedure Crash is
      Data : Page_Words;
   begin
      Check (Committed_State.Valid, "a committed checkpoint to restore");
      Test_Space.Reset;
      Test_Space.Add_Range (1, Test_Device.Page_Count + 1);
      for Index in Test_Space.Image_Page_Index loop
         Test_Device.Read_Image
           (Natural (Committed_State.Copy), Index, Data);
         Test_Space.Load_Image (Committed_State.Copy, Index, Data);
      end loop;
      Test_Tree.Restore (Committed_State.Root, Committed_State.Height);
      Pending_State.Valid := False;
   end Crash;

   --  The pages that the tree with this root names, itself included,
   --  each of which must be in use and named once.

   function Reachable (Root : Device_Page; Height : Positive) return Natural
   is
      Seen  : array (Device_Page range 1 .. Test_Device.Page_Count)
        of Boolean := (others => False);
      Count : Natural := 0;

      procedure Visit (Page : Device_Page; Level : Integer) is
         Data : Page_Words;
      begin
         if Page not in Seen'Range then
            Check (False, "a named page is on the device");
            return;
         end if;

         Check (not Seen (Page), "a page is named once");
         Check (Test_Space.Is_In_Use (Page), "a named page is in use");
         Seen (Page) := True;
         Count := Count + 1;

         if Level >= 0 then
            Test_Device.Peek (Page, Data);
            for Value of Data loop
               if Value /= 0 then
                  Visit (Device_Page (Value and 16#FFFF_FFFF#), Level - 1);
               end if;
            end loop;
         end if;
      end Visit;

   begin
      if Root /= No_Page then
         Visit (Root, Height - 1);
      end if;
      return Count;
   end Reachable;

   --  Take a checkpoint and commit it, and check that what is in use is
   --  exactly what the tree names.

   procedure Settle (Context : String) is
   begin
      Take_Checkpoint;
      Commit;
      Check (Used_Count
             = Reachable (Committed_State.Root, Committed_State.Height),
             Context & ": the pages in use are those the tree names");
   end Settle;

   procedure Start is
   begin
      Test_Space.Reset;
      Test_Space.Add_Range (1, Test_Device.Page_Count + 1);
      Test_Tree.Create;
      Pending_State.Valid := False;
      Settle ("start");
   end Start;

   procedure Expect
     (Key     : Key_Type;
      Version : Rose.Words.Word_64;
      Context : String)
   is
      Data           : Page_Words;
      Found, Success : Boolean;
   begin
      Test_Tree.Get (Key, Data, Found, Success);
      Check (Success, Context & ": get");
      if Version = 0 then
         Check (not Found, Context & ": key" & Key'Image & " has nothing");
      else
         Check (Found and then Data = Data_For (Key, Version),
                Context & ": key" & Key'Image & " has version"
                & Version'Image);
      end if;
   end Expect;

   procedure Put
     (Key     : Key_Type;
      Version : Rose.Words.Word_64;
      Added   : out Boolean)
   is
      Success : Boolean;
   begin
      Test_Tree.Put (Key, Data_For (Key, Version), Added, Success);
      Check (Success, "put of key" & Key'Image);
   end Put;

   procedure Put (Key : Key_Type; Version : Rose.Words.Word_64) is
      Added : Boolean;
   begin
      Put (Key, Version, Added);
   end Put;

   procedure Remove (Key : Key_Type; Removed : out Boolean) is
      Success : Boolean;
   begin
      Test_Tree.Remove (Key, Removed, Success);
      Check (Success, "remove of key" & Key'Image);
   end Remove;

   ------------------
   -- Test_Bitmaps --
   ------------------

   procedure Test_Bitmaps is
      use type Test_Space.Copy_Index;
      Pages : array (1 .. 99) of Device_Page;
      Page  : Device_Page;
      Data  : Page_Words;
   begin
      Test_Space.Reset;
      Test_Space.Add_Range (1, 100);
      Check (Test_Space.Free_Count = 99, "bitmap: 99 pages added");

      Test_Space.Get_Image (1, Data);
      Check (Data (0) = 1, "bitmap: page 0 is in use, pages 1 to 63 free");
      Check (Data (1) = not (2 ** 36 - 1),
             "bitmap: pages 64 to 99 free, 100 on in use");
      Check (Data (2) = Rose.Words.Word_64'Last,
             "bitmap: pages past the range are in use");

      for I in Pages'Range loop
         Test_Space.Allocate (Pages (I));
         Check (Pages (I) in 1 .. 99, "bitmap: allocated page in range");
         Check (Test_Space.Is_Born (Pages (I)), "bitmap: allocated is born");
         for J in 1 .. I - 1 loop
            Check (Pages (J) /= Pages (I), "bitmap: pages differ");
         end loop;
      end loop;

      Test_Space.Allocate (Page);
      Check (Page = No_Page, "bitmap: nothing left");

      Test_Space.Free (Pages (5));
      Check (Test_Space.Free_Count = 1, "bitmap: a born page is freed now");
      Test_Space.Allocate (Page);
      Check (Page = Pages (5), "bitmap: and given out again");

      Check (Test_Space.Image_Copy = 0, "bitmap: the first image copy is 0");
      Test_Space.Set_Written (0, 1);
      Check (not Test_Space.Is_Dirty (0, 1), "bitmap: copy 0 written");
      Check (Test_Space.Is_Dirty (1, 1), "bitmap: copy 1 not written");

      Test_Space.Checkpoint;
      Check (not Test_Space.Is_Born (Pages (5)),
             "bitmap: nothing is born after a checkpoint");
      Test_Space.Committed;
      Check (Test_Space.Image_Copy = 1,
             "bitmap: after a commit, the other copy");

      Test_Space.Free (Pages (7));
      Check (Test_Space.Is_Held (Pages (7)), "bitmap: a freed page is held");
      Check (Test_Space.Free_Count = 0, "bitmap: a held page isn't free");
      Test_Space.Free (Pages (7));
      Check (Test_Space.Free_Count = 0, "bitmap: freeing twice is ignored");

      Test_Space.Checkpoint;
      Test_Space.Get_Image (1, Data);
      Check ((Data (0) and 2 ** Natural (Pages (7))) = 0
             or else Pages (7) >= 64,
             "bitmap: a waiting page is free in the image");
      Check (Test_Space.Is_In_Use (Pages (7)),
             "bitmap: but in use until the commit");
      Test_Space.Allocate (Page);
      Check (Page = No_Page, "bitmap: a waiting page isn't given out");

      --  A checkpoint that isn't committed: the next one keeps holding
      --  what the first one held, and writes the same copy.

      Test_Space.Free (Pages (9));
      Test_Space.Checkpoint;
      Check (Test_Space.Image_Copy = 1,
             "bitmap: an uncommitted checkpoint's copy is used again");
      Test_Space.Allocate (Page);
      Check (Page = No_Page, "bitmap: still nothing free");

      Test_Space.Committed;
      Check (Test_Space.Free_Count = 2, "bitmap: the commit frees both");
      Check (Test_Space.Image_Copy = 0, "bitmap: copy 1 is committed");
      Check (not Test_Space.Is_In_Use (Pages (7))
             and then not Test_Space.Is_In_Use (Pages (9)),
             "bitmap: the waiting pages are free");
   end Test_Bitmaps;

   -----------------
   -- Test_Basics --
   -----------------

   procedure Test_Basics is
      Keys    : constant array (Positive range <>) of Key_Type :=
                  (0, 1, 511, 512, 513, 262_143, 262_144, 2 ** 40,
                   2 ** 62, Test_Tree.Max_Key);
      Added   : Boolean;
      Removed : Boolean;
      Before  : Natural;
   begin
      Start;

      for Key of Keys loop
         Put (Key, 1, Added);
         Check (Added, "basics: a new key is added");
      end loop;

      Check (Test_Tree.Tree_Height = 7, "basics: the tree grew to 7 levels");

      for Key of Keys loop
         Expect (Key, 1, "basics");
      end loop;
      Expect (2, 0, "basics");
      Expect (1000, 0, "basics");
      Expect (2 ** 41, 0, "basics");

      Before := Test_Space.Free_Count;
      Put (0, 2, Added);
      Check (not Added, "basics: a key is added once");
      Check (Test_Space.Free_Count = Before,
             "basics: a born page is written in place");
      Expect (0, 2, "basics");

      Settle ("basics");

      Before := Test_Space.Free_Count;
      Put (0, 3);
      Check (Test_Space.Free_Count < Before,
             "basics: after a checkpoint, a write goes to a new page");
      Expect (0, 3, "basics");

      Remove (2 ** 40, Removed);
      Check (Removed, "basics: remove a key");
      Expect (2 ** 40, 0, "basics");
      Remove (2 ** 40, Removed);
      Check (not Removed, "basics: remove it again");

      Settle ("basics");
      Crash;
      Check (Used_Count
             = Reachable (Committed_State.Root, Committed_State.Height),
             "basics: after a restore, the pages in use are those named");

      Expect (0, 3, "basics, restored");
      Expect (2 ** 40, 0, "basics, restored");
      for Key of Keys (2 .. Keys'Last) loop
         if Key /= 2 ** 40 then
            Expect (Key, 1, "basics, restored");
         end if;
      end loop;
   end Test_Basics;

   ------------------------------
   -- Test_Crash_Before_Commit --
   ------------------------------

   procedure Test_Crash_Before_Commit is
      A : constant Key_Type := 1_000;
      B : constant Key_Type := 1_001;
      C : constant Key_Type := 2 ** 30;
      D : constant Key_Type := 2 ** 50;
      Removed : Boolean;
   begin
      Start;
      Put (A, 1);
      Put (B, 1);
      Settle ("crash");

      Put (A, 2);
      Put (C, 1);
      Remove (B, Removed);
      Take_Checkpoint;

      --  The checkpoint isn't committed.

      Put (A, 3);
      Put (D, 1);
      Crash;

      Expect (A, 1, "crash before commit");
      Expect (B, 1, "crash before commit");
      Expect (C, 0, "crash before commit");
      Expect (D, 0, "crash before commit");
      Check (Used_Count
             = Reachable (Committed_State.Root, Committed_State.Height),
             "crash before commit: in use is what the checkpoint named");

      --  Two checkpoints in a row that aren't committed, then a crash.

      Put (A, 4);
      Take_Checkpoint;
      Put (A, 5);
      Remove (B, Removed);
      Take_Checkpoint;
      Put (A, 6);
      Crash;

      Expect (A, 1, "two uncommitted checkpoints");
      Expect (B, 1, "two uncommitted checkpoints");

      --  And a committed one after all that.

      Put (A, 7);
      Settle ("crash");
      Put (A, 8);
      Crash;
      Expect (A, 7, "committed after crashes");
      Expect (B, 1, "committed after crashes");
   end Test_Crash_Before_Commit;

   -------------------------
   -- Test_Cache_Pressure --
   -------------------------

   procedure Test_Cache_Pressure is
      Count : constant := 1_000;
      Seed  : Rose.Words.Word_64 := 12345;
      Keys  : array (1 .. Count) of Key_Type;
   begin
      Start;

      for I in Keys'Range loop
         Seed := Seed * 6364136223846793005 + 1442695040888963407;
         Keys (I) := Shift_Right (Seed, 19);
         for J in 1 .. I - 1 loop
            if Keys (J) = Keys (I) then
               Keys (I) := Keys (I) + 1;
            end if;
         end loop;
         Put (Keys (I), Rose.Words.Word_64 (I));
      end loop;

      Check (Test_Tree.Cached_Pages <= 12, "cache: no more pages than slots");

      for I in Keys'Range loop
         Expect (Keys (I), Rose.Words.Word_64 (I), "cache");
      end loop;

      Settle ("cache");
      Crash;

      for I in Keys'Range loop
         Expect (Keys (I), Rose.Words.Word_64 (I), "cache, restored");
      end loop;
   end Test_Cache_Pressure;

   ------------------
   -- Test_Release --
   ------------------

   procedure Test_Release is
      Outside : constant Key_Type := 2 ** 40;
      Freed   : Natural;
      Success : Boolean;
      Removed : Boolean;
   begin
      Start;

      for Key in Key_Type range 0 .. 599 loop
         Put (Key, 1);
      end loop;

      for I in Key_Type range 1 .. 200 loop
         Put (I * 2 ** 28 + I, 1);
      end loop;

      Put (Outside, 1);
      Settle ("release");

      Test_Tree.Release (0, 2 ** 37, Freed, Success);
      Check (Success, "release: succeeds");
      Check (Freed = 800, "release: frees 800 keys, not" & Freed'Image);

      Expect (0, 0, "release");
      Expect (599, 0, "release");
      Expect (7 * 2 ** 28 + 7, 0, "release");
      Expect (Outside, 1, "release");

      Settle ("release");
      Settle ("release");

      Remove (Outside, Removed);
      Check (Removed, "release: remove the last key");
      Settle ("release");
      Settle ("release");
      Check (Used_Count = 1,
             "release: only the root is left, not" & Used_Count'Image);

      --  Released keys can be written again.

      Put (5, 2);
      Expect (5, 2, "release");
   end Test_Release;

   -----------------
   -- Test_Random --
   -----------------

   --  Random operations, checked against a model of the keys' versions:
   --  now, at the pending checkpoint, and at the committed one.

   procedure Test_Random is
      Pool_Size : constant := 400;
      type Version_Array is array (1 .. Pool_Size) of Rose.Words.Word_64;

      Pool      : array (1 .. Pool_Size) of Key_Type;
      Current   : Version_Array := (others => 0);
      Pending   : Version_Array := (others => 0);
      Committed : Version_Array := (others => 0);
      Version   : Rose.Words.Word_64 := 0;
      Seed      : Rose.Words.Word_64 := 987654321;

      function Next (Bound : Positive) return Positive is
      begin
         Seed := Seed * 6364136223846793005 + 1442695040888963407;
         return Natural (Shift_Right (Seed, 33)
                         mod Rose.Words.Word_64 (Bound)) + 1;
      end Next;

      procedure Check_All (Context : String) is
      begin
         for I in Pool'Range loop
            Expect (Pool (I), Current (I), Context);
         end loop;
      end Check_All;

   begin
      for I in Pool'Range loop
         Pool (I) :=
           (case I is
               when 1 .. 100   => Key_Type (I) * 37,
               when 101 .. 250 => Key_Type (I) * 1_000_003,
               when 251 .. 350 => 2 ** 30 + Key_Type (I) * 2 ** 20,
               when others     => 2 ** 50 + Key_Type (I) * 2 ** 44);
      end loop;

      Start;

      for Step in 1 .. 30_000 loop
         declare
            Choice : constant Positive := Next (100);
            I      : constant Positive := Next (Pool_Size);
         begin
            case Choice is
               when 1 .. 45 =>
                  Version := Version + 1;
                  Put (Pool (I), Version);
                  Current (I) := Version;

               when 46 .. 60 =>
                  declare
                     Removed : Boolean;
                  begin
                     Remove (Pool (I), Removed);
                     Check (Removed = (Current (I) /= 0),
                            "random: removed only what was there");
                     Current (I) := 0;
                  end;

               when 61 .. 62 =>
                  declare
                     J        : constant Positive := Next (Pool_Size);
                     First    : constant Key_Type :=
                                  Key_Type'Min (Pool (I), Pool (J));
                     Bound    : constant Key_Type :=
                                  Key_Type'Max (Pool (I), Pool (J)) + 1;
                     Expected : Natural := 0;
                     Freed    : Natural;
                     Success  : Boolean;
                  begin
                     for K in Pool'Range loop
                        if Pool (K) in First .. Bound - 1 then
                           if Current (K) /= 0 then
                              Expected := Expected + 1;
                           end if;
                           Current (K) := 0;
                        end if;
                     end loop;
                     Test_Tree.Release (First, Bound, Freed, Success);
                     Check (Success and then Freed = Expected,
                            "random: release frees what was there");
                  end;

               when 63 .. 85 =>
                  Expect (Pool (I), Current (I), "random");

               when 86 .. 92 =>
                  Take_Checkpoint;
                  Pending := Current;

               when 93 .. 97 =>
                  if Pending_State.Valid then
                     Commit;
                     Committed := Pending;
                  end if;

               when 98 .. 99 =>
                  Crash;
                  Current := Committed;
                  Pending := Committed;
                  Check_All ("random, restored at step" & Step'Image);

               when others =>
                  Settle ("random");
                  Committed := Current;
                  Pending := Current;
            end case;
         end;
      end loop;

      Settle ("random, at the end");
      Committed := Current;
      Check_All ("random, at the end");

      declare
         Freed   : Natural;
         Success : Boolean;
      begin
         Test_Tree.Release (0, Test_Tree.Max_Key + 1, Freed, Success);
         Check (Success, "random: release everything");
         Settle ("random, released");
         Settle ("random, released");
         Check (Used_Count = 1,
                "random: only the root is left, not" & Used_Count'Image);
      end;
   end Test_Random;

begin
   Test_Bitmaps;
   Test_Basics;
   Test_Crash_Before_Commit;
   Test_Cache_Pressure;
   Test_Release;
   Test_Random;

   Check (Test_Device.Bad_Writes = 0,
          "every write was to a born page, not" & Test_Device.Bad_Writes'Image
          & " bad of" & Test_Device.Writes'Image);
   Check (Test_Device.Bad_Reads = 0, "every read was of a device page");

   Ada.Text_IO.Put_Line
     ("store page maps:" & Checks'Image & " checks," & Failures'Image
      & " failed");

   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Test_Page_Maps;
