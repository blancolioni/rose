with Rose.Boot.Console;
with Rose.Limits;
with Rose.Words;

with Rose.Kernel.Heap;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Physical_Memory;

package body Rose.Kernel.Checkpoint_Copies is

   Active : Boolean := False;

   type Reserve_Index is range 0 .. Max_Reserve;

   type Reserve_Entry is
      record
         Frame    : Rose.Addresses.Physical_Page_Address := 0;
         Original : Rose.Addresses.Physical_Page_Address := 0;
         --  The frame this one holds a copy of, or 0 if it is free.
      end record;

   Reserve      : array (Reserve_Index range 1 .. Max_Reserve)
     of Reserve_Entry;
   Reserve_Last : Reserve_Index := 0;
   Free_Count   : Natural := 0;

   --  Two bit arrays, the marks and the changed frames, with a bit for
   --  each frame up to the last frame of the user regions. Frames past it
   --  are never marked or changed.

   type Bit_Array is
     array (Rose.Addresses.Physical_Page_Address) of Boolean
     with Pack;

   type Bit_Word_Array is
     array (Rose.Addresses.Physical_Page_Address range 0 .. 2 ** 16 - 1)
     of Rose.Words.Word_32;
   --  The same bits, a word at a time, to clear them.

   Marks_Base   : System.Address := System.Null_Address;
   Changed_Base : System.Address := System.Null_Address;
   Bit_Bound    : Rose.Addresses.Physical_Page_Address := 0;
   Bit_Words    : Rose.Addresses.Physical_Page_Address := 0;
   --  Frames from 0 to Bit_Bound - 1 have a bit in each array, which take
   --  Bit_Words words each, one after the other, in the kernel's heap.

   function Get_Bit
     (Base  : System.Address;
      Frame : Rose.Addresses.Physical_Page_Address)
      return Boolean;

   procedure Set_Bit
     (Base  : System.Address;
      Frame : Rose.Addresses.Physical_Page_Address);

   type Written_Entry is
      record
         Process : Rose.Objects.Object_Id := 0;
         Page    : Rose.Addresses.Virtual_Page_Address := 0;
      end record;

   Written      : array (1 .. Rose.Limits.Checkpoint_Written_Pages)
     of Written_Entry;
   Written_Last : Natural := 0;

   Reported     : Boolean := False;

   procedure Allocate_Marks;
   --  Make the marks, the first time a checkpoint starts.

   procedure Clear_Marks;

   function Find_Copy
     (Frame : Rose.Addresses.Physical_Page_Address)
      return Reserve_Index;
   --  The reserve entry that holds Frame's copy, or 0.

   function Is_Marked
     (Frame : Rose.Addresses.Physical_Page_Address)
      return Boolean
   is (Get_Bit (Marks_Base, Frame));

   procedure Set_Mark (Frame : Rose.Addresses.Physical_Page_Address);

   procedure Release (Index : Reserve_Index);
   --  The copy in reserve entry Index is no longer needed.

   ---------------
   -- Add_Frame --
   ---------------

   procedure Add_Frame
     (Frame : Rose.Addresses.Physical_Page_Address;
      Added : out Boolean)
   is
   begin
      if Reserve_Last = Max_Reserve or else Frame = 0 then
         Added := False;
         return;
      end if;

      Reserve_Last := Reserve_Last + 1;
      Reserve (Reserve_Last) := (Frame => Frame, Original => 0);
      Free_Count := Free_Count + 1;
      Added := True;
   end Add_Frame;

   --------------------
   -- Allocate_Marks --
   --------------------

   procedure Allocate_Marks is
      use Rose.Addresses;
      Base, Bound : Physical_Page_Address;
      Top         : Physical_Page_Address := 0;
   begin
      for I in 1 .. Rose.Kernel.Physical_Memory.Get_User_Region_Count loop
         Rose.Kernel.Physical_Memory.Get_User_Region (I, Base, Bound);
         if Bound > Top then
            Top := Bound;
         end if;
      end loop;

      if Top = 0 then
         return;
      end if;

      declare
         Words : constant Physical_Page_Address := Top / 32 + 1;
         Base  : constant Virtual_Address :=
                   Rose.Kernel.Heap.Allocate
                     (Count => Virtual_Bytes (Words) * 8,
                      Align => 4);
      begin
         Marks_Base := System'To_Address (Base);
         Changed_Base :=
           System'To_Address (Base + Virtual_Address (Words) * 4);
         Bit_Words := Words;
         Bit_Bound := Top;
      end;
   end Allocate_Marks;

   -----------------
   -- Clear_Marks --
   -----------------

   procedure Clear_Marks is
      Words : Bit_Word_Array
        with Import, Address => Marks_Base;
   begin
      if Bit_Words = 0 then
         return;
      end if;

      --  Both arrays.

      for I in 0 .. 2 * Bit_Words - 1 loop
         Words (I) := 0;
      end loop;
   end Clear_Marks;

   -------------------
   -- Clear_Written --
   -------------------

   procedure Clear_Written is
   begin
      Written_Last := 0;
   end Clear_Written;

   -------------
   -- Copying --
   -------------

   function Copying return Boolean is (Active);

   ---------------
   -- Find_Copy --
   ---------------

   function Find_Copy
     (Frame : Rose.Addresses.Physical_Page_Address)
      return Reserve_Index
   is
   begin
      for I in 1 .. Reserve_Last loop
         if Reserve (I).Original = Frame then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Copy;

   -------------
   -- Get_Bit --
   -------------

   function Get_Bit
     (Base  : System.Address;
      Frame : Rose.Addresses.Physical_Page_Address)
      return Boolean
   is
      Bits : Bit_Array
        with Import, Address => Base;
   begin
      return Frame < Bit_Bound and then Bits (Frame);
   end Get_Bit;

   -----------------
   -- Get_Written --
   -----------------

   procedure Get_Written
     (Index   : Positive;
      Process : out Rose.Objects.Object_Id;
      Page    : out Rose.Addresses.Virtual_Page_Address)
   is
   begin
      Process := Written (Index).Process;
      Page := Written (Index).Page;
   end Get_Written;

   -------------------
   -- In_Checkpoint --
   -------------------

   function In_Checkpoint
     (Frame : Rose.Addresses.Physical_Page_Address)
      return Boolean
   is (Active and then not Is_Marked (Frame));

   ----------------
   -- Is_Written --
   ----------------

   function Is_Written
     (Process : Rose.Objects.Object_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Boolean
   is
      use type Rose.Objects.Object_Id;
   begin
      for Item of Written (1 .. Written_Last) loop
         if Item.Process = Process and then Item.Page = Page then
            return True;
         end if;
      end loop;
      return False;
   end Is_Written;

   ----------------
   -- Mark_Fresh --
   ----------------

   procedure Mark_Fresh (Frame : Rose.Addresses.Physical_Page_Address) is
   begin
      if Active and then not Is_Marked (Frame) then
         Release (Find_Copy (Frame));
         Set_Mark (Frame);
      end if;
   end Mark_Fresh;

   ------------------
   -- Note_Changed --
   ------------------

   procedure Note_Changed (Frame : Rose.Addresses.Physical_Page_Address) is
   begin
      if Active then
         Set_Bit (Changed_Base, Frame);
      end if;
   end Note_Changed;

   -------------------
   -- Prepare_Write --
   -------------------

   procedure Prepare_Write
     (Frame : Rose.Addresses.Physical_Page_Address;
      Ready : out Boolean)
   is
   begin
      Ready := True;

      if not In_Checkpoint (Frame)
        or else not Get_Bit (Changed_Base, Frame)
        or else Find_Copy (Frame) /= 0
      then
         return;
      end if;

      for I in 1 .. Reserve_Last loop
         if Reserve (I).Original = 0 then
            Rose.Kernel.Page_Table.Copy_Frame
              (From => Frame,
               To   => Reserve (I).Frame);
            Reserve (I).Original := Frame;
            Free_Count := Free_Count - 1;
            return;
         end if;
      end loop;

      Ready := False;
   end Prepare_Write;

   --------------------
   -- Record_Written --
   --------------------

   procedure Record_Written
     (Process  : Rose.Objects.Object_Id;
      Page     : Rose.Addresses.Virtual_Page_Address;
      Recorded : out Boolean)
   is
   begin
      if Written_Last = Written'Last then
         Recorded := False;
         return;
      end if;

      Written_Last := Written_Last + 1;
      Written (Written_Last) := (Process => Process, Page => Page);
      Recorded := True;
   end Record_Written;

   -------------
   -- Release --
   -------------

   procedure Release (Index : Reserve_Index) is
   begin
      if Index /= 0 and then Reserve (Index).Original /= 0 then
         Reserve (Index).Original := 0;
         Free_Count := Free_Count + 1;
      end if;
   end Release;

   -----------------
   -- Report_Full --
   -----------------

   procedure Report_Full (What : String) is
   begin
      if not Reported then
         Reported := True;
         Rose.Boot.Console.Put ("kernel: checkpoint ");
         Rose.Boot.Console.Put (What);
         Rose.Boot.Console.Put_Line (" full; a writer waits");
      end if;
   end Report_Full;

   -------------------
   -- Reserve_Count --
   -------------------

   function Reserve_Count return Natural is (Free_Count);

   ----------------
   -- Save_Frame --
   ----------------

   procedure Save_Frame
     (Frame : Rose.Addresses.Physical_Page_Address;
      To    : System.Address;
      Saved : out Boolean)
   is
      Copy : constant Reserve_Index := Find_Copy (Frame);
   begin
      Saved := In_Checkpoint (Frame);

      Rose.Kernel.Page_Table.Copy_Frame
        (From => (if Saved and then Copy /= 0
                  then Reserve (Copy).Frame
                  else Frame),
         To   => To);

      if Saved then
         Release (Copy);
         Set_Mark (Frame);
      end if;
   end Save_Frame;

   -------------
   -- Set_Bit --
   -------------

   procedure Set_Bit
     (Base  : System.Address;
      Frame : Rose.Addresses.Physical_Page_Address)
   is
      Bits : Bit_Array
        with Import, Address => Base;
   begin
      if Frame < Bit_Bound then
         Bits (Frame) := True;
      end if;
   end Set_Bit;

   --------------
   -- Set_Mark --
   --------------

   procedure Set_Mark (Frame : Rose.Addresses.Physical_Page_Address) is
   begin
      Set_Bit (Marks_Base, Frame);
   end Set_Mark;

   ------------------
   -- Start_Copies --
   ------------------

   procedure Start_Copies is
   begin
      if Bit_Words = 0 then
         Allocate_Marks;
      end if;

      Clear_Marks;

      for I in 1 .. Reserve_Last loop
         Release (I);
      end loop;

      Reported := False;
      Active := True;
   end Start_Copies;

   -----------------
   -- Stop_Copies --
   -----------------

   procedure Stop_Copies is
   begin
      Active := False;

      for I in 1 .. Reserve_Last loop
         Release (I);
      end loop;
   end Stop_Copies;

   -------------------
   -- Written_Count --
   -------------------

   function Written_Count return Natural is (Written_Last);

end Rose.Kernel.Checkpoint_Copies;
