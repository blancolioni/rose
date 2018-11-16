with Rose.Boot.Console;

with Rose.Kernel.Errors;
--  with Rose.Kernel.Mutex;

with Rose.Words;

package body Rose.Kernel.Physical_Memory is

   --  Mem_Lock : Mutex.Spinlock_Type;

   Max_Regions : constant := 64;

   type Region_Count is range 0 .. Max_Regions;

   subtype Maybe_Region_Index is Region_Count;

   subtype Region_Index is
     Region_Count range 1 .. Region_Count'Last;

   Alignment : constant array (Allocation_Constraint) of Physical_Address :=
     (Page_Aligned => Memory_Page_Size,
      Word_Aligned => 4,
      Byte_Aligned => 1);

   type Region_Info is
      record
         Base     : Physical_Address;
         Bound    : Physical_Address;
         Class    : Memory_Class;
         Usage    : Memory_Usage;
      end record;

   Memory_Table : array (Region_Index) of Region_Info;
   Num_Mem_Regions : Region_Count := 0;

   type User_Region_Info is
      record
         Base, Bound : Physical_Page_Address;
      end record;

   User_Memory_Table : array (Region_Index) of User_Region_Info;
   Num_User_Regions  : Region_Count := 0;

   procedure Cleanup;

   procedure New_Region (Base        : in  Physical_Address;
                         Bound       : in  Physical_Address;
                         Class       : in  Memory_Class;
                         Usage       : in  Memory_Usage);

   function New_Region (Base        : in  Physical_Address;
                        Bound       : in  Physical_Address;
                        Class       : in  Memory_Class;
                        Usage       : in  Memory_Usage)
                       return Region_Index;

   procedure Show_Region (Id : Region_Index);

   function Overlapping_Region (Base             : Physical_Address;
                                Bound            : Physical_Address;
                                Ignore_Available : Boolean := True)
                               return Maybe_Region_Index;
   --  Given a pair of physical addresses, find the first
   --  overlapping region, if any exist, including any region
   --  that contains or is fully contained by the pair

   pragma Unreferenced (Overlapping_Region);

   function Choose_Region (Size       : Physical_Address;
                           Constraint : Allocation_Constraint)
                          return Maybe_Region_Index;

   function Preferred_Region (R1, R2 : Maybe_Region_Index)
                              return Maybe_Region_Index;

   function Find_Region (Base : Physical_Address)
                         return Maybe_Region_Index;

   --------------------
   -- Allocate_Bytes --
   --------------------

   function Allocate_Bytes (Constraint  : Allocation_Constraint;
                            Size        : Physical_Address;
                            Usage       : Memory_Usage)
                           return Physical_Address
   is
      Index  : Maybe_Region_Index;
      Result : Physical_Address;
      Align  : constant Physical_Address := Alignment (Constraint);
   begin
      --  Mutex.Get_Spinlock (Mem_Lock);

      Index := Choose_Region (Size, Constraint);

      if Index = 0 then
--         Mutex.Release_Spinlock (Mem_Lock);
         return 0;
      end if;

      declare
         Mem     : Region_Info renames Memory_Table (Index);
         Success : Boolean;
         Base, Bound : Physical_Address;
      begin
         Rose.Boot.Console.Put ("Allocating from region: ");
         Show_Region (Index);
         Base := Mem.Base;
         Result := Base;
         if Result mod Align /= 0 then
            Result := Result + Align - Result mod Align;
         end if;
         Bound := Result + Size;

         Allocate_Region (Base, Bound, RAM, Usage, Success);

         if not Success then
            Errors.Fatal ("Allocating region should have worked",
                          Result, Result + Size);
         end if;
      end;

--      Mutex.Release_Spinlock (Mem_Lock);

      return Result;

   end Allocate_Bytes;

   ---------------------
   -- Allocate_Region --
   ---------------------

   procedure Allocate_Region (Base        : in  Physical_Address;
                              Bound       : in  Physical_Address;
                              Class       : in  Memory_Class;
                              Usage       : in  Memory_Usage;
                              Success     : out Boolean)
   is
      Index : Maybe_Region_Index;
   begin
      Success := True;
      if Base = Bound then
         return;
      end if;

      if Base > Bound then
         Errors.Fatal ("Physical_Memory.Allocate_Region: Base > Bound",
                       Base, Bound);
      end if;

--      Mutex.Get_Spinlock (Mem_Lock);

      Index := Find_Region (Base);
      if Index = 0 then
         Success := False;
         return;
      end if;

      declare
         Mem : Region_Info renames Memory_Table (Index);
      begin
         if Mem.Base > Base
           or else Mem.Bound < Bound
           or else Mem.Base > Bound
         then
            Rose.Boot.Console.Put ("while attempting to allocate region ");
            Rose.Boot.Console.Put (Rose.Words.Word_32 (Base));
            Rose.Boot.Console.Put (" -- ");
            Rose.Boot.Console.Put (Rose.Words.Word_32 (Bound));
            Rose.Boot.Console.New_Line;

            Rose.Boot.Console.Put ("  memory base = ");
            Rose.Boot.Console.Put (Rose.Words.Word_32 (Mem.Base));
            Rose.Boot.Console.New_Line;

            Rose.Boot.Console.Put ("  memory bound = ");
            Rose.Boot.Console.Put (Rose.Words.Word_32 (Mem.Bound));
            Rose.Boot.Console.New_Line;

            Errors.Fatal ("weird things in physical memory",
                          Mem.Base, Mem.Bound);
         end if;

         if Base /= Mem.Base then
            declare
               Old_Bound : constant Physical_Address := Mem.Bound;
            begin
               Mem.Bound := Base;
               Index := New_Region (Base, Old_Bound, Mem.Class, Mem.Usage);
            end;
         end if;
      end;

      declare
         Mem : Region_Info renames Memory_Table (Index);
      begin

         if Bound /= Mem.Bound then
            declare
               Old_Bound : constant Physical_Address := Mem.Bound;
            begin
               Mem.Bound := Bound;
               New_Region (Bound, Old_Bound, Mem.Class, Mem.Usage);
            end;
         end if;

         Mem.Usage := Usage;
         Mem.Class := Class;
      end;

      Cleanup;

--      Mutex.Release_Spinlock (Mem_Lock);

      Success := True;

   end Allocate_Region;

   ----------------------
   -- Available_Memory --
   ----------------------

   function Available_Memory
     (Unit_Size   : in Physical_Address;
      Contiguous  : in Boolean)
      return Physical_Address
   is
      Units            : Physical_Address := 0;
      Contiguous_Units : Physical_Address := 0;
   begin
--      Mutex.Get_Spinlock (Mem_Lock);

      for I in 1 .. Num_Mem_Regions loop
         declare
            Mem : Region_Info renames Memory_Table (I);
            Base, Bound : Physical_Address;
         begin
            if Mem.Class = RAM
              and then Mem.Usage = Available
            then
               Base  := Align_Up_To_Unit (Mem.Base, Unit_Size);
               Bound := Align_Down_To_Unit (Mem.Bound, Unit_Size);
               if Base < Bound then
                  declare
                     Units_Here : constant Physical_Address :=
                       (Bound - Base) / Unit_Size;
                  begin
                     if Units_Here > Contiguous_Units then
                        Contiguous_Units := Units_Here;
                     end if;
                     Units := Units + Units_Here;
                  end;
               end if;
            end if;
         end;
      end loop;
      if Contiguous then
         return Contiguous_Units;
      else
         return Units;
      end if;
   end Available_Memory;

   -------------------
   -- Choose_Region --
   -------------------

   function Choose_Region
     (Size       : Physical_Address;
      Constraint : Allocation_Constraint)
      return Maybe_Region_Index
   is
      Result : Maybe_Region_Index := 0;
   begin
      for I in 1 .. Num_Mem_Regions loop
         declare
            Mem : Region_Info renames Memory_Table (I);
         begin
            if Mem.Class = RAM
              and then Mem.Usage = Available
            then
               declare
                  Base  : Physical_Address := Mem.Base;
                  Bound : constant Physical_Address := Mem.Bound;
                  Align : constant Physical_Address := Alignment (Constraint);
               begin
                  if Base mod Align /= 0 then
                     Base := Base + Align - (Base mod Align);
                  end if;
                  if Base < Bound then
                     if Bound - Size - ((Bound - Size) mod Align) >= Base then
                        Result := Preferred_Region (Result, I);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      return Result;
   end Choose_Region;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup is
   begin
      for I in 1 .. Num_Mem_Regions loop
         for J in I + 1 .. Num_Mem_Regions loop
            if Memory_Table (J).Base < Memory_Table (I).Base then
               declare
                  T : constant Region_Info := Memory_Table (J);
               begin
                  Memory_Table (J) := Memory_Table (I);
                  Memory_Table (I) := T;
               end;
            end if;
         end loop;
      end loop;
   end Cleanup;

   ------------------------------
   -- Find_Available_RAM_Pages --
   ------------------------------

   procedure Find_Available_RAM_Pages
     (From     : in  Physical_Page_Address;
      Start    : out Physical_Page_Address;
      Bound    : out Physical_Page_Address;
      Finished : out Boolean)
   is
   begin
      for I in 1  .. Num_Mem_Regions loop
         declare
            M : Region_Info renames Memory_Table (I);
         begin
            if Physical_Address_To_Page (M.Base) > From
              and then M.Class = Unused
            then
               Start    := Physical_Address_To_Page (M.Base);
               Bound    := Physical_Address_To_Page (M.Bound);
               Finished := False;
               return;
            end if;
         end;
      end loop;
      Finished := True;
   end Find_Available_RAM_Pages;

   -----------------
   -- Find_Region --
   -----------------

   function Find_Region
     (Base : Physical_Address)
      return Maybe_Region_Index
   is
   begin
      for I in 1 .. Num_Mem_Regions loop
         if Memory_Table (I).Base <= Base
           and then Memory_Table (I).Bound > Base
         then
            return I;
         elsif Memory_Table (I).Base > Base then
            return 0;
         end if;
      end loop;
      return 0;
   end Find_Region;

   ----------------
   -- Get_Region --
   ----------------

   procedure Get_User_Region
     (Index : Positive;
      Base  : out Rose.Addresses.Physical_Page_Address;
      Bound : out Rose.Addresses.Physical_Page_Address)
   is
      Region : constant Region_Index := Region_Index (Index);
   begin
      if Region <= Num_User_Regions then
         declare
            Info : User_Region_Info renames User_Memory_Table (Region);
         begin
            Base := Info.Base;
            Bound := Info.Bound;
         end;
      else
         Base  := 0;
         Bound := 0;
      end if;
   end Get_User_Region;

   ----------------------
   -- Get_Region_Count --
   ----------------------

   function Get_User_Region_Count
     return Natural
   is
   begin
      if Num_User_Regions = 0 then
         for I in 1 .. Num_Mem_Regions loop
            declare
               Info : Region_Info renames Memory_Table (I);
            begin
               if Info.Usage = Available
                 and then Info.Class = RAM
               then
                  declare
                     Base : constant Physical_Page_Address :=
                              Physical_Address_To_Page
                                (Align_Up_To_Page_Boundary
                                   (Info.Base));
                     Bound : constant Rose.Addresses.Physical_Page_Address :=
                               Rose.Addresses.Physical_Address_To_Page
                                 (Info.Bound);
                  begin
                     if Bound - Base > 1 then
                        Num_User_Regions := Num_User_Regions + 1;
                        User_Memory_Table (Num_User_Regions) :=
                          User_Region_Info'
                            (Base  => Base,
                             Bound => Bound);
                     end if;
                  end;
               end if;
            end;
         end loop;
      end if;

      return Natural (Num_User_Regions);
   end Get_User_Region_Count;

   --------------------------
   -- Init_Physical_Memory --
   --------------------------

   procedure Init_Physical_Memory (Base  : Physical_Address;
                                   Bound : Physical_Address)
   is
   begin
      if Bound = 0 or else Base >= Bound then
         Kernel.Errors.Fatal ("Init_Physical_Memory: bad base/bound",
                              Base, Bound);
      end if;

--      Mutex.Initialise_Spinlock (Mem_Lock);
      New_Region (Base, Bound, Addressable, Available);
   end Init_Physical_Memory;

   ----------------
   -- New_Region --
   ----------------

   procedure New_Region (Base        : in  Physical_Address;
                         Bound       : in  Physical_Address;
                         Class       : in  Memory_Class;
                         Usage       : in  Memory_Usage)
   is
      Mem : constant Maybe_Region_Index :=
              New_Region (Base, Bound, Class, Usage);
      pragma Unreferenced (Mem);
   begin
      null;
   end New_Region;

   ----------------
   -- New_Region --
   ----------------

   function New_Region (Base        : in  Physical_Address;
                        Bound       : in  Physical_Address;
                        Class       : in  Memory_Class;
                        Usage       : in  Memory_Usage)
                       return Region_Index
   is
   begin
--      Mutex.Get_Spinlock (Mem_Lock);

      if Num_Mem_Regions = Max_Regions then
         Errors.Fatal ("Physical region list exhausted",
                       Rose.Words.Word_32 (Num_Mem_Regions));
      end if;

      Num_Mem_Regions := Num_Mem_Regions + 1;

      declare
         Mem : Region_Info renames Memory_Table (Num_Mem_Regions);
      begin

         Mem.Base  := Base;
         Mem.Bound := Bound;
         Mem.Class := Class;
         Mem.Usage := Usage;
      end;

      Cleanup;

--      Mutex.Release_Spinlock (Mem_Lock);

      --  Show_Regions;

      return Find_Region (Base);

   end New_Region;

   ------------------------
   -- Overlapping_Region --
   ------------------------

   function Overlapping_Region
     (Base             : Physical_Address;
      Bound            : Physical_Address;
      Ignore_Available : Boolean := True)
      return Maybe_Region_Index
   is
   begin
--      Mutex.Get_Spinlock (Mem_Lock);

      for I in 1 .. Num_Mem_Regions loop
         if Memory_Table (I).Class /= Unused
           and then (not Ignore_Available
                     or else Memory_Table (I).Usage /= Available)
         then
            if (Memory_Table (I).Base <= Base and then
                  Base < Memory_Table (I).Bound) or else
              (Memory_Table (I).Base < Bound and then
               Bound <= Memory_Table (I).Bound) or else
              (Base <= Memory_Table (I).Base and then
                 Bound >= Memory_Table (I).Bound)
            then
               return I;
            end if;
         end if;
      end loop;

--      Mutex.Release_Spinlock (Mem_Lock);

      return 0;
   end Overlapping_Region;

   ----------------------
   -- Preferred_Region --
   ----------------------

   function Preferred_Region
     (R1, R2 : Maybe_Region_Index)
      return Maybe_Region_Index
   is
   begin
      if R1 = 0 then
         return R2;
      elsif R2 = 0 then
         return R1;
      elsif Memory_Table (R1).Base < Memory_Table (R2).Base then
         return R2;
      else
         return R1;
      end if;
   end Preferred_Region;

   -----------------
   -- Show_Region --
   -----------------

   procedure Show_Region (Id : Region_Index) is
      use Rose.Boot.Console;
   begin
      Put ("[");
      Put (Memory_Table (Id).Base);
      Put (",");
      Put (Memory_Table (Id).Bound);
      Put ("] ");
      case Memory_Table (Id).Class is
         when Unused =>
            Put ("unused ");
         when Addressable =>
            Put ("PADRES ");
         when RAM =>
            Put ("RAM    ");
         when NVRAM =>
            Put ("NVRAM  ");
         when ROM =>
            Put ("ROM    ");
         when Device =>
            Put ("Device ");
      end case;

      case Memory_Table (Id).Usage is
         when Available =>
            Put ("Available  ");
         when BIOS =>
            Put ("BIOS       ");
         when Kernel_Mem =>
            Put ("KERNEL     ");
         when Initial_System_Image =>
            Put ("InitSysImg ");
         when Kernel_Heap =>
            Put ("Kern Heap  ");
         when Page_Table =>
            Put ("Page Table ");
         when Process_Table =>
            Put ("Process Tbl");
         when Device_Owned =>
            Put ("Devices    ");
         when Device_Pages =>
            Put ("Device pgs ");

      end case;

      Rose.Boot.Console.New_Line;

   end Show_Region;

   ------------------
   -- Show_Regions --
   ------------------

   procedure Show_Regions is
   begin
--      Mutex.Get_Spinlock (Mem_Lock);
      Rose.Boot.Console.Put_Line ("Current memory map");
      for I in 1 .. Num_Mem_Regions loop
         Show_Region (I);
      end loop;
   end Show_Regions;

end Rose.Kernel.Physical_Memory;
