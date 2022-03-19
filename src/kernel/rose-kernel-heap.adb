with System.Storage_Elements;

with Rose.Boot.Console;

with Rose.Words;
with Rose.Kernel.Page_Table;
with Rose.Kernel.Panic;
with Rose.Kernel.Physical_Memory;

with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Heap is

   Log_Heap : constant Boolean := False;

   Heap_Start : Virtual_Address;
   Heap_Bound : Virtual_Address;
   Heap_End   : Virtual_Address;

   Next_IPC_Buffer : Virtual_Page_Address :=
                       16#E_0000#;

   type Heap_Region_Page_Count is mod 2 ** 14;

   type Heap_Region_Record is
      record
         Physical_Base  : Physical_Page_Address  := 0;
         Virtual_Base   : Virtual_Page_Address   := 0;
         Page_Count     : Heap_Region_Page_Count := 0;
      end record
   with Pack, Size => 64;

   Max_Heap_Regions : constant := 4096 * 8 / 64;
   type Heap_Region_Index is mod Max_Heap_Regions;

   type Heap_Region_Array is array (Heap_Region_Index) of Heap_Region_Record
     with Pack, Size => 4096 * 8;

   Heap_Regions        : Heap_Region_Array;
   Next_Region         : Heap_Region_Index := 0;
   Initial_Physical_Bound : Physical_Page_Address;
   Initial_Virtual_Bound  : Virtual_Page_Address;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Count : Virtual_Bytes;
      Align : Virtual_Bytes := 4)
     return Virtual_Address
   is
      Original_Start : constant Virtual_Address := Heap_End;
      Result         : constant Virtual_Address :=
                         Align_Up_To_Unit (Heap_End, Align);
   begin

      Heap_End := Align_Up_To_Unit (Heap_End + Count, Align);

      if Log_Heap then
         Rose.Boot.Console.Put ("Heap: allocating ");
         Rose.Boot.Console.Put (Physical_Address (Count));
         Rose.Boot.Console.Put (" bytes at ");
         Rose.Boot.Console.Put (Get_Physical_Address (Result));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (Rose.Words.Word (Result));
         Rose.Boot.Console.New_Line;
      end if;

      if Heap_End >= Heap_Bound then
         Kernel.Panic.Panic ("Kernel heap exhausted ", Heap_Bound);
      end if;

      declare
         use System.Storage_Elements;
         Addr  : constant System.Address := System'To_Address (Original_Start);
         Block : Storage_Array
           (1 .. Storage_Offset (Heap_End - 1 - Original_Start));
         for Block'Address use Addr;
         pragma Import (Ada, Block);
      begin
         Block := (others => 0);
      end;
      return Result;
   end Allocate;

   -------------------
   -- Allocate_Page --
   -------------------

   function Allocate_Page return Virtual_Page_Address is
   begin
      Heap_End := Align_Up_To_Unit (Heap_End, Virtual_Page_Bytes);
      return Virtual_Address_To_Page (Allocate (Virtual_Page_Bytes));
   end Allocate_Page;

   function Allocate_Page return Physical_Page_Address
   is (Get_Physical_Page (Allocate_Page));

   --------------------
   -- Available_Heap --
   --------------------

   function Available_Heap return Physical_Bytes is
   begin
      return Physical_Bytes (Heap_Bound - Heap_End);
   end Available_Heap;

   ----------
   -- Free --
   ----------

   procedure Free (Addr : Virtual_Address) is
      pragma Unreferenced (Addr);   --  ummmaaaaaaaa
   begin
      null;    --  UMMMMMAAAAAAAAAAAAA
   end Free;

   --------------------------
   -- Get_Physical_Address --
   --------------------------

   function Get_Physical_Address
     (Virtual : Virtual_Address)
      return Physical_Address
   is
      Virtual_Page : constant Virtual_Page_Address :=
                       Virtual_Address_To_Page (Virtual);
      Physical_Page : constant Physical_Page_Address :=
                        Get_Physical_Page (Virtual_Page);
   begin
      return Physical_Page_To_Address (Physical_Page)
        + Physical_Address (Virtual - Virtual_Page_To_Address (Virtual_Page));
   end Get_Physical_Address;

   -----------------------
   -- Get_Physical_Page --
   -----------------------

   function Get_Physical_Page
     (Virtual_Page : Virtual_Page_Address)
      return Physical_Page_Address
   is
      Physical : Physical_Page_Address := 16#DEAD_0#;

   begin
      if Next_Region = 0
        or else Virtual_Page < Initial_Virtual_Bound
      then
         Physical :=
           Physical_Page_Address
             (Virtual_Page - Kernel_Virtual_Page_Base);
      else
         for Index in 0 .. Next_Region - 1 loop
            declare
               R : Heap_Region_Record renames Heap_Regions (Index);
               Base  : constant Virtual_Page_Address :=
                         R.Virtual_Base;
               Bound : constant Virtual_Page_Address :=
                         Base + Virtual_Page_Address (R.Page_Count);
            begin
               if Virtual_Page in Base .. Bound - 1 then
                  Physical :=
                    Physical_Page_Address (Virtual_Page - Base)
                    + R.Physical_Base;
                  exit;
               end if;
            end;
         end loop;
      end if;

      return Physical;

   end Get_Physical_Page;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Allocated : out Physical_Bytes;
      Available : out Physical_Bytes)
   is
   begin
      Allocated := Physical_Bytes (Heap_End - Heap_Start);
      Available := Physical_Bytes (Heap_Bound - Heap_End);
   end Get_Status;

   -------------------------
   -- Get_Virtual_Address --
   -------------------------

   function Get_Virtual_Address
     (Physical : Physical_Address)
      return Virtual_Address
   is
      Physical_Page : constant Physical_Page_Address :=
                        Physical_Address_To_Page (Physical);
      Virtual_Page  : constant Virtual_Page_Address :=
                        Get_Virtual_Page (Physical_Page);
   begin
      return Virtual_Page_To_Address (Virtual_Page)
        + Virtual_Address
        (Physical - Physical_Page_To_Address (Physical_Page));
   end Get_Virtual_Address;

   ----------------------
   -- Get_Virtual_Page --
   ----------------------

   function Get_Virtual_Page
     (Physical_Page : Physical_Page_Address)
      return Virtual_Page_Address
   is
      Virtual : Virtual_Page_Address := 16#DEAD_0#;

   begin
      if Next_Region = 0
        or else Physical_Page < Initial_Physical_Bound
      then
         Virtual :=
           Virtual_Page_Address
             (Physical_Page + Kernel_Virtual_Page_Base);
      else
         for Index in 0 .. Next_Region - 1 loop
            declare
               R     : Heap_Region_Record renames Heap_Regions (Index);
               Base  : constant Physical_Page_Address :=
                         R.Physical_Base;
               Bound : constant Physical_Page_Address :=
                         Base + Physical_Page_Address (R.Page_Count);
            begin
               if Physical_Page in Base .. Bound - 1 then
                  Virtual :=
                    Virtual_Page_Address (Physical_Page - Base)
                    + R.Virtual_Base;
                  exit;
               end if;
            end;
         end loop;
      end if;

      return Virtual;

   end Get_Virtual_Page;

   -------------------------
   -- Increase_Heap_Bound --
   -------------------------

   procedure Increase_Heap_Bound
     (Start  : Physical_Address;
      Amount : Physical_Bytes)
   is
      use Rose.Words;
      Base_Virtual_Page : constant Virtual_Page_Address :=
                            Virtual_Address_To_Page (Heap_Bound);
      Bound_Virtual_Page : constant Virtual_Page_Address :=
                             Virtual_Address_To_Page
                               (Align_Up_To_Page_Boundary
                                  (Heap_Bound + Virtual_Bytes (Amount)));
      Physical_Page      : Physical_Page_Address :=
                             Physical_Address_To_Page (Start);
      Region_Index       : constant Heap_Region_Index := Next_Region;
   begin

      if Amount = 0 then
         Rose.Boot.Console.Put_Line ("request to increase heap bound by 0");
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id);
         return;
      end if;

      Next_Region := Next_Region + 1;

      Heap_Regions (Region_Index) :=
        Heap_Region_Record'
          (Physical_Base => Physical_Page,
           Virtual_Base  => Base_Virtual_Page,
           Page_Count    =>
             Heap_Region_Page_Count (Bound_Virtual_Page - Base_Virtual_Page));

      for Page in Base_Virtual_Page .. Bound_Virtual_Page - 1 loop
         Rose.Kernel.Page_Table.Map_Kernel_Page
           (Virtual_Page  => Page,
            Physical_Page => Physical_Page,
            Readable      => True,
            Writable      => True,
            Executable    => False,
            User          => False);
         Physical_Page := Physical_Page + 1;
      end loop;
      Rose.Boot.Console.Put ("kernel: added ");
      Rose.Boot.Console.Put (Natural (Amount) / 1024);
      Rose.Boot.Console.Put ("K starting at ");
      Rose.Boot.Console.Put (Rose.Words.Word (Start));
      Rose.Boot.Console.Put (" physical ");
      Rose.Boot.Console.Put (Rose.Words.Word (Heap_Bound));
      Rose.Boot.Console.Put (" virtual");
      Rose.Boot.Console.New_Line;
      Rose.Boot.Console.Put ("kernel: region=");
      Rose.Boot.Console.Put (Natural (Region_Index));
      Rose.Boot.Console.Put (" virt base=");
      Rose.Boot.Console.Put
        (Rose.Words.Word (Heap_Regions (Region_Index).Virtual_Base) * 4096);
      Rose.Boot.Console.Put (" phys base=");
      Rose.Boot.Console.Put
        (Rose.Words.Word (Heap_Regions (Region_Index).Physical_Base) * 4096);
      Rose.Boot.Console.Put (" page count=");
      Rose.Boot.Console.Put
        (Rose.Words.Word (Heap_Regions (Region_Index).Page_Count));
      Rose.Boot.Console.Put (" virt bound=");
      Rose.Boot.Console.Put
        (Rose.Words.Word (Heap_Regions (Region_Index).Virtual_Base) * 4096
         + Rose.Words.Word (Heap_Regions (Region_Index).Page_Count) * 4096);
      Rose.Boot.Console.Put (" phys bound=");
      Rose.Boot.Console.Put
        (Rose.Words.Word (Heap_Regions (Region_Index).Physical_Base) * 4096
         + Rose.Words.Word (Heap_Regions (Region_Index).Page_Count) * 4096);
      Rose.Boot.Console.New_Line;

      Heap_Bound := Virtual_Page_To_Address (Bound_Virtual_Page);
   end Increase_Heap_Bound;

   ---------------------
   -- Initialise_Heap --
   ---------------------

   procedure Initialise_Heap (Required : Physical_Bytes) is
--        Phys_Addr : Physical_Address;
      Heap_Phys_Start : Physical_Address;
      Heap_Phys_Bound : Physical_Address;
   begin

      --  assume addresses up to 4M are identity mapped, and that
      --  the Kernel is << 4M in size
      Heap_Phys_Start :=
        Physical_Memory.Allocate_Bytes
        (Constraint => Physical_Memory.Page_Aligned,
         Size       => Required,
         Usage      => Physical_Memory.Kernel_Mem);

      Heap_Start :=
        Virtual_Address (Heap_Phys_Start) + Kernel_Virtual_Base;
      Heap_Phys_Bound :=
        Align_Up_To_Page_Boundary (Heap_Phys_Start + Required);
      Heap_Bound :=
        Virtual_Address (Heap_Phys_Bound) + Kernel_Virtual_Base;

      Initial_Physical_Bound := Physical_Address_To_Page (Heap_Phys_Bound);
      Initial_Virtual_Bound  := Virtual_Address_To_Page (Heap_Bound);

      Rose.Boot.Console.Put ("   heap start: ");
      Rose.Boot.Console.Put (Heap_Phys_Start);
      Rose.Boot.Console.Put (" (");
      Rose.Boot.Console.Put (Heap_Start);
      Rose.Boot.Console.Put (")");
      Rose.Boot.Console.New_Line;

      Rose.Boot.Console.Put ("   heap bound: ");
      Rose.Boot.Console.Put (Heap_Phys_Bound);
      Rose.Boot.Console.Put (" (");
      Rose.Boot.Console.Put (Heap_Bound);
      Rose.Boot.Console.Put (")");
      Rose.Boot.Console.New_Line;

      Rose.Boot.Console.New_Line;

      Heap_End := Heap_Start;

   end Initialise_Heap;

   ------------------
   -- Pop_IPC_Page --
   ------------------

   procedure Pop_IPC_Page is
   begin
      Next_IPC_Buffer := Next_IPC_Buffer - 1;
   end Pop_IPC_Page;

   -------------------
   -- Push_IPC_Page --
   -------------------

   function Push_IPC_Page return Rose.Addresses.Virtual_Page_Address is
   begin
      return Result : constant Rose.Addresses.Virtual_Page_Address :=
        Next_IPC_Buffer
      do
         Next_IPC_Buffer := Next_IPC_Buffer + 1;
      end return;
   end Push_IPC_Page;

end Rose.Kernel.Heap;
