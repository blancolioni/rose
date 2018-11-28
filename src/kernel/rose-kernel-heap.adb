with System.Storage_Elements;

with Rose.Boot.Console;

--  with Rose.Kernel.Page_Table;
with Rose.Kernel.Panic;
with Rose.Kernel.Physical_Memory;

package body Rose.Kernel.Heap is

   Log_Heap : constant Boolean := False;

   Heap_Start : Virtual_Address;
   Heap_Bound : Virtual_Address;
   Heap_End   : Virtual_Address;

   Next_IPC_Buffer : Virtual_Page_Address :=
                       16#E_0000#;

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
         Rose.Boot.Console.Put (Physical_Address (Result));
         Rose.Boot.Console.New_Line;
      end if;

      if Heap_End >= Heap_Bound then
         Kernel.Panic.Panic ("Kernel heap exhausted ", Heap_Bound);
      end if;

      --  Make sure we're mapped
--        for I in Virtual_Address_To_Page (Result) ..
--          Virtual_Address_To_Page (Heap_End)
--        loop
--           Rose.Boot.Console.Put ("Mapping: ");
--           Rose.Boot.Console.Put (Physical_Address (I));
--           Rose.Boot.Console.Put (" to ");
--           Rose.Boot.Console.Put (Physical_Address (I - 16#C_0000#));
--           Rose.Boot.Console.New_Line;

--           Page_Table.Map_Page (Physical_Page_Address (I - 16#C_0000#),
--                                I, True, True, False, False);
--        end loop;

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
   is (Physical_Page_Address (Virtual_Page_Address'(Allocate_Page)
                              - Kernel_Virtual_Page_Base));

   ----------
   -- Free --
   ----------

   procedure Free (Addr : Virtual_Address) is
      pragma Unreferenced (Addr);   --  ummmaaaaaaaa
   begin
      null;    --  UMMMMMAAAAAAAAAAAAA
   end Free;

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

      Heap_Start := To_Kernel_Virtual_Address (Heap_Phys_Start);
      Heap_Phys_Bound :=
        Align_Up_To_Page_Boundary (Heap_Phys_Start + Required);
      Heap_Bound := To_Kernel_Virtual_Address (Heap_Phys_Bound);

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

      --  As long as we don't exceed 4M, we're paged in

--        if False then
--           --  Get some physical memory for ourselves
--           Phys_Addr :=
--             Physical_Memory.Allocate_Bytes
--             (Constraint => Physical_Memory.Page_Aligned,
--              Size       => Required,
--              Usage      => Physical_Memory.Kernel_Heap);
--
--           Rose.Boot.Console.Put ("   heap physical address: ");
--           Rose.Boot.Console.Put (Phys_Addr);
--           Rose.Boot.Console.New_Line;
--
--           --  Make sure we're paged in
--           declare
--              Heap_Virtual_Page_Start : constant Virtual_Page_Address :=
--                Virtual_Address_To_Page (Heap_Start);
--              Heap_Virtual_Page_End   : constant Virtual_Page_Address :=
--                Virtual_Address_To_Page (Heap_End - 1);
--              Physical_Page           : Physical_Page_Address :=
--                Physical_Address_To_Page (Phys_Addr);
--
--           begin
--              for I in Heap_Virtual_Page_Start .. Heap_Virtual_Page_End loop
--                 Page_Table.Map_Page (Physical_Page, I,
--                                      Present    => True,
--                                      Writable   => True,
--                                      Executable => False,
--                                      User       => False);
--                 Physical_Page := Physical_Page + 1;
--              end loop;
--           end;
--        end if;

--        Physical_Memory.Allocate_Region
--          (Physical_Address (Heap_Start),
--           Physical_Address (Heap_Bound),
--           Physical_Memory.RAM,
--           Physical_Memory.Kernel_Heap,
--           Success);
--        if not Success then
--           Kernel.Panic.Panic ("Cannot allocate kernel heap", Required);
--        end if;

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
