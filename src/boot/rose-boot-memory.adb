with Rose.Addresses;
with Rose.Boot.Console;
with Rose.Kernel.Command_Line;
with Rose.Kernel.Modules;
with Rose.Kernel.Physical_Memory;
with Rose.Multiboot;
with Rose.Words;                       use Rose.Words;

package body Rose.Boot.Memory is

   Report_Memory_Allocation : constant Boolean := True;

   procedure Process_Multiboot_Info;
   --  Record the physical memory locations of modules and command line.
   --  This ensures that they are not allocated by the physical memory
   --  allocator as we build the heap later

   procedure Configure_Physical_Memory;
   --  Initialise physical memory -- multiboot-specific.

   procedure Process_Memory_Map;
   --  Process the multiboot-supplied memory map

   ----------------------
   -- Configure_Memory --
   ----------------------

   procedure Configure_Memory is
      use Rose.Addresses;
      Base, Bound : Physical_Address;
   begin
      Base := 0;
      Bound := Physical_Address'Last - 3;

      Kernel.Physical_Memory.Init_Physical_Memory
        (Base, Bound);

      Configure_Physical_Memory;
   end Configure_Memory;

   -------------------------------
   -- Configure_Physical_Memory --
   -------------------------------

   procedure Configure_Physical_Memory is
   begin
      --  Apparently QEMU doesn't provide all the memory layout
      --  information, but maybe BOCHS does.  It will be a while
      --  before anything runs on real hardware though.

      if Rose.Multiboot.Have_Physical_Memory_Range then
         Rose.Boot.Console.Put ("Physical memory: ");
         Rose.Boot.Console.Put
           (1024 * Rose.Multiboot.Physical_Memory_Low);
         Rose.Boot.Console.Put (" .. ");
         Rose.Boot.Console.Put
           (1024 * Rose.Multiboot.Physical_Memory_High);
         Rose.Boot.Console.New_Line;
      end if;

      if Rose.Multiboot.Have_Memory_Map then
         Rose.Boot.Console.Put_Line ("Memory map valid");
         Process_Memory_Map;
      else
         Rose.Boot.Console.Put_Line ("Did not get physical memory map");
      end if;

   end Configure_Physical_Memory;

   ------------------------
   -- Process_Memory_Map --
   ------------------------

   procedure Process_Memory_Map is
      use Rose.Addresses;
      use Rose.Multiboot;

      procedure Add_Region
        (Available : Boolean;
         Low       : Rose.Words.Word_64;
         High      : Rose.Words.Word_64);

      ----------------
      -- Add_Region --
      ----------------

      procedure Add_Region
        (Available : Boolean;
         Low       : Rose.Words.Word_64;
         High      : Rose.Words.Word_64)
      is
         Usage : constant Kernel.Physical_Memory.Memory_Usage :=
           (if Available
              then Kernel.Physical_Memory.Available
              else Kernel.Physical_Memory.BIOS);

      begin

         if not Available then
            return;
         end if;

         if Report_Memory_Allocation then
            Rose.Boot.Console.Put ("Allocating ");
            if Available then
               Rose.Boot.Console.Put ("RAM");
            else
               Rose.Boot.Console.Put ("reserved");
            end if;

            Rose.Boot.Console.Put (" region: ");
            Rose.Boot.Console.Put (Word_32 (Low / 2 ** 32));
            Rose.Boot.Console.Put (":");
            Rose.Boot.Console.Put (Word_32 (Low mod 2 ** 32));
            Rose.Boot.Console.Put (" ");
            Rose.Boot.Console.Put (Word_32 (High / 2 ** 32));
            Rose.Boot.Console.Put (":");
            Rose.Boot.Console.Put (Word_32 (High mod 2 ** 32));
            Rose.Boot.Console.New_Line;
         end if;

         if High < Word_64 (Physical_Address'Last) then
            declare
               Success : Boolean;
            begin
               Kernel.Physical_Memory.Allocate_Region
                 (Base     => Physical_Address (Low mod 2 ** 32),
                  Bound    => Physical_Address (High mod 2 ** 32),
                  Class    => Kernel.Physical_Memory.RAM,
                  Usage    => Usage,
                  Success  => Success);

               if not Success then
                  Rose.Boot.Console.Put ("Could not allocate region at");
                  Rose.Boot.Console.Put (Word_32 (Low mod 2 ** 32));
                  Rose.Boot.Console.Put ("-");
                  Rose.Boot.Console.Put (Word_32 (High mod 2 ** 32));
                  Rose.Boot.Console.Put (" as ");
                  if Available then
                     Rose.Boot.Console.Put_Line ("RAM");
                  else
                     Rose.Boot.Console.Put_Line ("BIOS");
                  end if;
               end if;
            end;
         else
            Rose.Boot.Console.Put_Line ("Ignoring region");
         end if;

      end Add_Region;

   begin
      Scan_Memory_Map (Add_Region'Access);
   end Process_Memory_Map;

   ----------------------------
   -- Process_Multiboot_Info --
   ----------------------------

   procedure Process_Multiboot_Info is
   begin
      --  Let's assume we're an ELF image.  In this case, no problem.
      null;

      --  Save the command line
      Rose.Kernel.Command_Line.Initialise_Command_Line;

   end Process_Multiboot_Info;

   ---------------------
   -- Protect_Modules --
   ---------------------

   procedure Protect_Modules is
   begin
      Process_Multiboot_Info;
      Kernel.Modules.Relocate;
   end Protect_Modules;

end Rose.Boot.Memory;
