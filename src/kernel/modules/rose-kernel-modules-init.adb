with Rose.Boot.Console;
with Rose.Multiboot;
with Rose.Words;

with Rose.Kernel.Errors;
with Rose.Kernel.Panic;
with Rose.Kernel.Physical_Memory;

package body Rose.Kernel.Modules.Init is

   Boot_Mapped_Bound : Rose.Words.Word_32;
   pragma Import (C, Boot_Mapped_Bound, "boot_mapped_bound");
   --  boot.s maps physical memory below this at Kernel_Virtual_Base

   Install_Modules : constant String :=
                       "init-install console mem pci ata store isofs "
                       & "restore-install scan partition elf timer caps "
                       & "checkpoint log";
   --  The install media's images/iso/boot/grub/grub.cfg

   Disk_Modules    : constant String :=
                       "init console mem pci ata store restore "
                       & "scan partition elf timer caps checkpoint log";
   --  The boot disk's grub.cfg (#322)

   --  Init launches boot modules by position, so these have to match
   --  the modules in src/servers/init/boot.yaml, which the build writes
   --  to each grub.cfg and to init's module tables (#374).  The first
   --  module's name says which list applies (#373).

   procedure Check_Modules (Expected_Modules : String);

   procedure Process_Module
     (Mod_Start  : Rose.Words.Word_32;
      Mod_End    : Rose.Words.Word_32;
      Mod_Text   : String);

   -------------------
   -- Check_Modules --
   -------------------

   procedure Check_Modules (Expected_Modules : String) is
      Start  : Positive := Expected_Modules'First;
      Module : Module_Index := Module_Index'First;
   begin
      loop
         declare
            Finish : Positive := Start;
            Name   : String (1 .. 16);
            Last   : Natural := 0;
         begin
            while Finish < Expected_Modules'Last
              and then Expected_Modules (Finish + 1) /= ' '
            loop
               Finish := Finish + 1;
            end loop;

            if Module <= Last_Boot_Module then
               Get_Module_Name (Module, Name, Last);
            end if;

            if Name (1 .. Last) /= Expected_Modules (Start .. Finish) then
               Rose.Boot.Console.Put ("boot module ");
               Rose.Boot.Console.Put (Natural (Module));
               Rose.Boot.Console.Put (": expected ");
               Rose.Boot.Console.Put (Expected_Modules (Start .. Finish));
               Rose.Boot.Console.Put (", found ");
               if Last = 0 then
                  Rose.Boot.Console.Put_Line ("nothing");
               else
                  Rose.Boot.Console.Put_Line (Name (1 .. Last));
               end if;
               Rose.Kernel.Panic.Panic
                 ("boot modules do not match grub.cfg");
            end if;

            exit when Finish = Expected_Modules'Last;
            Start := Finish + 2;
            Module := Module + 1;
         end;
      end loop;
   end Check_Modules;

   ------------------
   -- Load_Modules --
   ------------------

   procedure Load_Modules is
   begin
      Rose.Multiboot.Scan_Modules (Process_Module'Access);
      declare
         Name : String (1 .. 16);
         Last : Natural := 0;
      begin
         if Last_Boot_Module >= Module_Index'First then
            Get_Module_Name (Module_Index'First, Name, Last);
         end if;
         if Name (1 .. Last) = "init-install" then
            Check_Modules (Install_Modules);
         else
            Check_Modules (Disk_Modules);
         end if;
      end;
   end Load_Modules;

   --------------------
   -- Process_Module --
   --------------------

   procedure Process_Module
     (Mod_Start  : Rose.Words.Word_32;
      Mod_End    : Rose.Words.Word_32;
      Mod_Text   : String)
   is
      Success : Boolean;
      Base : constant Physical_Address := Physical_Address (Mod_Start);
      Bound : constant Physical_Address :=
        Physical_Address (Mod_End) + 1;
   begin
      Rose.Boot.Console.Put ("Module: ");
      Rose.Boot.Console.Put (Mod_Start);
      Rose.Boot.Console.Put (" ");
      Rose.Boot.Console.Put (Mod_End);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put_Line (Mod_Text);

      if Bound > Physical_Address (Boot_Mapped_Bound) then
         Rose.Kernel.Panic.Panic
           ("boot module is above the boot mapping: ", Bound);
      end if;

      Add_Module (Mod_Text, Base, Bound);

      Physical_Memory.Allocate_Region
        (Base, Bound,
         Physical_Memory.RAM,
         Physical_Memory.Initial_System_Image,
         Success);

      if not Success then
         Errors.Fatal ("could not allocate module image",
                       Physical_Address (Mod_Start),
                       Physical_Address (Mod_End));
      end if;
   end Process_Module;

end Rose.Kernel.Modules.Init;
