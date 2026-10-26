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

   Expected_Modules : constant String :=
                        "init console mem pci ata store isofs restore "
                        & "scan partition elf timer caps checkpoint log";
   --  init launches boot modules by position, so this has to match
   --  the module2 lines in grub.cfg and the module constants in
   --  src/servers/init/src/init-run.adb

   procedure Check_Modules;

   procedure Process_Module
     (Mod_Start  : Rose.Words.Word_32;
      Mod_End    : Rose.Words.Word_32;
      Mod_Text   : String);

   -------------------
   -- Check_Modules --
   -------------------

   procedure Check_Modules is
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
      Check_Modules;
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
