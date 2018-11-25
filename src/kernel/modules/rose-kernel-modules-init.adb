with Rose.Boot.Console;
with Rose.Multiboot;
with Rose.Words;

with Rose.Kernel.Errors;
with Rose.Kernel.Physical_Memory;

package body Rose.Kernel.Modules.Init is

   procedure Process_Module
     (Mod_Start  : Rose.Words.Word_32;
      Mod_End    : Rose.Words.Word_32;
      Mod_Text   : String);

   ------------------
   -- Load_Modules --
   ------------------

   procedure Load_Modules is
   begin
      Rose.Multiboot.Scan_Modules (Process_Module'Access);
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
