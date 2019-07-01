with Rose.Multiboot;

package body Rose.Boot.Check is

   -----------------------
   -- Check_Boot_Loader --
   -----------------------

   procedure Check_Boot_Loader is
   begin
      Rose.Multiboot.Check_Magic;
   end Check_Boot_Loader;

end Rose.Boot.Check;
