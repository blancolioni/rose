with System;

with Rose.Boot.Sequence;
with Rose.Boot.Console;
with Rose.Version;

with Rose.Multiboot;

package body Rose.Main is

   Starting         : constant String := "Starting ";

   procedure Last_Chance_Handler
     (Source_Location : System.Address;
      Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

   -----------------
   -- Kernel_Main --
   -----------------

   procedure Kernel_Main is
   begin

      Rose.Boot.Console.Clear;
      Rose.Boot.Console.Put (Starting);
      Rose.Boot.Console.Put_Line (Rose.Version.Full_Name);

      Rose.Multiboot.Load_Multiboot_Header;

      Rose.Boot.Sequence.Execute_Boot_Sequence;

   end Kernel_Main;

   procedure Last_Chance_Handler
     (Source_Location : System.Address;
      Line : Integer)
   is
      pragma Unreferenced (Source_Location, Line);
   begin
      Rose.Boot.Console.Put_Line ("Unhandled exception in kernel");
      loop
         null;
      end loop;
   end Last_Chance_Handler;

end Rose.Main;
