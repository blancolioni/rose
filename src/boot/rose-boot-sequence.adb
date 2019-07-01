with Rose.Boot.Check;
with Rose.Boot.Console;
with Rose.Boot.Memory;
with Rose.Kernel.Init;

package body Rose.Boot.Sequence is

   type Boot_Item is access procedure;

   type Boot_Info is
      record
         Item_Name : access String;
         Item_Proc : Boot_Item;
      end record;

   Console_Item      : aliased String := "Console";
   Check_Item        : aliased String := "Check";
   Memory_Item       : aliased String := "Memory";
   Kernel_Item       : aliased String := "Kernel";

   Boot_Item_List : constant array (Positive range <>) of Boot_Info :=
                  ((Item_Name => Console_Item'Access,
                    Item_Proc => Rose.Boot.Console.Init_Boot_Console'Access),
                   (Item_Name => Check_Item'Access,
                    Item_Proc => Rose.Boot.Check.Check_Boot_Loader'Access),
                   (Item_Name => Memory_Item'Access,
                    Item_Proc => Rose.Boot.Memory.Configure_Memory'Access),
                   (Item_Name => Kernel_Item'Access,
                    Item_Proc => Rose.Kernel.Init.Init_Kernel'Access)
                  );

   ----------------------------
   --  Execute_Boot_Sequence --
   ----------------------------

   procedure Execute_Boot_Sequence is
   begin
      for I in Boot_Item_List'Range loop
         Rose.Boot.Console.Put ("Init: ");
         Rose.Boot.Console.Put_Line (Boot_Item_List (I).Item_Name.all);
         Boot_Item_List (I).Item_Proc.all;
      end loop;
--        Rose.Boot.Console.Put_Line ("Initial boot sequence complete");
--        Rose.Boot.Console.Put_Line ("Ready to test keyboard interrupts");
--        Rose.Boot.Console.Put ("petal:/> ");
   end Execute_Boot_Sequence;

end Rose.Boot.Sequence;
