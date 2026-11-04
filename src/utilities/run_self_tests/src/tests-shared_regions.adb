with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Region;

with Tests.Registry;

package body Tests.Shared_Regions is

   use Rose.Words;
   use type Rose.Capabilities.Capability;
   use type System.Address;

   Page_Size : constant := 4096;
   Pages     : constant := 2;

   type Word_Array is array (1 .. Pages * Page_Size / 4) of Word_32
     with Volatile_Components;

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   -------------------------
   -- Shared_Regions_Test --
   -------------------------

   function Shared_Regions_Test return Boolean is
      use System.Storage_Elements;
   begin
      Ada.Text_IO.Put ("Shared regions test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      declare
         Bank   : Rose.Interfaces.Bank.Client.Bank_Client :=
                    Rose.Interfaces.Bank.Client.From_Cap
                      (Tests.Registry.Bank);
         Region : Rose.Interfaces.Region.Region_Object'Class :=
                    Bank.Reserve_Storage (Pages * Page_Size);
         First_Address  : constant System.Address :=
                            Rose.Shared_Regions.Map (Region);
         Second_Address : constant System.Address :=
                            Rose.Shared_Regions.Map (Region);
         Size   : constant Integer_Address := Pages * Page_Size;
         First  : Word_Array with Import, Address => First_Address;
         Second : Word_Array with Import, Address => Second_Address;
         Passed : Boolean := True;
      begin
         if To_Integer (Second_Address) < To_Integer (First_Address) + Size
           and then To_Integer (First_Address) < To_Integer (Second_Address)
             + Size
         then
            Passed := Fail ("the two mappings overlap");
         else
            First (1) := 16#5EED_0011#;
            Second (Word_Array'Last) := 16#5EED_0012#;
            if Second (1) /= 16#5EED_0011#
              or else First (Word_Array'Last) /= 16#5EED_0012#
            then
               Passed := Fail ("the mappings don't share their pages");
            end if;
         end if;

         --  The first mapping's address is free again, and is the first
         --  that fits.

         Rose.Shared_Regions.Unmap (First_Address);
         declare
            Again : constant System.Address :=
                      Rose.Shared_Regions.Map (Region);
         begin
            if Passed and then Again /= First_Address then
               Passed := Fail ("an unmapped address isn't used again");
            end if;
            Rose.Shared_Regions.Unmap (Again);
         end;

         begin
            Rose.Shared_Regions.Unmap (First_Address);
            if Passed then
               Passed := Fail ("unmapping it twice doesn't raise");
            end if;
         exception
            when Rose.Shared_Regions.Mapping_Error =>
               null;
         end;

         Rose.Shared_Regions.Unmap (Second_Address);
         Region.Release;
         return Passed;
      end;
   end Shared_Regions_Test;

end Tests.Shared_Regions;
