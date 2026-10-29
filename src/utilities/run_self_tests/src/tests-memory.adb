with Ada.Text_IO;

with System;
with System.Storage_Elements;

package body Tests.Memory is

   use System.Storage_Elements;

   function Copy
     (Dest   : System.Address;
      Source : System.Address;
      Size   : Storage_Count)
      return System.Address
     with Import, Convention => C, External_Name => "memcpy";

   ---------------
   -- Copy_Test --
   ---------------

   function Copy_Test return Boolean is
      Guard  : constant Storage_Element := 16#5A#;
      Source : constant Storage_Array (1 .. 16) := (others => 16#A5#);
      Target : Storage_Array (1 .. 17) := (others => Guard);
      Result : System.Address with Unreferenced;
   begin
      Ada.Text_IO.Put ("Memory copy test");

      for Length in Storage_Count range 0 .. 16 loop
         Target := (others => Guard);
         Result := Copy (Target'Address, Source'Address, Length);
         if Target (1 .. Length) /= Source (1 .. Length)
           or else Target (Length + 1) /= Guard
         then
            return False;
         end if;
      end loop;

      return True;
   end Copy_Test;

end Tests.Memory;
