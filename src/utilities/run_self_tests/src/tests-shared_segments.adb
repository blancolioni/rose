with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Capabilities.Standard;
with Rose.Words;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Region;
with Rose.Interfaces.Segment;

with Tests.Registry;

package body Tests.Shared_Segments is

   use Rose.Words;
   use type Rose.Capabilities.Capability;

   Page_Size : constant := 4096;
   Pages     : constant := 2;

   --  Page numbers in the window kept for shared segments (ADR 0025)

   First_Shared  : constant Word := 16#C_0000#;
   Second_Shared : constant Word := 16#C_0010#;
   Private_Copy  : constant Word := 16#C_0020#;

   Last_Index : constant := Pages * Page_Size / 4;

   type Word_Array is array (1 .. Last_Index) of Word_32
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
   -- Shared_Segment_Test --
   -------------------------

   function Shared_Segment_Test return Boolean is
      use Rose.Interfaces.Segment;
      use System.Storage_Elements;

      Second_Page : constant := Page_Size / 4 + 1;
      --  The index of the first word of the second page

      Shared_Flags : constant Word :=
                       Segment_Readable + Segment_Writable + Segment_Shared;

      function Address_Of (Page : Word) return System.Address
      is (To_Address (Integer_Address (Page) * Page_Size));

   begin
      Ada.Text_IO.Put ("Shared segment test");

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
         Self   : Rose.Interfaces.Process.Client.Process_Client :=
                    Rose.Interfaces.Process.Client.From_Cap
                      (Rose.Capabilities.Standard.Process_Interface_Cap);
         First  : Word_Array
           with Import, Address => Address_Of (First_Shared);
         Second : Word_Array
           with Import, Address => Address_Of (Second_Shared);
         Copy   : Word_Array
           with Import, Address => Address_Of (Private_Copy);
         Passed : Boolean := True;
      begin
         Self.Add_Segment
           (First_Shared, First_Shared + Pages, Region, 0, Shared_Flags);
         Self.Add_Segment
           (Second_Shared, Second_Shared + Pages, Region, 0, Shared_Flags);
         Self.Add_Segment
           (Private_Copy, Private_Copy + Pages, Region, 0,
            Segment_Readable + Segment_Writable);

         First (1) := 16#5EED_0001#;
         First (Second_Page) := 16#5EED_0002#;
         Second (2) := 16#5EED_0003#;

         if Second (1) /= 16#5EED_0001#
           or else Second (Second_Page) /= 16#5EED_0002#
         then
            Passed := Fail ("the second mapping doesn't see the first's");
         elsif First (2) /= 16#5EED_0003# then
            Passed := Fail ("the first mapping doesn't see the second's");
         elsif Copy (1) /= 0 then
            Passed := Fail ("a private mapping sees a shared write");
         end if;

         Copy (3) := 16#5EED_0004#;
         if Passed and then First (3) /= 0 then
            Passed := Fail ("a shared mapping sees a private write");
         end if;

         --  Nothing maps the pages shared now, so they are written back
         --  to the region, and the next shared mapping reads them from
         --  it.

         Self.Remove_Segment (First_Shared);
         Self.Remove_Segment (Second_Shared);
         Self.Add_Segment
           (First_Shared, First_Shared + Pages, Region, 0, Shared_Flags);

         if Passed
           and then (First (1) /= 16#5EED_0001#
                     or else First (2) /= 16#5EED_0003#
                     or else First (Second_Page) /= 16#5EED_0002#)
         then
            Passed := Fail ("the region doesn't have the shared writes");
         end if;

         Self.Remove_Segment (First_Shared);
         Self.Remove_Segment (Private_Copy);
         Region.Release;
         return Passed;
      end;
   end Shared_Segment_Test;

end Tests.Shared_Segments;
