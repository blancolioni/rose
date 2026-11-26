with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Unsaved_Memory;
with Rose.Words;

package body Tests.Unsaved_Memory is

   use Rose.Words;
   use System.Storage_Elements;
   use type System.Address;

   Page_Size : constant := 4096;
   Pages     : constant := 2;
   Size      : constant := Pages * Page_Size;

   Window_Base  : constant := 16#D000_0000#;
   Window_Bound : constant := 16#D100_0000#;

   type Word_Array is array (1 .. Size / 4) of Word_32
     with Volatile_Components;

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   function All_Zero (Words : Word_Array) return Boolean
   is (for all W of Words => W = 0);

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   -------------------------
   -- Unsaved_Memory_Test --
   -------------------------

   function Unsaved_Memory_Test return Boolean is
   begin
      Ada.Text_IO.Put ("Unsaved memory test");

      declare
         First_Address  : constant System.Address :=
                            Rose.Unsaved_Memory.Allocate (Size);
         Second_Address : constant System.Address :=
                            Rose.Unsaved_Memory.Allocate (Size - 1);
         First_At  : constant Integer_Address := To_Integer (First_Address);
         Second_At : constant Integer_Address := To_Integer (Second_Address);
         First     : Word_Array with Import, Address => First_Address;
         Second    : Word_Array with Import, Address => Second_Address;
         Passed    : Boolean := True;
      begin
         if First_At < Window_Base or else First_At + Size > Window_Bound
           or else Second_At < Window_Base
           or else Second_At + Size > Window_Bound
         then
            Passed := Fail ("a segment is outside the window");
         elsif Second_At < First_At + Size
           and then First_At < Second_At + Size
         then
            Passed := Fail ("the two segments overlap");
         elsif not All_Zero (First) or else not All_Zero (Second) then
            Passed := Fail ("a new segment isn't zeros");
         else
            First (1) := 16#5EED_1132#;
            First (Word_Array'Last) := 16#5EED_1133#;
            Second (1) := 16#5EED_1134#;
            Second (Word_Array'Last) := 16#5EED_1135#;
            if First (1) /= 16#5EED_1132#
              or else First (Word_Array'Last) /= 16#5EED_1133#
              or else Second (1) /= 16#5EED_1134#
              or else Second (Word_Array'Last) /= 16#5EED_1135#
            then
               Passed := Fail ("a segment doesn't keep what is written");
            end if;
         end if;

         --  The first segment's address is free again, and is the first that
         --  fits. What it held is gone.

         Rose.Unsaved_Memory.Release (First_Address);
         declare
            Again : constant System.Address :=
                      Rose.Unsaved_Memory.Allocate (Size);
            Words : Word_Array with Import, Address => Again;
         begin
            if Passed and then Again /= First_Address then
               Passed := Fail ("a released address isn't used again");
            elsif Passed and then not All_Zero (Words) then
               Passed := Fail ("a released segment's memory came back");
            end if;
            Rose.Unsaved_Memory.Release (Again);
         end;

         begin
            Rose.Unsaved_Memory.Release (First_Address);
            if Passed then
               Passed := Fail ("releasing it twice doesn't raise");
            end if;
         exception
            when Rose.Unsaved_Memory.Mapping_Error =>
               null;
         end;

         Rose.Unsaved_Memory.Release (Second_Address);
         return Passed;
      end;
   end Unsaved_Memory_Test;

end Tests.Unsaved_Memory;
