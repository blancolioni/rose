with Ada.Text_IO;

with System.Storage_Elements;

with Rose.Unsaved_Memory.Stacks;
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

   type Probe_Record is
      record
         Fail_Now : Boolean;
         Local_At : Integer_Address;
         Sum      : Word_32;
      end record;

   Probe_Failed : exception;

   procedure Probe (Argument : System.Address);
   --  Called on an unsaved stack with a Probe_Record: note where a local
   --  is, fill a kilobyte of the stack, and add it up, or raise
   --  Probe_Failed if Fail_Now.

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   -----------
   -- Probe --
   -----------

   procedure Probe (Argument : System.Address) is
      Item  : Probe_Record with Import, Address => Argument;
      Local : array (1 .. 256) of Word_32 with Volatile_Components;
   begin
      if Item.Fail_Now then
         raise Probe_Failed;
      end if;
      for I in Local'Range loop
         Local (I) := Word_32 (I);
      end loop;
      Item.Local_At := To_Integer (Local'Address);
      Item.Sum := 0;
      for W of Local loop
         Item.Sum := Item.Sum + W;
      end loop;
   end Probe;

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

   ------------------------
   -- Unsaved_Stack_Test --
   ------------------------

   function Unsaved_Stack_Test return Boolean is
      Item : Probe_Record;
   begin
      Ada.Text_IO.Put ("Unsaved stack test");

      --  A process has at most eight unsaved segments, so ten calls only
      --  work if each releases its stack.

      for Call_Count in 1 .. 10 loop
         Item := (Fail_Now => False, Local_At => 0, Sum => 0);
         Rose.Unsaved_Memory.Stacks.Call (Probe'Access, Item'Address);
         if Item.Local_At < Window_Base
           or else Item.Local_At >= Window_Bound
         then
            return Fail ("a local isn't in the window");
         elsif Item.Sum /= 256 * 257 / 2 then
            return Fail ("the procedure didn't fill in its argument");
         end if;
      end loop;

      Item.Fail_Now := True;
      begin
         Rose.Unsaved_Memory.Stacks.Call (Probe'Access, Item'Address);
         return Fail ("an exception didn't come out of Call");
      exception
         when Probe_Failed =>
            null;
      end;

      --  And the stack of the call that raised was released too.

      Item.Fail_Now := False;
      Rose.Unsaved_Memory.Stacks.Call (Probe'Access, Item'Address);
      return True;
   end Unsaved_Stack_Test;

end Tests.Unsaved_Memory;
