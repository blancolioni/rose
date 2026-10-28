with Ada.Finalization;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Tests.Finalization is

   Initialized : Natural := 0;
   Adjusted    : Natural := 0;
   Finalized   : Natural := 0;

   package Counted_Objects is

      type Counted is new Ada.Finalization.Controlled with
         record
            Value : Natural := 0;
         end record;

      overriding procedure Initialize (Item : in out Counted);
      overriding procedure Adjust (Item : in out Counted);
      overriding procedure Finalize (Item : in out Counted);

   end Counted_Objects;

   procedure Reset;
   --  Set the counts to zero.

   function Counts_Are
     (Initialize, Adjust, Finalize : Natural)
      return Boolean
   is (Initialized = Initialize
       and then Adjusted = Adjust
       and then Finalized = Finalize);

   ---------------------
   -- Counted_Objects --
   ---------------------

   package body Counted_Objects is

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Item : in out Counted) is
         pragma Unreferenced (Item);
      begin
         Adjusted := Adjusted + 1;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Item : in out Counted) is
         pragma Unreferenced (Item);
      begin
         Finalized := Finalized + 1;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      overriding procedure Initialize (Item : in out Counted) is
      begin
         Item.Value := 1;
         Initialized := Initialized + 1;
      end Initialize;

   end Counted_Objects;

   -----------------------
   -- Finalization_Test --
   -----------------------

   function Finalization_Test return Boolean is
      use Counted_Objects;
   begin
      Ada.Text_IO.Put ("Finalization test");

      --  An object declared in a block is initialized and finalized.

      Reset;
      declare
         A : Counted;
      begin
         if A.Value /= 1 or else not Counts_Are (1, 0, 0) then
            return False;
         end if;
      end;

      if not Counts_Are (1, 0, 1) then
         return False;
      end if;

      --  A copy is adjusted, not initialized. An assignment finalizes the
      --  target before the copy, and adjusts the copy.

      Reset;
      declare
         A : Counted;
         B : Counted := A;
      begin
         B := A;
         if B.Value /= 1 or else not Counts_Are (1, 2, 1) then
            return False;
         end if;
      end;

      if not Counts_Are (1, 2, 3) then
         return False;
      end if;

      --  An allocated object is finalized when it is freed, and objects
      --  still on the heap when their access type goes out of scope.

      Reset;
      declare
         type Counted_Access is access Counted;
         procedure Free is
           new Ada.Unchecked_Deallocation (Counted, Counted_Access);
         P : Counted_Access := new Counted;
         Q : constant Counted_Access := new Counted;
      begin
         Free (P);
         if P /= null
           or else Q.Value /= 1
           or else not Counts_Are (2, 0, 1)
         then
            return False;
         end if;
      end;

      return Counts_Are (2, 0, 2);
   end Finalization_Test;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Initialized := 0;
      Adjusted := 0;
      Finalized := 0;
   end Reset;

end Tests.Finalization;
