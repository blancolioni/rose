with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Tests.Heap is

   type Block is array (Positive range <>) of Character;
   type Block_Access is access Block;

   procedure Free is
     new Ada.Unchecked_Deallocation (Block, Block_Access);

   Block_Count : constant := 2_000;
   Large_Size  : constant := 400_000;

   Huge_Size   : constant := 1_536 * 2 ** 20;
   Second_Size : constant := 1_024 * 2 ** 20;
   --  The heap segment is 2 GiB, so one block of each size fits, but not
   --  both.

   Blocks : array (1 .. Block_Count) of Block_Access;

   function Huge_Test return Boolean;
   --  Allocate a block much larger than the heap's old limit, check that
   --  a second large block raises Storage_Error, and free both.

   function Expected
     (Seed     : Positive;
      Position : Positive)
      return Character
   is (Character'Val (Character'Pos ('A') + (Seed + Position) mod 26));
   --  The character at Position in a block filled with Seed.

   procedure Fill (Item : Block_Access; Seed : Positive);
   --  Fill Item with characters that depend on Seed.

   function Is_Filled (Item : Block_Access; Seed : Positive) return Boolean;
   --  True if Item is as Fill left it.

   ----------
   -- Fill --
   ----------

   procedure Fill (Item : Block_Access; Seed : Positive) is
   begin
      for Position in Item'Range loop
         Item (Position) := Expected (Seed, Position);
      end loop;
   end Fill;

   ---------------
   -- Heap_Test --
   ---------------

   function Heap_Test return Boolean is

      function Seed (Index : Positive; Round : Positive) return Positive
      is (Index + 7 * Round);

   begin
      Ada.Text_IO.Put ("Heap test");

      for Index in Blocks'Range loop
         Blocks (Index) := new Block (1 .. 1 + Index mod 128);
         Fill (Blocks (Index), Seed (Index, 1));
      end loop;

      --  Free every other block, and allocate blocks of other sizes in
      --  their place: some fit in the freed blocks, and some do not.

      for Index in Blocks'Range loop
         if Index mod 2 = 0 then
            Free (Blocks (Index));
         end if;
      end loop;

      for Index in Blocks'Range loop
         if Index mod 2 = 0 then
            Blocks (Index) := new Block (1 .. 1 + (Index * 7) mod 200);
            Fill (Blocks (Index), Seed (Index, 2));
         end if;
      end loop;

      for Index in Blocks'Range loop
         if not Is_Filled
           (Blocks (Index), Seed (Index, (if Index mod 2 = 0 then 2 else 1)))
         then
            return False;
         end if;
      end loop;

      --  With every block free, a large one needs the free blocks merged,
      --  and the heap grown.

      for Item of Blocks loop
         Free (Item);
      end loop;

      declare
         Large : Block_Access := new Block (1 .. Large_Size);
         Valid : Boolean;
      begin
         Fill (Large, 3);
         Valid := Is_Filled (Large, 3);
         Free (Large);
         if not Valid then
            return False;
         end if;
      end;

      return Huge_Test;
   end Heap_Test;

   ---------------
   -- Huge_Test --
   ---------------

   function Huge_Test return Boolean is
      Huge  : Block_Access := new Block (1 .. Huge_Size);
      Valid : Boolean := True;
   begin

      --  The heap grows far past its old 1 MiB without the pages in
      --  between being touched. Only the ends of the block are written.

      Huge (Huge'First) := 'H';
      Huge (Huge'Last) := 'T';
      Valid := Huge (Huge'First) = 'H' and then Huge (Huge'Last) = 'T';

      --  With the huge block still there, another cannot fit below the
      --  heap segment's limit, so the heap does not grow and the
      --  allocation raises Storage_Error (#759).

      if Valid then
         declare
            Second : Block_Access;
         begin
            Second := new Block (1 .. Second_Size);
            Free (Second);
            Valid := False;
         exception
            when Storage_Error =>
               null;
         end;
      end if;

      Free (Huge);

      --  The heap still serves small blocks.

      if Valid then
         declare
            Small : Block_Access := new Block (1 .. 100);
         begin
            Fill (Small, 4);
            Valid := Is_Filled (Small, 4);
            Free (Small);
         end;
      end if;

      return Valid;
   end Huge_Test;

   ---------------
   -- Is_Filled --
   ---------------

   function Is_Filled (Item : Block_Access; Seed : Positive) return Boolean is
   begin
      for Position in Item'Range loop
         if Item (Position) /= Expected (Seed, Position) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Filled;

end Tests.Heap;
