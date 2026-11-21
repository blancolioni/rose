with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Processes.Restorable.Server;

package body Page_Holder is

   use type Rose.Words.Word_32;

   Page_Size  : constant := 4096;
   Page_Words : constant := Page_Size / 4;

   Held_Pages : constant := 12_288;
   --  48 MiB, more than the 32 MiB machine of the boot tests has
   --  (scripts/qemu-test-lib.sh), so that mem has to page them out.

   Rewrite_Share : constant := 4;
   --  After each restore, one page in Rewrite_Share is written again.

   Context : Rose.Server.Server_Context;

   type Holder_Object is
     limited new Rose.Interfaces.Processes.Restorable.Server
       .Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Holder_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   overriding procedure Restored (Item : in out Holder_Object);

   The_Holder : aliased Holder_Object;

   type Generation_Number is mod 2 ** 8;

   Held : array (0 .. Held_Pages * Page_Words - 1) of Rose.Words.Word_32
     with Alignment => Page_Size;
   --  The pages. They are in the program's bss, since a program's heap
   --  is at most 1 MiB (Elf.Loader.Heap_Size).

   Generations : array (0 .. Held_Pages - 1) of Generation_Number;
   --  The generation that each page was last written with.

   Restores : Natural := 0;
   --  The restores so far. It is saved with the rest of the process, so
   --  a restore of an older checkpoint counts from that checkpoint.

   function Pattern
     (Page       : Natural;
      Word       : Natural;
      Generation : Generation_Number)
      return Rose.Words.Word_32
   is (Rose.Words.Word_32 (Page) * 16#9E37_79B1#
       + Rose.Words.Word_32 (Word) * 16#0001_0003#
       + Rose.Words.Word_32 (Generation) * 16#0100_0000#
       + 16#5A5A_0000#);
   --  What word Word of page Page holds after it is written with
   --  Generation.

   function Bad_Pages (Report : String) return Natural;
   --  The pages that don't hold their pattern. The first few are listed
   --  on standard output, after Report.

   procedure Fill_Page
     (Page       : Natural;
      Generation : Generation_Number);

   function Hex (X : Rose.Words.Word_32) return String;
   --  X in hexadecimal, for the report of a page that is wrong.

   function Image (X : Natural) return String;
   --  X, with no leading space.

   ---------------
   -- Bad_Pages --
   ---------------

   function Bad_Pages (Report : String) return Natural is
      Bad : Natural := 0;
   begin
      for Page in Generations'Range loop
         declare
            First    : constant Natural := Page * Page_Words;
            Expected : constant Generation_Number := Generations (Page);
         begin
            for Word in 0 .. Page_Words - 1 loop
               if Held (First + Word) /= Pattern (Page, Word, Expected) then
                  if Bad < 4 then
                     Ada.Text_IO.Put_Line
                       ("hold_pages: " & Report & ": page " & Image (Page)
                        & " word " & Image (Word)
                        & " holds " & Hex (Held (First + Word))
                        & "; expected "
                        & Hex (Pattern (Page, Word, Expected)));
                  end if;
                  Bad := Bad + 1;
                  exit;
               end if;
            end loop;
         end;
      end loop;
      return Bad;
   end Bad_Pages;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      Bad : Natural;
   begin
      Rose.Interfaces.Processes.Restorable.Server
        .Attach (Context, The_Holder'Access);

      for Page in Generations'Range loop
         Generations (Page) := 0;
         Fill_Page (Page, 0);
      end loop;

      Bad := Bad_Pages ("filled");
      if Bad = 0 then
         Ada.Text_IO.Put_Line
           ("hold_pages: filled " & Image (Held_Pages) & " pages: ok");
      else
         Ada.Text_IO.Put_Line
           ("hold_pages: filled " & Image (Held_Pages) & " pages: FAIL: "
            & Image (Bad) & " wrong");
      end if;
   end Create_Server;

   ---------------
   -- Fill_Page --
   ---------------

   procedure Fill_Page
     (Page       : Natural;
      Generation : Generation_Number)
   is
      First : constant Natural := Page * Page_Words;
   begin
      for Word in 0 .. Page_Words - 1 loop
         Held (First + Word) := Pattern (Page, Word, Generation);
      end loop;
   end Fill_Page;

   ---------
   -- Hex --
   ---------

   function Hex (X : Rose.Words.Word_32) return String is
      Digit : constant String := "0123456789ABCDEF";
      Text  : String (1 .. 8);
      Y     : Rose.Words.Word_32 := X;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digit (Natural (Y mod 16) + 1);
         Y := Y / 16;
      end loop;
      return Text;
   end Hex;

   -----------
   -- Image --
   -----------

   function Image (X : Natural) return String is
      Text : constant String := Natural'Image (X);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Holder_Object) is
      pragma Unreferenced (Item);
      Report  : constant String := "restore " & Image (Restores + 1);
      Bad     : Natural;
      Written : Natural := 0;
   begin
      Restores := Restores + 1;
      Bad := Bad_Pages (Report);

      if Bad > 0 then
         Ada.Text_IO.Put_Line
           ("hold_pages: " & Report & ": FAIL: " & Image (Bad) & " of "
            & Image (Held_Pages) & " pages wrong");
         return;
      end if;

      --  The pages written again are paged out, or saved by the next
      --  checkpoint, so the next restore reads them from a later entry
      --  of the log than the rest.

      for Page in Generations'Range loop
         if Page mod Rewrite_Share = Restores mod Rewrite_Share then
            Generations (Page) := Generation_Number (Restores mod 256);
            Fill_Page (Page, Generations (Page));
            Written := Written + 1;
         end if;
      end loop;

      Ada.Text_IO.Put_Line
        ("hold_pages: " & Report & ": " & Image (Held_Pages)
         & " pages intact; wrote " & Image (Written) & " again");
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Page_Holder;
