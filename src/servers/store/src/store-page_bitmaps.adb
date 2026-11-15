with Rose.Words;

package body Store.Page_Bitmaps is

   use type Rose.Words.Word_64;

   Word_Count : constant Positive := Max_Pages / 64;
   --  The words of each bit array, 64 pages to a word.

   Words_Per_Image_Page : constant := Words_Per_Page;

   subtype Bit_Word is Natural range 0 .. Word_Count - 1;

   type Bit_Array is array (Bit_Word) of Rose.Words.Word_64;

   All_Ones : constant Rose.Words.Word_64 := Rose.Words.Word_64'Last;

   In_Use  : Bit_Array;
   Born    : Bit_Array;
   Held    : Bit_Array;
   Waiting : Bit_Array;
   --  Born: allocated since the last checkpoint.  Held: freed since the
   --  last checkpoint, and not born.  Waiting: freed before it, until a
   --  checkpoint is committed.  Held and waiting pages are in use.  They
   --  have no initial values, so that they take no space in the store's
   --  image; Reset gives them their values.

   Free_Pages : Natural := 0;
   --  The pages that aren't in use.

   type Dirty_Array is array (Copy_Index, Image_Page_Index) of Boolean;

   Dirty : Dirty_Array := (others => (others => True));
   --  Set if a copy's page of the image is older than the image.

   Committed_Copy : Copy_Index := 1;
   Pending        : Boolean := False;
   --  The copy that the last committed checkpoint wrote, and whether a
   --  checkpoint has been taken since.

   Cursor : Bit_Word := 0;
   --  Where Allocate starts to look.

   function Bit_Of
     (Page : Device_Page)
      return Rose.Words.Word_64;
   --  Page's bit in its word.

   function Bit_Count (Bits : Rose.Words.Word_64) return Natural;
   --  How many bits of Bits are set.

   procedure Changed (Word : Bit_Word);
   --  The image has changed in Word.

   function Is_Page
     (Page : Device_Page)
      return Boolean;
   --  True if the bitmap holds Page, which isn't No_Page.

   function Word_Of
     (Page : Device_Page)
      return Bit_Word;
   --  The word that holds Page's bit.

   ---------------
   -- Add_Range --
   ---------------

   procedure Add_Range
     (First : Device_Page;
      Bound : Device_Page)
   is
   begin
      if Bound <= First then
         return;
      end if;

      for Page in First .. Bound - 1 loop
         if Is_Page (Page)
           and then (In_Use (Word_Of (Page)) and Bit_Of (Page)) /= 0
         then
            In_Use (Word_Of (Page)) :=
              In_Use (Word_Of (Page)) and not Bit_Of (Page);
            Free_Pages := Free_Pages + 1;
            Changed (Word_Of (Page));
         end if;
      end loop;
   end Add_Range;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Page : out Device_Page)
   is
      Word : Bit_Word := Cursor;
   begin
      for Count in 1 .. Word_Count loop
         if In_Use (Word) /= All_Ones then
            declare
               Free_Bits : constant Rose.Words.Word_64 := not In_Use (Word);
               Bit_Index : Natural := 0;
               Bit       : Rose.Words.Word_64;
            begin
               while (Shift_Right (Free_Bits, Bit_Index) and 1) = 0 loop
                  Bit_Index := Bit_Index + 1;
               end loop;

               Bit := Shift_Left (1, Bit_Index);
               In_Use (Word) := In_Use (Word) or Bit;
               Born (Word) := Born (Word) or Bit;
               Free_Pages := Free_Pages - 1;
               Changed (Word);
               Cursor := Word;
               Page := Device_Page (Word * 64 + Bit_Index);
               return;
            end;
         end if;

         Word := (if Word = Bit_Word'Last then 0 else Word + 1);
      end loop;

      Page := No_Page;
   end Allocate;

   ------------
   -- Bit_Of --
   ------------

   function Bit_Of
     (Page : Device_Page)
      return Rose.Words.Word_64
   is (Shift_Left (1, Natural (Page mod 64)));

   ---------------
   -- Bit_Count --
   ---------------

   function Bit_Count (Bits : Rose.Words.Word_64) return Natural is
      Rest  : Rose.Words.Word_64 := Bits;
      Count : Natural := 0;
   begin
      while Rest /= 0 loop
         Rest := Rest and (Rest - 1);
         Count := Count + 1;
      end loop;
      return Count;
   end Bit_Count;

   -------------
   -- Changed --
   -------------

   procedure Changed (Word : Bit_Word) is
      Index : constant Image_Page_Index :=
                Word / Words_Per_Image_Page + 1;
   begin
      Dirty (0, Index) := True;
      Dirty (1, Index) := True;
   end Changed;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint is
   begin
      for Word in Bit_Word loop
         if Held (Word) /= 0 then
            Waiting (Word) := Waiting (Word) or Held (Word);
            Held (Word) := 0;
            Changed (Word);
         end if;
         Born (Word) := 0;
      end loop;
      Pending := True;
   end Checkpoint;

   ---------------
   -- Committed --
   ---------------

   procedure Committed is
   begin

      --  A waiting page is in use and not in the image, so freeing it
      --  leaves the image as it is.

      for Word in Bit_Word loop
         if Waiting (Word) /= 0 then
            In_Use (Word) := In_Use (Word) and not Waiting (Word);
            Free_Pages := Free_Pages + Bit_Count (Waiting (Word));
            Waiting (Word) := 0;
         end if;
      end loop;

      if Pending then
         Committed_Copy := 1 - Committed_Copy;
         Pending := False;
      end if;
   end Committed;

   ----------
   -- Free --
   ----------

   procedure Free
     (Page : Device_Page)
   is
   begin
      if not Is_Page (Page) then
         return;
      end if;

      declare
         Word : constant Bit_Word := Word_Of (Page);
         Bit  : constant Rose.Words.Word_64 := Bit_Of (Page);
      begin
         if (In_Use (Word) and Bit) = 0
           or else ((Held (Word) or Waiting (Word)) and Bit) /= 0
         then
            return;
         end if;

         if (Born (Word) and Bit) /= 0 then
            In_Use (Word) := In_Use (Word) and not Bit;
            Born (Word) := Born (Word) and not Bit;
            Free_Pages := Free_Pages + 1;
            Changed (Word);
         else
            Held (Word) := Held (Word) or Bit;
         end if;
      end;
   end Free;

   ----------------
   -- Free_Count --
   ----------------

   function Free_Count return Natural is (Free_Pages);

   ---------------
   -- Get_Image --
   ---------------

   procedure Get_Image
     (Index : Image_Page_Index;
      Data  : out Page_Words)
   is
      First : constant Bit_Word := (Index - 1) * Words_Per_Image_Page;
   begin
      for I in Data'Range loop
         Data (I) := In_Use (First + Natural (I))
           and not Waiting (First + Natural (I));
      end loop;
   end Get_Image;

   ----------------
   -- Image_Copy --
   ----------------

   function Image_Copy return Copy_Index is (1 - Committed_Copy);

   -------------
   -- Is_Born --
   -------------

   function Is_Born
     (Page : Device_Page)
      return Boolean
   is (Is_Page (Page)
       and then (Born (Word_Of (Page)) and Bit_Of (Page)) /= 0);

   --------------
   -- Is_Dirty --
   --------------

   function Is_Dirty
     (Copy  : Copy_Index;
      Index : Image_Page_Index)
      return Boolean
   is (Dirty (Copy, Index));

   -------------
   -- Is_Held --
   -------------

   function Is_Held
     (Page : Device_Page)
      return Boolean
   is (Is_Page (Page)
       and then ((Held (Word_Of (Page)) or Waiting (Word_Of (Page)))
                 and Bit_Of (Page)) /= 0);

   ---------------
   -- Is_In_Use --
   ---------------

   function Is_In_Use
     (Page : Device_Page)
      return Boolean
   is (not Is_Page (Page)
       or else (In_Use (Word_Of (Page)) and Bit_Of (Page)) /= 0);

   -------------
   -- Is_Page --
   -------------

   function Is_Page
     (Page : Device_Page)
      return Boolean
   is (Page /= No_Page and then Page < Device_Page (Max_Pages));

   ----------------
   -- Load_Image --
   ----------------

   procedure Load_Image
     (Copy  : Copy_Index;
      Index : Image_Page_Index;
      Data  : Page_Words)
   is
      First : constant Bit_Word := (Index - 1) * Words_Per_Image_Page;
   begin
      for I in Data'Range loop
         Free_Pages := Free_Pages
           - Bit_Count (not In_Use (First + Natural (I)))
           + Bit_Count (not Data (I));
         In_Use (First + Natural (I)) := Data (I);
         Born (First + Natural (I)) := 0;
         Held (First + Natural (I)) := 0;
         Waiting (First + Natural (I)) := 0;
      end loop;

      --  Page 0 stands for no page, so it is always in use.

      if Index = 1 and then (In_Use (0) and 1) = 0 then
         Free_Pages := Free_Pages - 1;
         In_Use (0) := In_Use (0) or 1;
      end if;

      Dirty (Copy, Index) := False;
      Dirty (1 - Copy, Index) := True;
      Committed_Copy := Copy;
      Pending := False;
   end Load_Image;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      In_Use := (others => All_Ones);
      Born := (others => 0);
      Held := (others => 0);
      Waiting := (others => 0);
      Free_Pages := 0;
      Dirty := (others => (others => True));
      Committed_Copy := 1;
      Pending := False;
      Cursor := 0;
   end Reset;

   -----------------
   -- Set_Written --
   -----------------

   procedure Set_Written
     (Copy  : Copy_Index;
      Index : Image_Page_Index)
   is
   begin
      Dirty (Copy, Index) := False;
   end Set_Written;

   -------------
   -- Word_Of --
   -------------

   function Word_Of
     (Page : Device_Page)
      return Bit_Word
   is (Natural (Page / 64));

end Store.Page_Bitmaps;
