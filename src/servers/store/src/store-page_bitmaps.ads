with Store.Device_Pages;

generic
   Max_Pages : Positive;
   --  The bitmap holds the pages 0 .. Max_Pages - 1, of which page 0 is
   --  never free.  A multiple of Pages_Per_Image_Page.
package Store.Page_Bitmaps is

   --  The free map of ADR 0038, sections 4 and 5.
   --
   --  A page is in use from Allocate until it is freed, but a page that
   --  a checkpoint may name stays in use until a later checkpoint is
   --  committed:
   --
   --  * a page allocated since the last checkpoint is born, and is free
   --    again as soon as it is freed, since no checkpoint names it;
   --
   --  * any other page is held when it is freed, and waits for the next
   --    checkpoint to be committed.
   --
   --  The image is the free map as a checkpoint saves it: a bit for each
   --  page, set if the page is in use, less the pages that wait for a
   --  checkpoint already taken.  Taken just after Checkpoint, it is
   --  exactly the pages that the checkpoint's tree names.  The store
   --  keeps two copies of it, and a checkpoint writes the copy that the
   --  last committed checkpoint didn't, so that copy is never
   --  overwritten before a later checkpoint is committed.

   use Store.Device_Pages;

   Pages_Per_Image_Page : constant := Words_Per_Page * 64;

   Image_Page_Count : constant Positive :=
                        Max_Pages / Pages_Per_Image_Page;

   subtype Image_Page_Index is Positive range 1 .. Image_Page_Count;

   type Copy_Index is range 0 .. 1;

   procedure Reset;
   --  Every page in use, and none born or held, as before any backing
   --  store is added.

   procedure Add_Range
     (First : Device_Page;
      Bound : Device_Page);
   --  Make the pages First .. Bound - 1 free.

   procedure Allocate
     (Page : out Device_Page);
   --  A free page, now in use and born, or No_Page if none is free.

   procedure Free
     (Page : Device_Page);
   --  Free Page now if it is born, or else hold it for the next commit.

   function Is_Born
     (Page : Device_Page)
      return Boolean;
   --  True if Page was allocated since the last checkpoint.

   function Is_In_Use
     (Page : Device_Page)
      return Boolean;
   --  True if Page can't be allocated, which includes a held page.

   function Is_Held
     (Page : Device_Page)
      return Boolean;
   --  True if Page has been freed, and waits for a commit.

   function Free_Count return Natural;
   --  The pages that can be allocated.

   procedure Checkpoint;
   --  The store's part of a checkpoint is being written.  The pages held
   --  so far wait for it to be committed, and no page is born.

   procedure Committed;
   --  The last checkpoint is committed.  The pages that waited for it
   --  are free, and its image copy is the one to keep.

   function Image_Copy return Copy_Index;
   --  The copy that the image goes to: the one that the last committed
   --  checkpoint didn't write.

   function Is_Dirty
     (Copy  : Copy_Index;
      Index : Image_Page_Index)
      return Boolean;
   --  True if page Index of copy Copy is older than the image.

   procedure Get_Image
     (Index : Image_Page_Index;
      Data  : out Page_Words);
   --  Page Index of the image.

   procedure Set_Written
     (Copy  : Copy_Index;
      Index : Image_Page_Index);
   --  Page Index of the image is on the device, in copy Copy.

   procedure Load_Image
     (Copy  : Copy_Index;
      Index : Image_Page_Index;
      Data  : Page_Words);
   --  Restore page Index of the image, read from copy Copy, which the
   --  restored checkpoint wrote.  The pages it says are in use are in
   --  use, and none is born or held.

end Store.Page_Bitmaps;
