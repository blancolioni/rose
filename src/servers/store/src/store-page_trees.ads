with Rose.Words;

with Store.Device_Pages;

generic
   Cache_Size : Positive;
   --  The pages of the tree that are kept in memory.  At least
   --  Max_Height + 2, so that a path from the root to a leaf, and one
   --  more page, fit.

   with procedure Read
     (Page : Store.Device_Pages.Device_Page;
      Data : out Store.Device_Pages.Page_Words);

   with procedure Write
     (Page : Store.Device_Pages.Device_Page;
      Data : Store.Device_Pages.Page_Words);

   with procedure Allocate
     (Page : out Store.Device_Pages.Device_Page);
   --  A free page, which is born, or No_Page if none is free.

   with procedure Free
     (Page : Store.Device_Pages.Device_Page);

   with function Is_Born
     (Page : Store.Device_Pages.Device_Page)
      return Boolean;
   --  True if Page was allocated since the last checkpoint, so that no
   --  checkpoint names it.

package Store.Page_Trees is

   --  The block map of ADR 0038, sections 1 to 3, 7 and 9: a radix tree
   --  of pages on the backing stores, from a key to the device page that
   --  holds its contents.  The store's key is a page id less
   --  Page_Object_Id'First.  A page of the tree is 512 entries, for 9
   --  bits of the key, and an entry is a device page, or zero for none.
   --  A key with no entry reads as zeroes.
   --
   --  Pages are copy-on-write.  A page of the tree or of data whose
   --  device page is born is written in place, and any other is written
   --  to a new device page, and the old one is freed.  So the root that
   --  Checkpoint returns names the tree and the data as they were then,
   --  whatever is written later.
   --
   --  The subprograms that may need a new device page have Success,
   --  which is False if none was free.

   use Store.Device_Pages;
   use type Rose.Words.Word_64;

   subtype Key_Type is Rose.Words.Word_64;

   Max_Height : constant := 7;
   Max_Key    : constant Key_Type := 2 ** 63 - 1;

   procedure Create;
   --  An empty tree.

   procedure Restore
     (Root   : Device_Page;
      Height : Positive);
   --  The tree whose root Checkpoint returned, with its height.

   procedure Find
     (Key     : Key_Type;
      Page    : out Device_Page;
      Success : out Boolean);
   --  The device page that holds Key's contents, or No_Page.

   procedure Get
     (Key     : Key_Type;
      Data    : out Page_Words;
      Found   : out Boolean;
      Success : out Boolean);
   --  Key's contents, or zeroes, with Found False, if it has none.

   procedure Put
     (Key     : Key_Type;
      Data    : Page_Words;
      Added   : out Boolean;
      Success : out Boolean);
   --  Write Key's contents.  Added is True if Key had none before.

   procedure Remove
     (Key     : Key_Type;
      Removed : out Boolean;
      Success : out Boolean);
   --  Free Key's contents, if it has any.

   procedure Release
     (First   : Key_Type;
      Bound   : Key_Type;
      Freed   : out Natural;
      Success : out Boolean);
   --  Free the contents of the keys First .. Bound - 1, visiting only
   --  the parts of the tree that hold some.  Freed is how many keys had
   --  contents.

   procedure Checkpoint
     (Root    : out Device_Page;
      Height  : out Positive;
      Success : out Boolean);
   --  Write every page of the tree that has changed, from the leaves up,
   --  and return the root and the height that Restore needs.

   function Tree_Height return Positive;

   function Cached_Pages return Natural;
   --  The pages of the tree in the cache.

   function Dirty_Pages return Natural;
   --  The pages of the tree in the cache that have changed.

end Store.Page_Trees;
