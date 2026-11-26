with System;

with Rose.Addresses;
with Rose.Objects;

private package Mem.Calls is

   procedure Load_Memory_Map;

   procedure Map
     (Process       : Rose.Objects.Object_Id;
      Physical      : Rose.Addresses.Physical_Page_Address;
      Virtual       : Rose.Addresses.Virtual_Page_Address;
      Readable      : Boolean;
      Writeable     : Boolean;
      Executable    : Boolean;
      Write_Tracked : Boolean := False);
   --  Write_Tracked: a clean page of a persistent writable segment, mapped
   --  read-only so that mem sees its first write. During a checkpoint the
   --  kernel makes it writable itself, and gives the page to mem at
   --  End_Page_Copies (ADR 0030, section 3).

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address);
   --  Unmap Virtual from Process. The kernel refuses a page that is pinned
   --  for a call in progress, and says so in its log (#972). Mem says
   --  nothing on the console, whose driver may be waiting on mem (#973).

   procedure Unmap
     (Process    : Rose.Objects.Object_Id;
      Virtual    : Rose.Addresses.Virtual_Page_Address;
      Unmapped   : out Boolean);
   --  Unmap Virtual from Process. Unmapped is False if the kernel refused:
   --  the page is in a buffer that Process has lent to a server in its call
   --  in progress, and the server still maps it (#972).

   procedure Load_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address;
      Fill       : Boolean := False);
   --  Map Physical at Address in mem. Fill says that mem is about to give
   --  the frame new contents, so it is not in a checkpoint that is running
   --  (ADR 0030, section 1).

   procedure Unload_Page
     (Physical   : Rose.Addresses.Physical_Page_Address;
      Address    : System.Address);

   procedure Get_Process_Page
     (From    : Rose.Objects.Page_Object_Id;
      Process : out Rose.Objects.Object_Id;
      Address : out Rose.Addresses.Virtual_Page_Address);

   function Get_Page_Object
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address)
      return Rose.Objects.Page_Object_Id;

   --  Copying pages written during a checkpoint (ADR 0030). Without the
   --  kernel's caps, a page is saved from its frame, and every changed
   --  page is in the checkpoint.

   function Has_Page_Copies return Boolean;
   --  Mem has the kernel's caps for copies.

   procedure Add_Copy_Frames (Count : Natural);
   --  Give the kernel Count free frames for its copies. Mem never uses
   --  them again.

   function In_Checkpoint
     (Physical : Rose.Addresses.Physical_Page_Address)
      return Boolean;
   --  Physical is in the checkpoint that is running: it was neither filled
   --  nor saved since the checkpoint started.

   procedure Copy_Saved_Page
     (Physical : Rose.Addresses.Physical_Page_Address;
      Buffer   : System.Address);
   --  Copy Physical's contents when the checkpoint started into the page
   --  at Buffer, and tell the kernel it is saved.

   type Written_Page is
      record
         Process : Rose.Objects.Object_Id;
         Page    : Rose.Addresses.Virtual_Page_Address;
      end record;

   type Written_Pages is array (Positive range <>) of Written_Page;

   procedure End_Page_Copies
     (Saved   : Boolean;
      Written : out Written_Pages;
      Last    : out Natural);
   --  Mem has saved its pages, or failed if not Saved. The pages that
   --  processes wrote during the checkpoint are in Written (1 .. Last).
   --  Written has room for Rose.Limits.Checkpoint_Written_Pages.

end Mem.Calls;
