with System;

with Rose.Objects;

package Rose.Kernel.Checkpoint_Copies is

   --  The kernel's side of ADR 0030: while the memory server writes a
   --  checkpoint, a persistent process that writes a page of it goes on
   --  running. The kernel copies the page's frame first, into a frame of
   --  a reserve that the memory server gives it, and the memory server
   --  saves the copy instead of the frame.
   --
   --  A frame is in the checkpoint unless it is marked: the memory server
   --  has saved it (Save_Frame), or has filled it with new contents since
   --  the checkpoint started (Mark_Fresh), so it holds a page that the
   --  checkpoint doesn't save.
   --
   --  A frame is copied before it is written only if it is in the
   --  checkpoint, and was changed when the checkpoint started: some
   --  process had it mapped writable (Note_Changed). A clean page needs
   --  no copy, but a frame of a shared segment can be clean in one
   --  process and changed in another.

   Max_Reserve : constant := 256;
   --  The most frames the reserve holds.

   function Copying return Boolean;
   --  A checkpoint has started, and the memory server hasn't yet ended
   --  its part (Stop_Copies).

   procedure Start_Copies;
   --  The checkpoint starts: no frame is marked or changed, and there are
   --  no copies. It is called before the pages are made read-only.

   procedure Note_Changed (Frame : Rose.Addresses.Physical_Page_Address);
   --  A process had Frame mapped writable when the checkpoint started
   --  (Rose.Kernel.Page_Table.Disable_Page_Writes).

   procedure Stop_Copies;
   --  The memory server has saved its pages: every copy goes back to the
   --  reserve, and no frame is copied until the next checkpoint. The list
   --  of written pages is kept until Clear_Written.

   procedure Add_Frame
     (Frame : Rose.Addresses.Physical_Page_Address;
      Added : out Boolean);
   --  Add Frame to the reserve. Added is False if the reserve is full.

   function Reserve_Count return Natural;
   --  The frames in the reserve that hold no copy.

   procedure Mark_Fresh (Frame : Rose.Addresses.Physical_Page_Address);
   --  The memory server is filling Frame with new contents: it is no
   --  longer in the checkpoint, and a copy of it is dropped. Nothing
   --  happens unless Copying.

   function In_Checkpoint
     (Frame : Rose.Addresses.Physical_Page_Address)
      return Boolean;
   --  Copying, and Frame is not marked.

   procedure Prepare_Write
     (Frame : Rose.Addresses.Physical_Page_Address;
      Ready : out Boolean);
   --  Frame is about to be written. If it is in the checkpoint, was
   --  changed, and has no copy, copy it. Ready is False if a copy was
   --  needed and the reserve is empty.

   procedure Save_Frame
     (Frame : Rose.Addresses.Physical_Page_Address;
      To    : System.Address;
      Saved : out Boolean);
   --  Copy Frame's contents when the checkpoint started, from its copy if
   --  it has one, or else from the frame itself, to the page at To in
   --  the current address space. Saved is In_Checkpoint (Frame) before
   --  the call. If it is True, Frame is marked, and its copy goes back to
   --  the reserve.

   procedure Record_Written
     (Process  : Rose.Objects.Object_Id;
      Page     : Rose.Addresses.Virtual_Page_Address;
      Recorded : out Boolean);
   --  Add Page of Process to the pages written during the checkpoint.
   --  Recorded is False if the list is full.

   function Is_Written
     (Process : Rose.Objects.Object_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Boolean;
   --  Page of Process is in the list of written pages. Until mem has the
   --  list, its tables say such a page is clean, so the kernel refuses to
   --  unmap it: mem would drop it, and lose the write.

   function Written_Count return Natural;

   procedure Get_Written
     (Index   : Positive;
      Process : out Rose.Objects.Object_Id;
      Page    : out Rose.Addresses.Virtual_Page_Address);

   procedure Clear_Written;

   procedure Report_Full (What : String);
   --  Say in the kernel's log, once for each checkpoint, that a writer
   --  waits because the reserve or the list of written pages is full.

end Rose.Kernel.Checkpoint_Copies;
