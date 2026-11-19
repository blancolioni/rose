with System.Storage_Elements;

with Rose.Addresses;
with Rose.Limits;
with Rose.Words;

package Rose.Checkpoints is

   use type System.Storage_Elements.Storage_Offset;

   --  A checkpoint log entry holds, in this order:
   --
   --  * any number of batches of the memory server's pages, which it
   --    paged out between checkpoints, while the entry was open (ADR
   --    0028, section 4);
   --  * the kernel's tables;
   --  * the storage server's tables;
   --  * one or more batches of the memory server's pages: header pages,
   --    then the pages that they list;
   --  * any number of index sections (ADR 0028): an index header page,
   --    then the index pages that it lists;
   --  * the memory server's tables;
   --  * if the memory server's table header has Mem_Has_Shared_Pages, the
   --    pages of shared segments (ADR 0025).
   --
   --  A reader finds each part by the magic number of its first page, and
   --  skips the pages that a header page counts.

   --  The kernel's part: a header page, then for each persistent process
   --  a process record page followed by its capability pages.  The
   --  kernel writes a snapshot of these into the checkpoint server's
   --  buffer when the checkpoint starts, and the checkpoint server
   --  appends the pages to the log.  The header page also lists the
   --  interrupts that persistent processes handle: each is the vector,
   --  the process, and the endpoint cap in that process that the kernel
   --  sends the interrupt to.

   Kernel_Table_Magic   : constant := 16#4B54_4142_4C45_5331#;
   Kernel_Table_Version : constant := 11;
   --  Version 2 added the interrupts, version 3 the next object id,
   --  version 4 each process's FPU and SSE state, version 5 the packing
   --  of a capability's header, version 6 the derivation records
   --  (ADR 0001), version 7 a table of 64 endpoints in each process,
   --  version 8 each process's signal bits (ADR 0006), version 9 the
   --  interfaces of the boot set that wrote it (ADR 0031, #375),
   --  version 10 the 28-word invocation record, with four capabilities
   --  (#234), and version 11 the 20-word one, with eight data words
   --  (#985).

   Max_Saved_Interrupts : constant := 32;

   type Saved_Interrupt is
      record
         Vector : Rose.Words.Word_32 := 0;
         Cap    : Rose.Words.Word_32 := 0;
         Oid    : Rose.Words.Word_64 := 0;
      end record
     with Pack, Size => 128;

   type Saved_Interrupt_Array is
     array (1 .. Max_Saved_Interrupts) of Saved_Interrupt
     with Pack, Size => Max_Saved_Interrupts * 128;

   Kernel_Header_Bytes : constant := 40 + Max_Saved_Interrupts * 16 + 16;

   type Kernel_Table_Header is
      record
         Magic           : Rose.Words.Word_64 := Kernel_Table_Magic;
         Version         : Rose.Words.Word_32 := Kernel_Table_Version;
         Page_Count      : Rose.Words.Word_32 := 0;
         --  All of the kernel's pages, including this one.
         Process_Count   : Rose.Words.Word_32 := 0;
         Interrupt_Count : Rose.Words.Word_32 := 0;
         Next_Object_Id  : Rose.Words.Word_64 := 0;
         --  The object id the kernel gives the next process.  A restore
         --  carries on from here, so that no process has the id of one
         --  that a cap in the checkpoint names.
         Record_Page_Count : Rose.Words.Word_32 := 0;
         Record_Count      : Rose.Words.Word_32 := 0;
         --  The pages of derivation records that follow this one, and
         --  how many records are in them: those numbered 1 to
         --  Record_Count, as Saved_Record.  There are no pages if no
         --  record is in use.
         Interrupts      : Saved_Interrupt_Array;
         Interface_Major : Rose.Words.Word_16 := 0;
         Interface_Minor : Rose.Words.Word_16 := 0;
         Interface_Level : Rose.Words.Word_32 := 0;
         Interface_Hash  : Rose.Words.Word_64 := 0;
         --  The interfaces of the boot set that wrote the checkpoint
         --  (Rose.Interface_Level): its release series, the number of its
         --  endpoint ids, and a hash of them.  A boot set restores the
         --  checkpoint only if it has the same series and at least the
         --  same level, and at the same level, the same hash (ADR 0031,
         --  section 2).
         Reserved        : System.Storage_Elements.Storage_Array
                             (1 .. Rose.Limits.Page_Size
                                  - Kernel_Header_Bytes) :=
                               (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  A derivation record (ADR 0001).  The counts of the slots and the
   --  children that name it are not saved: a restore counts them again
   --  from the capabilities and the records that it restores.

   Record_Used    : constant := 1;
   Record_Revoked : constant := 2;

   type Saved_Record is
      record
         Parent : Rose.Words.Word_16 := 0;
         Flags  : Rose.Words.Word_16 := 0;
      end record
     with Pack, Size => 32;

   type Saved_Record_Array is
     array (Positive range <>) of Saved_Record
     with Pack;

   Records_Per_Page : constant := Rose.Limits.Page_Size / 4;
   Max_Record_Pages : constant := 4;

   Process_Record_Magic : constant := 16#5052_4F43_4553_5331#;

   Max_Saved_Waiting   : constant := 16;
   Max_Saved_Ranges    : constant := 8;
   Max_Saved_Endpoints : constant := 64;
   Max_Saved_Shared    : constant := 8;

   Saved_Register_Bytes   : constant := 64;
   Saved_Invocation_Bytes : constant := 80;
   Saved_FPU_Bytes        : constant := 512;

   --  Page range flags.
   Range_Valid         : constant := 16#01#;
   Range_Readable      : constant := 16#02#;
   Range_Writable      : constant := 16#04#;
   Range_Executable    : constant := 16#08#;
   Range_Disable_Cache : constant := 16#10#;
   Range_Write_Through : constant := 16#20#;
   Range_Fixed_Address : constant := 16#40#;

   type Saved_Page_Range is
      record
         Base  : Rose.Words.Word_32 := 0;
         Bound : Rose.Words.Word_32 := 0;
         Flags : Rose.Words.Word_32 := 0;
      end record
     with Pack, Size => 96;

   type Saved_Endpoint is
      record
         Endpoint    : Rose.Words.Word_64 := 0;
         Receive_Cap : Rose.Words.Word_32 := 0;
         Send_Cap    : Rose.Words.Word_32 := 0;
      end record
     with Pack, Size => 128;

   --  A page of another process's buffer, which the kernel mapped into
   --  this process's shared slot at Page when it delivered a call from
   --  Caller (#519). An entry with Caller 0 is free; the records of older
   --  checkpoints have only free entries.

   type Saved_Shared_Page is
      record
         Page   : Rose.Words.Word_32 := 0;
         Unused : Rose.Words.Word_32 := 0;
         Caller : Rose.Words.Word_64 := 0;
      end record
     with Pack, Size => 128;

   type Saved_Shared_Array is
     array (1 .. Max_Saved_Shared) of Saved_Shared_Page
     with Pack, Size => Max_Saved_Shared * 128;

   type Saved_Waiting_Array is
     array (1 .. Max_Saved_Waiting) of Rose.Words.Word_64
     with Pack, Size => Max_Saved_Waiting * 64;

   type Saved_Range_Array is
     array (1 .. Max_Saved_Ranges) of Saved_Page_Range
     with Pack, Size => Max_Saved_Ranges * 96;

   type Saved_Endpoint_Array is
     array (1 .. Max_Saved_Endpoints) of Saved_Endpoint
     with Pack, Size => Max_Saved_Endpoints * 128;

   Process_Record_Bytes : constant :=
                            896 + 2 * Saved_Invocation_Bytes
                            + 512 + 4 + Saved_FPU_Bytes
                            + Max_Saved_Shared * 16 + 4;
   --  The bytes of a process record page that are in use.

   --  State is the kernel's Process_State'Pos, and bit N of Flags is set
   --  if the process has the flag whose Process_Flag'Pos is N.
   --  Registers and the two invocation records are copied as the kernel
   --  lays them out on this architecture.  Waiting holds the object ids
   --  of the processes waiting to send to this one, in order.

   type Process_Record is
      record
         Magic            : Rose.Words.Word_64 := Process_Record_Magic;
         Oid              : Rose.Words.Word_64 := 0;
         Name             : String (1 .. 16) := (others => ' ');
         State            : Rose.Words.Word_32 := 0;
         Flags            : Rose.Words.Word_32 := 0;
         Priority         : Rose.Words.Word_32 := 0;
         Default_Cap      : Rose.Words.Word_32 := 0;
         Receive_Cap      : Rose.Words.Word_32 := 0;
         Send_Cap         : Rose.Words.Word_32 := 0;
         Last_Cap         : Rose.Words.Word_32 := 0;
         Cap_Count        : Rose.Words.Word_32 := 0;
         Cap_Page_Count   : Rose.Words.Word_32 := 0;
         Command_Endpoint : Rose.Words.Word_32 := 0;
         Waiting_Endpoint : Rose.Words.Word_32 := 0;
         Waiting_Cap_Id   : Rose.Words.Word_32 := 0;
         Queued_Endpoint  : Rose.Words.Word_32 := 0;
         Queued_Cap_Id    : Rose.Words.Word_32 := 0;
         Checkpoint_Fault : Rose.Words.Word_32 := 0;
         Waiting_Count    : Rose.Words.Word_32 := 0;
         Waiting          : Saved_Waiting_Array := (others => 0);
         Registers        : System.Storage_Elements.Storage_Array
                              (1 .. Saved_Register_Bytes) :=
                                (others => 0);
         Current_Params   : System.Storage_Elements.Storage_Array
                              (1 .. Saved_Invocation_Bytes) :=
                                (others => 0);
         Queued_Params    : System.Storage_Elements.Storage_Array
                              (1 .. Saved_Invocation_Bytes) :=
                                (others => 0);
         Page_Ranges      : Saved_Range_Array;
         Endpoints        : Saved_Endpoint_Array;
         FPU_Valid        : Rose.Words.Word_32 := 0;
         --  1 if FPU_State is the process's FPU and SSE state, in the
         --  FXSAVE layout, and 0 if it has none and starts clean.
         FPU_State        : System.Storage_Elements.Storage_Array
                              (1 .. Saved_FPU_Bytes) :=
                                (others => 0);
         Shared           : Saved_Shared_Array;
         Signals          : Rose.Words.Word_32 := 0;
         --  The signal bits that are set and not yet delivered (ADR
         --  0006).
         Reserved         : System.Storage_Elements.Storage_Array
                              (1 .. Rose.Limits.Page_Size
                                   - Process_Record_Bytes) :=
                                     (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  A capability page holds up to Caps_Per_Page of the process's
   --  active capabilities, as its index and its 16-byte layout.

   Saved_Layout_Bytes : constant := 16;

   type Saved_Cap is
      record
         Index  : Rose.Words.Word_32 := 0;
         Layout : System.Storage_Elements.Storage_Array
                    (1 .. Saved_Layout_Bytes) := (others => 0);
      end record
     with Pack, Size => 160;

   Caps_Per_Page : constant := Rose.Limits.Page_Size / 20;

   type Saved_Cap_Array is
     array (1 .. Caps_Per_Page) of Saved_Cap
     with Pack, Size => Caps_Per_Page * 160;

   type Cap_Page is
      record
         Caps     : Saved_Cap_Array;
         Reserved : System.Storage_Elements.Storage_Array
                      (1 .. Rose.Limits.Page_Size - Caps_Per_Page * 20) :=
                        (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  The storage server's part comes next: a header page, then its
   --  regions, Regions_Per_Page to a page.  A region is saved with its
   --  index, which the caps for it carry as their identifier, its pages
   --  Base .. Bound - 1, Root, the index of the region that the pages
   --  were reserved for, which is its own unless it is a subregion, and
   --  Pages, how many of its pages have space on a backing store, which
   --  only a region that is its own root counts.  A region's pages are
   --  the page ids from the header's Next_Page on, never used again.
   --
   --  The block map isn't in the log (ADR 0038).  It is a tree of pages
   --  on the backing stores, from a page id less Page_Object_Id'First to
   --  the device page that holds the page, written at each checkpoint
   --  before this table.  The header names its root, a device page, and
   --  its height, and which of the two copies of the free map's image the
   --  checkpoint wrote, at the start of each backing store.  The storage
   --  server reads them back itself (Storage_Checkpoint.Restore_Table_
   --  Page).  A page with no entry in the tree reads as zeroes.
   --
   --  The banks come last (ADR 0014, #518): Bank_Page_Count pages, which
   --  hold Bank_Count banks, Banks_Per_Page to a page, each with its
   --  index, which the caps for it carry as their identifier, its parent
   --  (zero for the root bank) and its limits.  A region is saved with
   --  the bank it is charged to.  What a bank has been charged isn't
   --  saved: the storage server works it out again from the regions.
   --
   --  Version 3 replaced the space of each region and its fresh pages
   --  with the map, version 4 added the banks, and version 5 replaced the
   --  map with the tree on the backing stores.  A version of 0 is a table
   --  whose tree couldn't be written, which can't be restored.

   Store_Table_Magic   : constant := 16#5354_4F52_4554_4231#;
   Store_Table_Version : constant := 5;

   type Store_Table_Header is
      record
         Magic            : Rose.Words.Word_64 := Store_Table_Magic;
         Next_Page        : Rose.Words.Word_64 := 0;
         Version          : Rose.Words.Word_32 := Store_Table_Version;
         Page_Count       : Rose.Words.Word_32 := 0;
         --  All of the storage server's pages, including this one.
         Region_Count     : Rose.Words.Word_32 := 0;
         Tree_Height      : Rose.Words.Word_32 := 0;
         Image_Copy       : Rose.Words.Word_32 := 0;
         Bank_Count       : Rose.Words.Word_32 := 0;
         Bank_Page_Count  : Rose.Words.Word_32 := 0;
         Unused           : Rose.Words.Word_32 := 0;
         Tree_Root        : Rose.Words.Word_64 := 0;
         Reserved         : System.Storage_Elements.Storage_Array
                              (1 .. Rose.Limits.Page_Size - 56) :=
                                (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   type Saved_Region is
      record
         Index    : Rose.Words.Word_32 := 0;
         Root     : Rose.Words.Word_32 := 0;
         Base     : Rose.Words.Word_64 := 0;
         Bound    : Rose.Words.Word_64 := 0;
         Pages    : Rose.Words.Word_32 := 0;
         Bank     : Rose.Words.Word_32 := 0;
      end record
     with Pack, Size => 256;

   Regions_Per_Page : constant := Rose.Limits.Page_Size / 32;

   type Saved_Region_Array is
     array (1 .. Regions_Per_Page) of Saved_Region
     with Pack, Size => Regions_Per_Page * 256;

   type Region_Page is
      record
         Regions : Saved_Region_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   type Saved_Bank is
      record
         Index        : Rose.Words.Word_32 := 0;
         Parent       : Rose.Words.Word_32 := 0;
         Page_Limit   : Rose.Words.Word_64 := 0;
         Region_Limit : Rose.Words.Word_32 := 0;
         Owner        : Rose.Words.Word_32 := 0;
         --  The object id of the process whose own bank it is (ADR 0015,
         --  #750), or zero.  Process object ids fit in 32 bits.
      end record
     with Pack, Size => 192;

   Banks_Per_Page : constant := Rose.Limits.Page_Size / 24;

   type Saved_Bank_Array is
     array (1 .. Banks_Per_Page) of Saved_Bank
     with Pack, Size => Banks_Per_Page * 192;

   type Bank_Page is
      record
         Banks    : Saved_Bank_Array;
         Reserved : System.Storage_Elements.Storage_Array
                      (1 .. Rose.Limits.Page_Size - Banks_Per_Page * 24) :=
                        (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  The memory server's tables come after its pages and index pages: a
   --  header page, then a record page for each of its active processes,
   --  which are the persistent ones.  A record names the process by its
   --  object id and by the memory server's own process id, which the caps
   --  the memory server gave the process use as their identifier, and
   --  which the header pages below use for pages.
   --
   --  Version 2 put the tables after the pages, and added the header's
   --  Flags and each process's Index_Directory (ADR 0028).

   Mem_Table_Magic   : constant := 16#4D45_4D54_4142_4C31#;
   Mem_Table_Version : constant := 2;

   Mem_Has_Shared_Pages : constant := 16#0001#;
   --  A flag of Mem_Table_Header: the pages of shared segments follow the
   --  tables.

   Mem_Has_Index : constant := 16#0002#;
   --  A flag of Mem_Table_Header: each process's Index_Directory names
   --  every page of it that the log holds (ADR 0028, section 2), so a
   --  restore can read its pages through the index rather than from
   --  every entry (section 9). Checkpoints from before #949 don't have
   --  it.

   type Mem_Table_Header is
      record
         Magic         : Rose.Words.Word_64 := Mem_Table_Magic;
         Version       : Rose.Words.Word_32 := Mem_Table_Version;
         Page_Count    : Rose.Words.Word_32 := 0;
         --  All of the memory server's table pages, including this one.
         Process_Count : Rose.Words.Word_32 := 0;
         Flags         : Rose.Words.Word_32 := 0;
         Reserved      : System.Storage_Elements.Storage_Array
                           (1 .. Rose.Limits.Page_Size - 24) :=
                             (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   Mem_Process_Magic : constant := 16#4D50_524F_4345_5331#;

   Max_Saved_Segments : constant := 32;
   --  A process record holds up to 101 segments in its page (ADR 0025).

   --  Segment flags.
   Segment_Readable       : constant := 16#01#;
   Segment_Writable       : constant := 16#02#;
   Segment_Executable     : constant := 16#04#;
   Segment_Extensible     : constant := 16#08#;
   Segment_Persistent     : constant := 16#10#;
   Segment_Private_Region : constant := 16#20#;
   Segment_Shared         : constant := 16#40#;

   --  Base, Bound and Limit are virtual page addresses.  The segment's
   --  pages come from the region whose pages have object ids from
   --  Region_Base to Region_Bound, starting Region_Offset pages in.  A
   --  segment without a region has zero for all three.

   type Saved_Segment is
      record
         Base          : Rose.Words.Word_32 := 0;
         Bound         : Rose.Words.Word_32 := 0;
         Limit         : Rose.Words.Word_32 := 0;
         Flags         : Rose.Words.Word_32 := 0;
         Region_Base   : Rose.Words.Word_64 := 0;
         Region_Bound  : Rose.Words.Word_64 := 0;
         Region_Offset : Rose.Words.Word_64 := 0;
      end record
     with Pack, Size => 320;

   type Saved_Segment_Array is
     array (1 .. Max_Saved_Segments) of Saved_Segment
     with Pack, Size => Max_Saved_Segments * 320;

   Mem_Process_Record_Bytes : constant := 32 + Max_Saved_Segments * 40;

   --  State is the memory server's process state: 1 for active, 2 for
   --  faulted.  Index_Directory is the log page of the process's index
   --  directory, or 0 if the log has none of its pages (ADR 0028,
   --  section 2).

   type Mem_Process_Record is
      record
         Magic           : Rose.Words.Word_64 := Mem_Process_Magic;
         Oid             : Rose.Words.Word_64 := 0;
         Mem_Pid         : Rose.Words.Word_32 := 0;
         State           : Rose.Words.Word_32 := 0;
         Segment_Count   : Rose.Words.Word_32 := 0;
         Index_Directory : Rose.Words.Word_32 := 0;
         Segments        : Saved_Segment_Array;
         Reserved        : System.Storage_Elements.Storage_Array
                             (1 .. Rose.Limits.Page_Size
                                  - Mem_Process_Record_Bytes) :=
                               (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  A batch of the memory server's pages is one or more header pages
   --  that list the pages, followed by those pages in the order they are
   --  listed. The last header page has Is_Last_Header_Page set. An entry
   --  has one batch or more after the storage server's tables, before
   --  its index sections and the memory server's tables, and may have
   --  more before the kernel's tables, which the memory server paged out
   --  between checkpoints. The memory server writes them, and the
   --  restore server reads them, in order, so that a page's last copy
   --  wins.
   --
   --  If the memory server's table header has Mem_Has_Shared_Pages, the
   --  pages of shared segments come after its tables (ADR 0025): one or
   --  more shared header pages, the last with Is_Last_Header_Page, then
   --  the pages that they list as saved, in that order.

   Header_Page_Magic : constant := 16#7E6A_F178_79CB_E048#;

   type Header_Page_Flags is new Rose.Words.Word_32;

   Is_Last_Header_Page : constant Header_Page_Flags := 16#0001#;

   type Page_Reference is
      record
         Process : Rose.Words.Word_16 := 0;
         Page    : Rose.Addresses.Virtual_Page_Address := 0;
      end record
     with Pack, Size => 40;

   Max_Page_References : constant := (Rose.Limits.Page_Size - 16) / 5;

   type Page_Reference_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Page_References)
     of Page_Reference;

   type Header_Page is
      record
         Magic : Rose.Words.Word_64 := Header_Page_Magic;
         Flags : Header_Page_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Pages : Page_Reference_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  The index from each page of a process to the log page that holds
   --  its latest copy (ADR 0028, section 2). A process's index directory
   --  is an index page whose entries are the log pages of its index
   --  leaves, and an index leaf is an index page whose entries are the
   --  log pages of 1,024 pages of the process's address space, 4 MiB in
   --  all. An entry of 0 means that the log has no such page: log page 0
   --  is the log's header, so it never holds a page of an entry. A log
   --  page is numbered from 0 within the log partition.
   --
   --  Index pages are never changed in place. An index section is one or
   --  more index header pages, the last with Is_Last_Header_Page, then
   --  the index pages that they list, in that order. A reference names
   --  the process by the memory server's id, and the page by the entry
   --  of the process's directory that names it, or Directory_Slot for the
   --  directory itself.

   Index_Entries_Per_Page : constant := 1024;

   type Index_Entry_Array is
     array (0 .. Index_Entries_Per_Page - 1) of Rose.Words.Word_32
     with Pack, Size => Index_Entries_Per_Page * 32;

   type Index_Page is
      record
         Entries : Index_Entry_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   Index_Header_Magic : constant := 16#494E_4445_5848_4431#;

   Directory_Slot : constant := 16#FFFF#;

   type Index_Reference is
      record
         Process : Rose.Words.Word_16 := 0;
         Slot    : Rose.Words.Word_16 := 0;
      end record
     with Pack, Size => 32;

   Max_Index_References : constant := (Rose.Limits.Page_Size - 16) / 4;

   type Index_Reference_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Index_References)
     of Index_Reference
     with Pack, Size => Max_Index_References * 32;

   type Index_Header_Page is
      record
         Magic : Rose.Words.Word_64 := Index_Header_Magic;
         Flags : Header_Page_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Pages : Index_Reference_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  A shared header page lists store pages, each saved once however many
   --  processes map it. A saved page's contents follow the header pages.
   --  A released page was written back to its region when nothing mapped
   --  it any more, so the region has its newest contents, and a restore
   --  doesn't use an older entry's copy of it.

   Shared_Header_Magic : constant := 16#5348_4152_4544_5031#;

   Shared_Page_Saved    : constant := 1;
   Shared_Page_Released : constant := 2;

   type Shared_Page_Reference is
      record
         Page  : Rose.Words.Word_64 := 0;
         Flags : Rose.Words.Word_32 := 0;
      end record
     with Pack, Size => 96;

   Max_Shared_References : constant := (Rose.Limits.Page_Size - 16) / 12;

   type Shared_Reference_Array is
     array (Rose.Words.Word_32 range 1 .. Max_Shared_References)
     of Shared_Page_Reference
     with Pack, Size => Max_Shared_References * 96;

   type Shared_Header_Page is
      record
         Magic : Rose.Words.Word_64 := Shared_Header_Magic;
         Flags : Header_Page_Flags := 0;
         Count : Rose.Words.Word_32 := 0;
         Pages : Shared_Reference_Array;
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

end Rose.Checkpoints;
