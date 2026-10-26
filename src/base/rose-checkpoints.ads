with System.Storage_Elements;

with Rose.Addresses;
with Rose.Limits;
with Rose.Words;

package Rose.Checkpoints is

   use type System.Storage_Elements.Storage_Offset;

   --  A checkpoint log entry holds the kernel's tables, then the storage
   --  server's region table, then the memory server's tables and pages.

   --  The kernel's part: a header page, then for each persistent process
   --  a process record page followed by its capability pages.  The
   --  kernel writes a snapshot of these into the checkpoint server's
   --  buffer when the checkpoint starts, and the checkpoint server
   --  appends the pages to the log.

   Kernel_Table_Magic   : constant := 16#4B54_4142_4C45_5331#;
   Kernel_Table_Version : constant := 1;

   type Kernel_Table_Header is
      record
         Magic         : Rose.Words.Word_64 := Kernel_Table_Magic;
         Version       : Rose.Words.Word_32 := Kernel_Table_Version;
         Page_Count    : Rose.Words.Word_32 := 0;
         --  All of the kernel's pages, including this one.
         Process_Count : Rose.Words.Word_32 := 0;
         Reserved      : System.Storage_Elements.Storage_Array
                           (1 .. Rose.Limits.Page_Size - 20) :=
                             (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   Process_Record_Magic : constant := 16#5052_4F43_4553_5331#;

   Max_Saved_Waiting   : constant := 16;
   Max_Saved_Ranges    : constant := 8;
   Max_Saved_Endpoints : constant := 32;

   Saved_Register_Bytes   : constant := 64;
   Saved_Invocation_Bytes : constant := 160;

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

   type Saved_Waiting_Array is
     array (1 .. Max_Saved_Waiting) of Rose.Words.Word_64
     with Pack, Size => Max_Saved_Waiting * 64;

   type Saved_Range_Array is
     array (1 .. Max_Saved_Ranges) of Saved_Page_Range
     with Pack, Size => Max_Saved_Ranges * 96;

   type Saved_Endpoint_Array is
     array (1 .. Max_Saved_Endpoints) of Saved_Endpoint
     with Pack, Size => Max_Saved_Endpoints * 128;

   Process_Record_Bytes : constant := 1216;
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
   --  index, which the caps for it carry as their identifier, the
   --  backing store it is on, its pages Base .. Bound - 1, and the
   --  space it was allocated, which a subregion doesn't have: its
   --  Alloc_Index is zero.

   Store_Table_Magic   : constant := 16#5354_4F52_4554_4231#;
   Store_Table_Version : constant := 1;

   type Store_Table_Header is
      record
         Magic        : Rose.Words.Word_64 := Store_Table_Magic;
         Version      : Rose.Words.Word_32 := Store_Table_Version;
         Page_Count   : Rose.Words.Word_32 := 0;
         --  All of the storage server's pages, including this one.
         Region_Count : Rose.Words.Word_32 := 0;
         Reserved     : System.Storage_Elements.Storage_Array
                          (1 .. Rose.Limits.Page_Size - 20) :=
                            (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   type Saved_Region is
      record
         Index       : Rose.Words.Word_32 := 0;
         Device      : Rose.Words.Word_32 := 0;
         Base        : Rose.Words.Word_64 := 0;
         Bound       : Rose.Words.Word_64 := 0;
         Alloc_Index : Rose.Words.Word_32 := 0;
         Alloc_Banks : Rose.Words.Word_32 := 0;
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

   --  The memory server's part of a checkpoint log entry starts with its
   --  tables: a header page, then a record page for each of its active
   --  processes, which are the persistent ones.  A record names the
   --  process by its object id and by the memory server's own process
   --  id, which the caps the memory server gave the process use as
   --  their identifier, and which the header pages below use for pages.

   Mem_Table_Magic   : constant := 16#4D45_4D54_4142_4C31#;
   Mem_Table_Version : constant := 1;

   type Mem_Table_Header is
      record
         Magic         : Rose.Words.Word_64 := Mem_Table_Magic;
         Version       : Rose.Words.Word_32 := Mem_Table_Version;
         Page_Count    : Rose.Words.Word_32 := 0;
         --  All of the memory server's table pages, including this one.
         Process_Count : Rose.Words.Word_32 := 0;
         Reserved      : System.Storage_Elements.Storage_Array
                           (1 .. Rose.Limits.Page_Size - 20) :=
                             (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   Mem_Process_Magic : constant := 16#4D50_524F_4345_5331#;

   Max_Saved_Segments : constant := 8;

   --  Segment flags.
   Segment_Readable       : constant := 16#01#;
   Segment_Writable       : constant := 16#02#;
   Segment_Executable     : constant := 16#04#;
   Segment_Extensible     : constant := 16#08#;
   Segment_Persistent     : constant := 16#10#;
   Segment_Private_Region : constant := 16#20#;

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
   --  faulted.

   type Mem_Process_Record is
      record
         Magic         : Rose.Words.Word_64 := Mem_Process_Magic;
         Oid           : Rose.Words.Word_64 := 0;
         Mem_Pid       : Rose.Words.Word_32 := 0;
         State         : Rose.Words.Word_32 := 0;
         Segment_Count : Rose.Words.Word_32 := 0;
         Unused        : Rose.Words.Word_32 := 0;
         Segments      : Saved_Segment_Array;
         Reserved      : System.Storage_Elements.Storage_Array
                           (1 .. Rose.Limits.Page_Size
                                - Mem_Process_Record_Bytes) :=
                             (others => 0);
      end record
     with Pack, Size => Rose.Limits.Page_Size * 8;

   --  After the tables come one or more header pages that list the
   --  pages the memory server saved, followed by those pages in the
   --  order they are listed. The last header page has
   --  Is_Last_Header_Page set. The memory server writes this, and the
   --  restore server reads it.

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

end Rose.Checkpoints;
