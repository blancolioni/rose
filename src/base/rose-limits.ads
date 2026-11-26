package Rose.Limits is

   Max_Processes : constant := 80;
   --  The kernel's limit: the most processes it runs at once, boot
   --  modules included. It bounds everything that counts processes, such
   --  as the processes in a checkpoint (checkpoint, restore). It was 64
   --  until #913; a checkpoint doesn't depend on it, so one made with the
   --  old limit restores.
   --
   --  Not more: the memory server's caps run out before 128 processes
   --  (#1131), and a process that calls it then faults, so the kernel's
   --  limit is kept below that, where a launch is refused cleanly. The
   --  kernel's and mem's process tables are also reserved at boot: 256
   --  took about 2.4 MB more than 64, too much for a 32 MB system.

   Mem_Boot_Entries : constant := 24;
   --  The memory server's table keeps entries 1 to Mem_Boot_Entries for
   --  boot modules, and gives launched processes the rest, so that a boot
   --  module never takes the entry of a process that a restore brings
   --  back (#873). The install media boots about 19 modules.

   Mem_Process_Entries : constant := Mem_Boot_Entries + Max_Processes;
   --  The memory server's table of processes, which it numbers by entry
   --  (Mem.Processes). With Max_Processes entries for launched processes,
   --  it always has room for every process the kernel can run, so only
   --  the kernel's limit is reached (#1090). A checkpoint keeps each
   --  process's entry number as a word, so the table can grow without a
   --  change to the format.

   Max_Physical_Pages : constant := 2 ** 20 - 1;
   Max_Virtual_Pages : constant := 2 ** 20 - 1;

   Min_Mapped_Pages : constant := 8192;
   --  The fewest pages the memory server's table of mapped pages
   --  (Mem.Page_Table) has room for: as many as a 32 MB machine has, which
   --  the system needs as a minimum. The memory server sizes the table
   --  when it starts, at twice its physical pages, since processes can
   --  map a shared or cached page more than once, and at least this many
   --  (#998). It takes the table's pages from its own memory, not from
   --  the kernel's reservation for boot modules. If the table still
   --  fills, the memory server frees pages, as it does when free pages
   --  run low (Reserve_Low_Pages and Reserve_High_Pages). A page that
   --  still doesn't fit is mapped and then not tracked, and a later fault
   --  on it fails the process (#591).

   Max_Dirty_Pages : constant := 8192;
   --  The persistent pages that the memory server lets be changed since
   --  the last checkpoint, and so the most that a checkpoint writes of
   --  them. A write that would change one more pages out a batch of
   --  changed pages first, or else waits for a checkpoint (#998). This
   --  bounds the log's reserve (Log_Reserve_Pages), however large the
   --  table of mapped pages is. A page that a restore or a replayed fault
   --  changes, which can't wait, can take it past the limit, and the
   --  memory server says so in the kernel's log.

   Reserve_Low_Pages  : constant := 64;
   Reserve_High_Pages : constant := 128;
   --  The memory server's reserve of free pages: when fewer than
   --  Reserve_Low_Pages are free, it frees pages that it can read again,
   --  cached pages that nothing maps and then clean pages of processes,
   --  until Reserve_High_Pages are free (ADR 0028, section 5; #973).

   Change_Table_Entries : constant := 4096;
   Change_Table_Bytes   : constant := Change_Table_Entries * 12;
   --  The memory server's table of changes: for each page it has paged
   --  out since the last checkpoint, the process, the page and the log
   --  page it went to (ADR 0028, sections 3 and 6; #974). Part of its
   --  writable data, like Mapped_Page_Table_Bytes.

   Page_Out_Batch_Pages : constant := 7;
   --  The changed pages that the memory server pages out in one batch,
   --  after a header page. It maps them next to each other to append
   --  them in one call, in a window of its writable data (#974). The
   --  header and the pages are lent to the log, and the kernel shares at
   --  most 8 pages with a process at once (Max_Shared_Pages).

   Clean_Start_Percent : constant := 50;
   Clean_Read_Limit    : constant := 256;
   Clean_Copy_Limit    : constant := 512;
   --  The memory server cleans the checkpoint log once it is
   --  Clean_Start_Percent full. After each commit, it reads at most
   --  Clean_Read_Limit header pages of the oldest entries, and copies
   --  at most Clean_Copy_Limit of their live pages forward (#952), and
   --  never while its table of changes is half full or more. It has to
   --  keep up with what the system writes: hold_pages (#975) writes about
   --  3,000 pages again after each restore, and 128 a commit couldn't.
   --  Where it is in the walk isn't saved, so after a restore it starts
   --  again from the first entry, and has to read the whole of an entry's
   --  header pages within one run of the system to free it: one of the
   --  install's entries has 456, which 64 a commit didn't manage in a
   --  four-minute run.

   Log_Table_Margin  : constant := 512;
   Log_Reserve_Pages : constant :=
                         Max_Dirty_Pages + Log_Table_Margin
                         + Clean_Copy_Limit;
   --  The checkpoint log keeps Log_Reserve_Pages free for checkpoints and
   --  cleaning (#1029). Only a commit frees space in the log, so a log
   --  too full for a checkpoint would stay full. The memory server pages
   --  nothing out that would leave less than the reserve, so once the
   --  log is that low, a checkpoint's pages are only the changed pages
   --  in memory, at most Max_Dirty_Pages, and its tables, which take
   --  less than Log_Table_Margin. Cleaning copies into the reserve, but
   --  leaves Log_Table_Margin. It doesn't grow with the machine's memory,
   --  since the changed pages are limited, not the mapped ones (#998).

   Page_Size : constant := 2 ** 12;

end Rose.Limits;
