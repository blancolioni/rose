package Rose.Limits is

   Max_Processes : constant := 64;
   --  The kernel's limit: the most processes it runs at once, boot
   --  modules included. It bounds everything that counts processes, such
   --  as the processes in a checkpoint (checkpoint, restore).

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

   Max_Mapped_Pages : constant := 8192;
   --  The pages that are mapped at once, for all processes, which the
   --  memory server keeps in a table (Mem.Page_Table). It is as many as a
   --  32 MB machine has, which the system needs as a minimum. On a
   --  machine with more, the memory server frees pages when the table is
   --  nearly full, as it does when free pages run low (Reserve_Low_Pages
   --  and Reserve_High_Pages), so it maps at most this many. A page that
   --  still doesn't fit is mapped and then not tracked, and a later fault
   --  on it fails the process (#591).

   Mapped_Page_Table_Bytes : constant := Max_Mapped_Pages * 12;
   --  The memory server's table, which is part of its writable data: a
   --  64-bit entry and a 32-bit index of the dirty pages, for each page.
   --  The kernel makes room for it when it loads the boot modules
   --  (Rose.Kernel.Boot_Module_Pages).

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
                         Max_Mapped_Pages + Log_Table_Margin
                         + Clean_Copy_Limit;
   --  The checkpoint log keeps Log_Reserve_Pages free for checkpoints and
   --  cleaning (#1029). Only a commit frees space in the log, so a log
   --  too full for a checkpoint would stay full. The memory server pages
   --  nothing out that would leave less than the reserve, so once the
   --  log is that low, a checkpoint's pages are only the changed pages
   --  in memory, at most Max_Mapped_Pages, and its tables, which take
   --  less than Log_Table_Margin. Cleaning copies into the reserve, but
   --  leaves Log_Table_Margin. If Max_Mapped_Pages comes to grow with the
   --  machine's memory (#591), so must this.

   Page_Size : constant := 2 ** 12;

end Rose.Limits;
