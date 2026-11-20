package Rose.Limits is

   Max_Processes : constant := 64;

   Max_Physical_Pages : constant := 2 ** 20 - 1;
   Max_Virtual_Pages : constant := 2 ** 20 - 1;

   Max_Mapped_Pages : constant := 8192;
   --  The pages that are mapped at once, for all processes, which the
   --  memory server keeps in a table (Mem.Page_Table). It is as many as a
   --  32 MB machine has, which the system needs as a minimum, so that
   --  machine can't fill it. A page that doesn't fit is mapped and then
   --  not tracked, and a later fault on it fails the process (#591).

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

   Page_Size : constant := 2 ** 12;

end Rose.Limits;
