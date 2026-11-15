with Rose.Words;

with Rose.Interfaces.Checkpoints.Log_Reader.Client;

package Restore.Entries is

   --  Where the parts of a checkpoint log entry are (Rose.Checkpoints).
   --  Pages are numbered from 1, as Log_Reader.Read_Page numbers them.
   --  The memory server's tables come after its pages and index pages
   --  (ADR 0028), so finding them means skipping those by the counts in
   --  their header pages.

   type Entry_Layout is
      record
         Page_Count   : Natural := 0;
         --  The entry's pages, not counting its header.
         Kernel_Pages : Natural := 0;
         Store_Pages  : Natural := 0;
         --  The kernel's tables start at page 1, and the storage
         --  server's follow them.
         Batch_First  : Positive := 1;
         --  The first page after the storage server's tables, which
         --  starts the first batch of the memory server's pages.
         Mem_First    : Positive := 1;
         Mem_Pages    : Natural := 0;
         --  The memory server's tables.  The batches and index sections
         --  are the pages from Batch_First to Mem_First - 1.
         Has_Shared   : Boolean := False;
         Shared_First : Positive := 1;
         --  The first shared header page, if Has_Shared.
      end record;

   type Layout_Status is
     (Found,
      Other_Version,
      --  The memory server's tables are at Mem_First, but they are not
      --  the version this restore reads, so nothing else is known.
      No_Kernel_Tables,
      No_Store_Tables,
      No_Memory_Tables,
      No_Batch,
      Bad_Section,
      Runs_Past_Entry);

   procedure Find_Layout
     (Reader   : Rose.Interfaces.Checkpoints.Log_Reader.Client
                   .Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Layout   : out Entry_Layout;
      Status   : out Layout_Status);
   --  Find the parts of the entry at Location.  Only the tables' header
   --  pages and the header pages of the batches and index sections are
   --  read, and only their magic numbers and counts are checked.

   type Section_Kind is (Memory_Batch, Index_Section);

   procedure Section_Size
     (Reader   : Rose.Interfaces.Checkpoints.Log_Reader.Client
                   .Log_Reader_Client;
      Location : Rose.Words.Word_64;
      First    : Positive;
      Last     : Natural;
      Kind     : out Section_Kind;
      Size     : out Natural;
      Status   : out Layout_Status);
   --  The kind of the batch or index section that starts at page First
   --  of the entry at Location, and its size: its header pages and the
   --  pages they list.  It must end by page Last.

end Restore.Entries;
