package Mem.Cleaning is

   --  Cleaning the checkpoint log (#356, step F; #952). The log only
   --  grows, so mem frees its oldest entries as the system runs: it walks
   --  them from the first, and copies forward the pages that are still
   --  live in them.
   --
   --  An entry describes itself (Rose.Checkpoints): batches of pages
   --  paged out between checkpoints, then the kernel's and the store's
   --  tables, which the counts in their headers skip, then batches and
   --  index sections up to mem's tables. Shared pages come last, and are
   --  not walked. A batch's header pages list each page's process and
   --  virtual page, and an index section's list each index page's
   --  process and directory slot, so whether a page is live is a lookup
   --  in the index, as a fault makes:
   --
   --  * A page of a batch is live if the index, or the table of changes,
   --    still names that copy. It is read from the log and appended to
   --    the open entry in a batch of copies, which goes into the table
   --    of changes, as a page-out does.
   --
   --  * An index page is live if its process's committed directory, or
   --    the directory itself, still names it. It is written again at the
   --    next checkpoint (Mem.Log_Index.Rewrite).
   --
   --  * Shared pages are not walked. One that anything maps is in mem's
   --    memory, and one that nothing maps has been written back to its
   --    region (ADR 0025), so every shared page that has been written is
   --    saved again by the checkpoint that frees entries.
   --
   --  Once every entry before entry X has been walked, the next
   --  checkpoint asks the log server to start the log at X when it
   --  commits (Page_Log.Free_Before). The kernel's, the store's and mem's
   --  tables are complete in every entry, so only pages hold an entry
   --  back.
   --
   --  Cleaning starts once the log is Rose.Limits.Clean_Start_Percent
   --  full, and a round reads and copies a bounded number of pages.

   procedure Clean;
   --  A round of cleaning, after the log server has committed an entry.

   procedure Prepare_Checkpoint;
   --  Before mem writes its part of a checkpoint: if entries can be freed,
   --  every shared page that has been written is saved again.

   procedure Free_Cleaned;
   --  mem has written its part of a checkpoint: ask the log server to
   --  free the entries that have been cleaned when it commits.

end Mem.Cleaning;
