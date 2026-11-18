with Rose.Addresses;
with Rose.Words;

package Mem.Log_Index is

   --  Each persistent process's index from its pages to the log pages
   --  that hold their latest copies (ADR 0028, section 2). The index is
   --  in the log: a directory page for each process, whose entries are
   --  the log pages of its leaves, and leaf pages, whose entries are the
   --  log pages of 1,024 pages of the process's address space. mem keeps
   --  the log page of each process's directory, and a cache of index
   --  pages read from the log (section 3). A checkpoint writes new copies
   --  of the leaves that changed, and of the directories that name them.

   procedure Forget (Process : Process_Id);
   --  Process's slot has a new process, which has nothing in the log.

   procedure Set_Directory
     (Process   : Process_Id;
      Directory : Rose.Words.Word_32);
   --  Process has been restored from a checkpoint, which names Directory
   --  as its index directory.

   function Find
     (Process : Process_Id;
      Page    : Rose.Addresses.Virtual_Page_Address)
      return Rose.Words.Word_32;
   --  The log page that holds the latest copy of Page of Process, or 0 if
   --  the log has none (ADR 0028, section 7).

   procedure Evict
     (Physical : out Rose.Addresses.Physical_Page_Address;
      Success  : out Boolean);
   --  Take a page out of the cache of index pages, so that its memory can
   --  be used again. Success is False if the cache has no memory.

   function Saved_Directory
     (Process : Process_Id)
      return Rose.Words.Word_32;
   --  The log page of Process's index directory, for its record in the
   --  checkpoint being written: the one that Write_Index wrote for it,
   --  or else the last one committed. 0 if the log has none of its pages.

   procedure Write_Index
     (Batch_First : Rose.Words.Word_32;
      Success     : out Boolean);
   --  Append the index pages that the checkpoint's batch changes: an
   --  index section with a leaf for each 4 MiB of a process's address
   --  space with a page in the batch, and a directory for each process
   --  with a leaf. The batch saved the dirty pages that are not shared
   --  (Mem.Page_Table.Iterate_Dirty_Pages), in order, at consecutive log
   --  pages from Batch_First. Success is False if an append failed.

   procedure End_Checkpoint (Committed : Boolean);
   --  The checkpoint's part of the entry is complete. If Committed, the
   --  directories that Write_Index wrote replace the old ones, and the
   --  index pages it wrote join the cache; otherwise they are forgotten,
   --  since the entry is left out of the log.

end Mem.Log_Index;
