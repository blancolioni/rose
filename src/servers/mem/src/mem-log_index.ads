with Rose.Words;

package Mem.Log_Index is

   --  Each persistent process's index from its pages to the log pages
   --  that hold their latest copies (ADR 0028, section 2). The index is
   --  in the log: a directory page for each process, whose entries are
   --  the log pages of its leaves, and leaf pages, whose entries are the
   --  log pages of 1,024 pages of the process's address space. mem keeps
   --  only the log page of each process's directory. A checkpoint writes
   --  new copies of the leaves that changed, and of the directories that
   --  name them.

   procedure Forget (Process : Process_Id);
   --  Process's slot has a new process, which has nothing in the log.

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
   --  directories that Write_Index wrote replace the old ones; otherwise
   --  they are forgotten, since the entry is left out of the log.

end Mem.Log_Index;
