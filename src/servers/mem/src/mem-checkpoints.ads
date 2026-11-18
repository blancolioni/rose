package Mem.Checkpoints is

   procedure Checkpoint;
   --  Append mem's part of the checkpoint to the log's open entry (ADR
   --  0028, section 4): the batch of changed pages, the index pages that
   --  name them, mem's tables, and the pages of shared segments. If an
   --  append fails, the log drops the checkpoint's pages, and the changed
   --  pages are saved by the next checkpoint.

end Mem.Checkpoints;
