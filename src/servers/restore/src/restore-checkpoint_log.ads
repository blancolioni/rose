with Rose.Capabilities;
with Rose.Words;

package Restore.Checkpoint_Log is

   function Find_Checkpoint
     (Reader_Cap : Rose.Capabilities.Capability)
      return Rose.Words.Word_64;
   --  Walk the committed entries of the checkpoint log through the
   --  Log_Reader interface at Reader_Cap, check that each one is a
   --  complete checkpoint (the kernel's tables, the memory server's
   --  tables, and the memory server's pages), and write a summary to
   --  the console.  Return the location of the last consistent entry,
   --  which is the checkpoint to restore from, or 0 if there is none or
   --  the log is Refused.

   function Refused return Boolean;
   --  True if Find_Checkpoint found an entry that this boot set can't
   --  read: one of its tables has a version that this restore server
   --  doesn't know, or it was written with interfaces that this boot set
   --  doesn't have (ADR 0031, section 2).  Such an entry isn't skipped,
   --  as an inconsistent one is, because restoring an older entry instead
   --  would lose what was saved after it, and installing would lose the
   --  lot.  Find_Checkpoint has said why.

   procedure Put_Last_Entry;
   --  Write the location of the last entry that Find_Checkpoint found
   --  the kernel's tables in, with their version, and the interface
   --  level they were written with if this boot set reads that version.
   --  Nothing if there was no such entry.

end Restore.Checkpoint_Log;
