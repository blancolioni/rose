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
   --  which is the checkpoint to restore from, or 0 if there is none.

end Restore.Checkpoint_Log;
