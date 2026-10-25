with Rose.Capabilities;

package Restore.Checkpoint_Log is

   procedure Report (Reader_Cap : Rose.Capabilities.Capability);
   --  Walk the committed entries of the checkpoint log through the
   --  Log_Reader interface at Reader_Cap, check that each one is a
   --  memory checkpoint whose header pages account for all of its
   --  pages, and write a summary to the console.

end Restore.Checkpoint_Log;
