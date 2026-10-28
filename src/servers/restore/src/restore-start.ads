--  What restore and restore-install share (#373): taking their caps,
--  finding and restoring the last checkpoint, and telling init.

with Rose.Words;

package Restore.Start is

   procedure Next (Cap : out Rose.Capabilities.Capability);
   --  The next cap from the set init launched the server with

   procedure Get_Caps;
   --  Take the caps that both servers have, in the order init sends
   --  them, from Console_Cap to Memory_Restore_Cap (Restore).

   procedure Prepare (Checkpoint : out Rose.Words.Word_64);
   --  Open the console, give the log server its device, and give the
   --  storage server its backing store.  Checkpoint is the location of
   --  the last complete entry in the log, or 0 if there is none.

   procedure Restore_Checkpoint
     (Checkpoint : Rose.Words.Word_64;
      Restored   : out Boolean);
   --  Restore the checkpoint at Checkpoint.  Restored is False if it
   --  can't be restored.

   procedure Finish (Restored : Boolean);
   --  Tell init whether a checkpoint was restored, and if so, let the
   --  restored processes run once init replies.  Never returns.

end Restore.Start;
