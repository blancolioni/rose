--  What restore and restore-install share (#373): taking their caps,
--  finding and restoring the last checkpoint, and telling init.

with Rose.Words;

package Restore.Start is

   procedure Next (Cap : out Rose.Capabilities.Capability);
   --  The next cap from the set init launched the server with

   procedure Get_Caps;
   --  Take the caps that both servers have, in the order init sends
   --  them, from Console_Cap to Exit_Cap (Restore), and then the delete
   --  cap.

   procedure Delete_Cap_Set;
   --  Delete the cap to the set, once every cap has been taken from it
   --  (#826).

   procedure Prepare
     (Checkpoint : out Rose.Words.Word_64;
      Refuse     : Boolean := True);
   --  Open the console, give the log server its device, and find the
   --  last checkpoint.  Checkpoint is the location of the last complete
   --  entry in the log, or 0 if there is none.  If the log has an entry
   --  that this boot set can't read (Restore.Checkpoint_Log.Refused),
   --  restore says so and exits, unless Refuse is False: then Checkpoint
   --  is 0, for restore-install to ask whether to erase it (#836).

   procedure Add_Storage;
   --  Give the storage server its backing store, which a restore and an
   --  install both need.  Nothing on hd0 has changed before this.

   procedure Exit_Restore;
   --  End restore, and with it the caps it was given (#451), without
   --  telling init, which is left waiting.  The system idles with what
   --  restore last wrote on the screen.

   procedure Restore_Checkpoint
     (Checkpoint : Rose.Words.Word_64;
      Restored   : out Boolean);
   --  Restore the checkpoint at Checkpoint.  Restored is False if it
   --  can't be restored.

   procedure Finish (Restored : Boolean);
   --  Tell init whether a checkpoint was restored, and if so, let the
   --  restored processes run once init replies.  Never returns.

end Restore.Start;
