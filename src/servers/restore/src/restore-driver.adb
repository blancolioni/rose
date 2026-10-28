--  The boot disk's restore server (#373).  It restores the last
--  checkpoint, and has no installer: if there is nothing to restore, it
--  says so, and init stops.

with Rose.Words;

with Rose.Console_IO;

with Restore.Start;

procedure Restore.Driver is
   use type Rose.Words.Word_64;
   Checkpoint : Rose.Words.Word_64;
   Restored   : Boolean := False;
begin
   Restore.Start.Get_Caps;
   Restore.Start.Prepare (Checkpoint);

   if Checkpoint /= 0 then
      Restore.Start.Restore_Checkpoint (Checkpoint, Restored);
   end if;

   if not Restored then
      Rose.Console_IO.Put_Line ("restore: no system on this disk");
   end if;

   Restore.Start.Finish (Restored);
end Restore.Driver;
