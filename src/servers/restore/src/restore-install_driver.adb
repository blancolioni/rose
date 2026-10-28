--  The install media's restore server.  It restores the last checkpoint,
--  as the boot disk's does, and if there is none, or it can't be
--  restored, it installs a new system from the install media.

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.File_System.Client;

with Rose.Console_IO;
with Rose.Directories;

with Restore.Installer.Default;
with Restore.Start;

procedure Restore.Install_Driver is
   use type Rose.Words.Word_64;
   Checkpoint : Rose.Words.Word_64;
   Restored   : Boolean := False;
begin
   Restore.Start.Get_Caps;
   Restore.Start.Next (Install_Media_Cap);
   Restore.Start.Next (Log_Reset_Cap);

   Restore.Start.Prepare (Checkpoint);

   if Checkpoint /= 0 then
      Restore.Start.Restore_Checkpoint (Checkpoint, Restored);
      if not Restored then
         Rose.Console_IO.Put_Line ("restore: installing instead");
      end if;
   end if;

   if not Restored then

      --  A new system starts a new log, so that the entries of the old
      --  one are never mixed with its own.

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Log_Reset_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      Rose.Console_IO.Put_Line ("restore: creating system image");

      declare
         use Rose.Interfaces.File_System.Client;
         File_System : File_System_Client;
      begin
         Open_Cap_Set (File_System, Install_Media_Cap);
         Rose.Directories.Open_Root_File_System (File_System);
         Restore.Installer.Install
           (Source => Restore.Installer.Default.Iterate_Steps'Access);
      end;
   end if;

   Restore.Start.Finish (Restored);
end Restore.Install_Driver;
