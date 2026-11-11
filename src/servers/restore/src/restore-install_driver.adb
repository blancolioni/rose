--  The install media's restore server.  It restores the last checkpoint,
--  as the boot disk's does, and if there is none, or it can't be
--  restored, it installs a new system from the install media.  A
--  checkpoint that this boot set can't read stops it before either
--  (Restore.Start.Prepare, #375).
--
--  The install media's erasing entry puts erase-hd0 on the kernel's
--  command line (#836).  Then restore-install doesn't restore: it asks
--  before it installs over a checkpoint, readable or not, and if the
--  answer isn't "erase", it exits with hd0 unchanged, and the system
--  idles.
--
--  The update entry puts update-hd0 there (#828, ADR 0031).  Then
--  restore-install never installs a new system.  It restores hd0's
--  checkpoint with this boot set, and only if that works, writes the set
--  to hd0's boot partition (Restore.Boot_Sets).  Once the restored
--  processes run, it installs the commands again in the restored exec
--  (#829), and init then restarts into the new set.  If anything stops
--  it before the set is written, hd0 still boots its old set.

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.File_System.Client;

with Rose.Console_IO;
with Rose.Directories;

with Restore.Ask;
with Restore.Boot_Disk;
with Restore.Boot_Sets;
with Restore.Checkpoint_Log;
with Restore.Command_Line;
with Restore.Installer.Default;
with Restore.Start;

procedure Restore.Install_Driver is
   use type Rose.Words.Word_64;
   Checkpoint : Rose.Words.Word_64;
   Restored   : Boolean := False;
   Erasing    : Boolean;
   Updating   : Boolean;

   Install_Media : Rose.Interfaces.File_System.Client.File_System_Client;

   procedure Open_Install_Media;
   --  Open the install media's file system, for Rose.Directories

   procedure Update;
   --  The update boot: restore the checkpoint, write this boot set to
   --  hd0, and tell init.  Exits with hd0 unchanged if any of these
   --  fails.  Never returns.

   procedure Install_Commands_Again;
   --  Install the install media's commands in the restored exec and
   --  registry (#829), once the restored processes are running.

   ----------------------------
   -- Install_Commands_Again --
   ----------------------------

   procedure Install_Commands_Again is
   begin
      Restore.Installer.Update
        (File_System => Install_Media,
         Source      => Restore.Installer.Default.Iterate_Steps'Access);
   end Install_Commands_Again;

   ------------------------
   -- Open_Install_Media --
   ------------------------

   procedure Open_Install_Media is
      use Rose.Interfaces.File_System.Client;
   begin
      Open_Cap_Set (Install_Media, Install_Media_Cap);
      Rose.Directories.Open_Root_File_System (Install_Media);
   end Open_Install_Media;

   ------------
   -- Update --
   ------------

   procedure Update is
      Partition : Restore.Boot_Sets.Boot_Partition;
      Found     : Boolean;
      Written   : Boolean;
   begin
      if Checkpoint = 0 then
         Rose.Console_IO.Put_Line
           ("restore: hd0 has no checkpoint to update; install Rose "
            & "instead");
         Restore.Start.Exit_Restore;
      end if;

      --  Before anything changes, check that hd0's boot partition has
      --  two slots, and find the current one.

      Restore.Boot_Sets.Find (Partition, Found);
      if not Found then
         Rose.Console_IO.Put_Line ("restore: hd0 is unchanged");
         Restore.Start.Exit_Restore;
      end if;

      --  Restoring with this boot set before writing it tests the set
      --  against this disk's own checkpoint.

      Restore.Start.Add_Storage;
      Restore.Start.Restore_Checkpoint (Checkpoint, Restored);
      if not Restored then
         Rose.Console_IO.Put_Line
           ("restore: this boot set can't restore hd0's checkpoint, so "
            & "hd0 is unchanged");
         Restore.Start.Exit_Restore;
      end if;

      Open_Install_Media;
      Restore.Boot_Sets.Write_Update (Partition, Written);
      if not Written then
         Restore.Start.Exit_Restore;
      end if;

      Restore.Start.Finish
        (Restored     => True,
         Updated      => True,
         After_Resume => Install_Commands_Again'Access);
   end Update;

begin
   Restore.Start.Get_Caps;
   Restore.Start.Next (Install_Media_Cap);
   Restore.Start.Next (Log_Reset_Cap);
   Restore.Start.Next (MBR_Cap);
   Restore.Start.Next (BIOS_Boot_Cap);
   Restore.Start.Next (Boot_Partition_Cap);
   Restore.Start.Next (Timer_Cap);
   Restore.Start.Next (Keyboard_Status_Cap);
   Restore.Start.Next (Keyboard_Data_Cap);
   Restore.Start.Next (Command_Line_Cap);
   Restore.Start.Next (Restored_Endpoint_Cap);
   Restore.Start.Delete_Cap_Set;

   Erasing := Restore.Command_Line.Has_Argument ("erase-hd0");
   Updating := not Erasing
     and then Restore.Command_Line.Has_Argument ("update-hd0");

   Restore.Start.Prepare (Checkpoint, Refuse => not Erasing);

   if Updating then
      Update;
   end if;

   if Erasing
     and then (Checkpoint /= 0 or else Restore.Checkpoint_Log.Refused)
   then
      Restore.Checkpoint_Log.Put_Last_Entry;
      if not Restore.Ask.Confirm
          (Question =>
             "restore: type erase and press enter to erase it and install "
           & "a new system: ",
           Answer   => "erase")
      then
         Rose.Console_IO.Put_Line
           ("restore: hd0 is unchanged.  Restart the machine to boot "
            & "something else");
         Restore.Start.Exit_Restore;
      end if;
      Rose.Console_IO.Put_Line ("restore: erasing hd0");
      Checkpoint := 0;
   end if;

   Restore.Start.Add_Storage;

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
           (File_System => File_System,
            Source      => Restore.Installer.Default.Iterate_Steps'Access);
      end;

      --  So that the disk boots without the install media.  A new
      --  system writes the whole boot partition, with this boot set in
      --  slot a; an update writes only a slot and the menu (Update).

      Restore.Boot_Disk.Write_Boot_Loader;
   end if;

   Restore.Start.Finish (Restored);
end Restore.Install_Driver;
