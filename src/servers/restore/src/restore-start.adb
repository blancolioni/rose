with System;

with Rose.Console_IO;

with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Restore.Checkpoint_Log;
with Restore.Checkpoints;

package body Restore.Start is

   Next_Cap_Index : Positive := 1;

   procedure Wait_Forever;
   --  Wait for a message that never comes.  Exit_Restore does this if
   --  restore has no exit cap.

   -----------------
   -- Add_Storage --
   -----------------

   procedure Add_Storage is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin

      --  The storage server needs its backing store either way: a restore
      --  recreates regions on it, and an install reserves them.

      Rose.Console_IO.Put_Line ("restore: activating storage");

      Rose.System_Calls.Initialize_Send (Params, Add_Storage_Cap);
      Rose.System_Calls.Send_Cap (Params, Swap_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Add_Storage;

   --------------------
   -- Delete_Cap_Set --
   --------------------

   procedure Delete_Cap_Set is
   begin
      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);
   end Delete_Cap_Set;

   ------------------
   -- Exit_Restore --
   ------------------

   procedure Exit_Restore is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Exit_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
      Wait_Forever;
   end Exit_Restore;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (Restored     : Boolean;
      Updated      : Boolean := False;
      After_Resume : access procedure := null)
   is
   begin
      Rose.Console_IO.Put_Line ("restore: done");

      --  Init waits for a message without caps.  Its data word tells init
      --  whether the installed programs are running already, restored
      --  from the checkpoint: 0 if not, 1 if they are, and 2 if they are
      --  and the update boot has written this boot set to hd0, so that
      --  init restarts into it (#828).  After a restore, init replies
      --  once it has started its last boot modules, and only then can
      --  the restored processes run (see Restore.Checkpoints).

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Word
           (Params,
            Rose.Words.Word'
              (if not Restored then 0 elsif Updated then 2 else 1));
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      if Restored then
         Restore.Checkpoints.Resume_Processes;
         Rose.Console_IO.Put_Line ("restore: restored processes are running");
      end if;

      --  After an update, init waits for this second message before it
      --  takes its checkpoint, so that the checkpoint has what
      --  After_Resume did (#829).

      if Restored and then Updated then
         if After_Resume /= null then
            After_Resume.all;
         end if;

         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Init_Cap);
            Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(3));
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end if;

      Exit_Restore;
   end Finish;

   --------------
   -- Get_Caps --
   --------------

   procedure Get_Caps is
   begin
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);
      Next (Console_Cap);
      Next (Swap_Cap);
      Next (Log_Cap);
      Next (Add_Storage_Cap);
      Next (Init_Cap);
      Next (Add_Log_Device_Cap);
      Next (Log_Reader_Cap);
      Next (Restore_Process_Cap);
      Next (Resume_Process_Cap);
      Next (Restore_Interrupt_Cap);
      Next (Store_Restore_Cap);
      Next (Memory_Restore_Cap);
      Next (Restore_Derivations_Cap);
      Next (Exit_Cap);

      declare
         Delete_Cap : Rose.Capabilities.Capability;
      begin
         Next (Delete_Cap);
         Rose.System_Calls.Use_Capabilities (Delete_Cap => Delete_Cap);
      end;
   end Get_Caps;

   ----------
   -- Next --
   ----------

   procedure Next (Cap : out Rose.Capabilities.Capability) is
   begin
      Cap :=
        Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Next_Cap_Index)));
      Next_Cap_Index := Next_Cap_Index + 1;
   end Next;

   -------------
   -- Prepare --
   -------------

   procedure Prepare
     (Checkpoint : out Rose.Words.Word_64;
      Refuse     : Boolean := True)
   is
      use type Rose.Capabilities.Capability;
   begin
      Rose.Console_IO.Open (Console_Cap);

      --  Without a swap and a log partition, as on a disk whose GPT scan
      --  refused (#405), there is nothing to restore or install to.
      --  Restore exits, init is left waiting, and the system idles with
      --  this on the screen.

      if Swap_Cap = Rose.Capabilities.Null_Capability
        or else Log_Cap = Rose.Capabilities.Null_Capability
      then
         Rose.Console_IO.Put_Line
           ("restore: hd0 has no swap or log partition; nothing to "
            & "restore or install to");
         Exit_Restore;
      end if;

      --  The log server can only read the log once it has its device.

      Rose.Console_IO.Put_Line ("restore: adding log device");

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Add_Log_Device_Cap);
         Rose.System_Calls.Send_Cap (Params, Log_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      --  A checkpoint is the last complete entry in the checkpoint log.

      Checkpoint := Restore.Checkpoint_Log.Find_Checkpoint (Log_Reader_Cap);

      --  A checkpoint that this boot set can't read is neither restored
      --  nor installed over (#375), unless the person installing has
      --  chosen to erase hd0 (#836).  Restore exits before it has changed
      --  anything, and the system idles with this on the screen.

      if Refuse and then Restore.Checkpoint_Log.Refused then
         Rose.Console_IO.Put_Line
           ("restore: this boot set can't read the checkpoint on hd0, so it");
         Rose.Console_IO.Put_Line
           ("restore: neither restores it nor installs over it.  Boot the");
         Rose.Console_IO.Put_Line
           ("restore: release that wrote it, from hd0 or its install media,");
         Rose.Console_IO.Put_Line
           ("restore: or choose the install media's entry that erases hd0");
         Exit_Restore;
      end if;
   end Prepare;

   ------------------------
   -- Restore_Checkpoint --
   ------------------------

   procedure Restore_Checkpoint
     (Checkpoint : Rose.Words.Word_64;
      Restored   : out Boolean)
   is
   begin
      Rose.Console_IO.Put_Line ("restore: restoring from checkpoint");
      Restore.Checkpoints.Restore (Log_Reader_Cap, Checkpoint, Restored);
      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);
      if not Restored then
         Rose.Console_IO.Put_Line ("restore: cannot restore the checkpoint");
      end if;
   end Restore_Checkpoint;

   ------------------
   -- Wait_Forever --
   ------------------

   procedure Wait_Forever is
      use Rose.Invocation;
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
      Receive_Cap : constant Rose.Capabilities.Capability :=
                      Rose.System_Calls.Server.Create_Receive_Cap
                        (Create_Cap   => Create_Endpoint_Cap);
   begin
      Params :=
        Invocation_Record'
          (Control       =>
             Control_Word'
               (Flags          =>
                  (Receive     => True,
                   Block       => True,
                   Recv_Words  => True,
                   Recv_Caps   => True,
                   others      => False),
                others         => <>),
           Cap           => Receive_Cap,
           others        => <>);

      Invoke_Capability (Params);
   end Wait_Forever;

end Restore.Start;
