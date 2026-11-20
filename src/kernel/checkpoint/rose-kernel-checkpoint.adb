with Rose.Boot.Console;
with Rose.Interfaces.Checkpoints.Kernel_Log;
with Rose.Invocation;

with Rose.Kernel.Processes.Snapshot;

package body Rose.Kernel.Checkpoint is

   type Log_Notice is (Create_Notice, Commit_Notice);

   Log_Started : Boolean := False;
   Log_Process : Rose.Kernel.Processes.Process_Id;
   Log_Create  : Rose.Capabilities.Capability := 0;
   Log_Append  : Rose.Capabilities.Capability with Unreferenced;
   Log_Commit  : Rose.Capabilities.Capability := 0;

   Pending     : array (Log_Notice) of Boolean := (others => False);
   --  Notices that wait for the log to receive (#1013).

   function Notice_Endpoint
     (Notice : Log_Notice)
      return Rose.Objects.Endpoint_Index
   is (Rose.Kernel.Processes.Find_Endpoint
         (Log_Process,
          (case Notice is
              when Create_Notice =>
                 Rose.Interfaces.Checkpoints.Kernel_Log.Create_Endpoint,
              when Commit_Notice =>
                 Rose.Interfaces.Checkpoints.Kernel_Log.Commit_Endpoint)));

   procedure Notify_Log (Notice : Log_Notice);
   --  Send Notice to the log now if it is waiting to receive it, or keep
   --  it until the log next receives.

   procedure Deliver (Notice : Log_Notice);
   --  Send Notice to the log, which is blocked in its receive.

   -------------
   -- Deliver --
   -------------

   procedure Deliver (Notice : Log_Notice) is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
      Cap    : constant Rose.Capabilities.Capability :=
                 (case Notice is
                     when Create_Notice => Log_Create,
                     when Commit_Notice => Log_Commit);
   begin
      Params.Control.Flags := (Send       => True,
                               others     => False);

      Params.Cap := Cap;
      Params.Endpoint :=
        (case Notice is
            when Create_Notice =>
               Rose.Interfaces.Checkpoints.Kernel_Log.Create_Endpoint,
            when Commit_Notice =>
               Rose.Interfaces.Checkpoints.Kernel_Log.Commit_Endpoint);
      Params.Identifier := 0;
      Params.Reply_Cap := 0;

      Pending (Notice) := False;

      Rose.Kernel.Processes.Send_Cap
        (From_Process_Id => Rose.Kernel.Processes.To_Process_Id (1),
         To_Process_Id   => Log_Process,
         Sender_Cap      => 0,
         Receiver_Cap    => Cap,
         Params          => Params);
   end Deliver;

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   procedure Enter_Checkpoint
     (Tables     : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean)
   is
   begin
      --  The snapshot is taken first, while the caller's page directory
      --  is still active, because Tables is in the caller's memory.
      Rose.Kernel.Processes.Snapshot.Write_Tables
        (Tables, Page_Count, Success);

      if not Success then
         Rose.Boot.Console.Put_Line
           ("kernel: checkpoint tables do not fit; not entering checkpoint");
         return;
      end if;

      Rose.Boot.Console.Checkpoint_Status (True);
      Rose.Boot.Console.Put_Line ("Entering checkpoint");
      Rose.Kernel.Processes.Enter_Checkpoint;
      Rose.Boot.Console.Put_Line ("Checkpoint mode ready");

      if Log_Started then
         Notify_Log (Create_Notice);
      end if;
   end Enter_Checkpoint;

   --------------------
   -- Has_Log_Notice --
   --------------------

   function Has_Log_Notice
     (Process  : Rose.Kernel.Processes.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Boolean
   is
      use type Rose.Kernel.Processes.Process_Id;
      use type Rose.Objects.Endpoint_Index;
   begin
      if not Log_Started or else Process /= Log_Process then
         return False;
      end if;

      for Notice in Log_Notice loop
         if Pending (Notice) then

            --  Create goes first, so a receive that only takes Commit
            --  waits for it.

            return Endpoint = 0 or else Endpoint = Notice_Endpoint (Notice);
         end if;
      end loop;

      return False;
   end Has_Log_Notice;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
   begin
      Rose.Boot.Console.Put_Line ("Leaving checkpoint");
      if Log_Started then
         Notify_Log (Commit_Notice);
      end if;
      Rose.Kernel.Processes.Leave_Checkpoint;
      Rose.Boot.Console.Checkpoint_Status (False);
   end Leave_Checkpoint;

   ----------------
   -- Notify_Log --
   ----------------

   procedure Notify_Log (Notice : Log_Notice) is
   begin
      if Pending (Notice) then
         Rose.Boot.Console.Put_Line
           ("kernel: warning: a log notice is already waiting");
         return;
      end if;

      --  The log is told only while it waits to receive. A notice sent
      --  while it is in a call of its own would replace that call's
      --  reply (#1013).

      if not Pending (Create_Notice)
        and then Rose.Kernel.Processes.Is_Blocked_On_Endpoint
          (Log_Process, Notice_Endpoint (Notice))
      then
         Deliver (Notice);
      else
         Pending (Notice) := True;
      end if;
   end Notify_Log;

   ---------------------
   -- Send_Log_Notice --
   ---------------------

   procedure Send_Log_Notice is
   begin
      for Notice in Log_Notice loop
         if Pending (Notice) then
            Deliver (Notice);
            return;
         end if;
      end loop;
   end Send_Log_Notice;

   ----------------------
   -- Set_Log_Handlers --
   ----------------------

   procedure Set_Log_Handlers
     (Create, Append, Commit : Rose.Capabilities.Capability)
   is
   begin
      Log_Process := Rose.Kernel.Processes.Current_Process_Id;
      Log_Create := Create;
      Log_Append := Append;
      Log_Commit := Commit;
      Log_Started := True;
   end Set_Log_Handlers;

end Rose.Kernel.Checkpoint;
