with System.Storage_Elements;

with Rose.Invocation;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Checkpoints.Checkpointer.Server;
with Rose.Interfaces.Checkpoints.Storage_Checkpoint;
with Rose.Interfaces.Time.Timeout.Server;

with Rose.Interfaces.Caps.Cap.Client;
with Rose.Interfaces.Time.Timer.Client;

package body Checkpoint.Server is

   use type System.Storage_Elements.Storage_Offset;

   Timer : Rose.Interfaces.Time.Timer.Client.Timer_Client;

   Timeout_Cap           : Rose.Interfaces.Caps.Cap.Client.Cap_Client;
   Has_Timeout           : Boolean := False;
   --  The timer's control cap for the latest timeout, if it hasn't
   --  arrived yet.

   Timeout_Id            : Rose.Objects.Capability_Identifier := 0;
   --  The identifier of the latest timeout's cap. A timeout from an
   --  earlier one is ignored.
   Enter_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Leave_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Memory_Checkpoint_Cap : Rose.Capabilities.Capability;
   Append_Log_Cap        : Rose.Capabilities.Capability;
   Store_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Hold_Cap              : Rose.Capabilities.Capability;
   Last_Entry_Cap        : Rose.Capabilities.Capability;

   Max_Table_Pages : constant := 1 + 4 + 5 * Rose.Limits.Max_Processes;
   --  The header page, four pages of derivation records (the kernel's
   --  4,095 of them), and for each process its record and four pages of
   --  caps (Rose.Checkpoints.Caps_Per_Page each), on average. If the
   --  tables need more, the kernel skips the checkpoint.

   Kernel_Tables : System.Storage_Elements.Storage_Array
     (1 .. Max_Table_Pages * Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   --  The kernel writes the snapshot of its tables here when the
   --  checkpoint starts. Only its first Ready_Pages are offered to the
   --  kernel, and only they are ever touched: the memory server maps a
   --  page of this boot module's .bss when it is first touched, so the
   --  rest costs only address space (#913).

   First_Ready_Pages : constant := 64;
   --  The pages offered at the first checkpoint: about 18 processes'
   --  tables take 40 pages.

   Grow_Slack : constant := 16;
   --  The pages added beyond what the kernel says it needs, so that a
   --  few more processes don't make it ask again.

   Max_Enter_Attempts : constant := 100;
   --  How many times Enter_Checkpoint waits for the memory server to be
   --  in none of its operations before the checkpoint is skipped
   --  (#1174).

   Ready_Pages : Natural := First_Ready_Pages;
   --  The pages of Kernel_Tables offered to the kernel, which grow when
   --  the kernel says that its tables don't fit.

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier);

   function Take_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;
   --  Checkpointer.Take_Checkpoint (#413).

   procedure Request_Checkpoint
     (Id : Rose.Objects.Capability_Identifier);
   --  Checkpointer.Request_Checkpoint, which the memory server sends
   --  without waiting (ADR 0028, section 6).

   function Checkpoint (Wait_For_Commit : Boolean) return Boolean;
   --  Take a checkpoint, and set the timer for the next one. False if the
   --  kernel could not take its snapshot and there is none. If
   --  Wait_For_Commit, also False if the log didn't commit the entry
   --  (#984).

   procedure Set_Timer;
   --  Ask the timer for a timeout in 30 seconds, after every checkpoint
   --  however it was started. The timer sends a timeout without waiting,
   --  so one that arrives while the server is in a call is lost, and
   --  the next checkpoint sets the timer again (#984). The timeout set
   --  before is destroyed. If it was sent just before, it arrives with
   --  an earlier identifier, and is ignored.

   function Last_Entry return Rose.Words.Word_64;
   --  The log's last committed entry, or 0 if there is none.

   function Enter_Checkpoint (Page_Count : out Natural) return Boolean;
   --  Enter checkpoint mode, with the kernel's tables in Kernel_Tables.
   --  False if the kernel could not take the snapshot, in which case
   --  checkpoint mode was not entered.

   procedure Leave_Checkpoint;

   procedure Append_Kernel_Tables (Page_Count : Natural);
   --  Append the first Page_Count pages of Kernel_Tables to the log.

   procedure Execute_Checkpoint;

   procedure Store_Checkpoint;
   --  Have the storage server append its region table to the log.

   procedure Hold_Store;
   --  Hold the calls to the store until the checkpoint ends, but for
   --  Store_Checkpoint and mem's own calls, so that no region is made or
   --  destroyed between the store's part and mem's (#1061). Without
   --  the hold, a launch could reserve a region after the store saved
   --  its table, and add the segment to mem before mem saved its own.

   --------------------------
   -- Append_Kernel_Tables --
   --------------------------

   procedure Append_Kernel_Tables (Page_Count : Natural) is
   begin
      for Page in 1 .. Page_Count loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
            Start  : constant System.Storage_Elements.Storage_Offset :=
                       Kernel_Tables'First
                         + System.Storage_Elements.Storage_Offset (Page - 1)
                         * Rose.Limits.Page_Size;
         begin
            Rose.System_Calls.Initialize_Send (Params, Append_Log_Cap);
            Rose.System_Calls.Send_Buffer
              (Params, Rose.Limits.Page_Size,
               Kernel_Tables (Start)'Address, False);
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end loop;
   end Append_Kernel_Tables;

   ----------------
   -- Checkpoint --
   ----------------

   function Checkpoint (Wait_For_Commit : Boolean) return Boolean is
      use type Rose.Words.Word_64;
      Page_Count : Natural;
      Committed  : constant Rose.Words.Word_64 :=
                     (if Wait_For_Commit then Last_Entry else 0);
      Result     : Boolean;
   begin

      --  The kernel's tables come first in the log entry, then the
      --  storage server's region table, then the memory server's tables
      --  and pages.  Without the kernel's tables the entry would be
      --  no use for a restore, so if the kernel can't take its snapshot
      --  there is no checkpoint this time, and the memory server keeps
      --  its dirty pages for the next one.

      if Enter_Checkpoint (Page_Count) then
         Append_Kernel_Tables (Page_Count);

         --  Only now: the log, which the kernel's tables have just been
         --  appended to, has finished telling the store of the last
         --  commit, so it does not wait on the hold.

         Hold_Store;
         Store_Checkpoint;
         Execute_Checkpoint;
         Leave_Checkpoint;

         --  Leaving checkpoint mode tells the log to commit, without
         --  waiting for it. The kernel gives the log that notice before
         --  any caller (#1013), so the log answers this call only after
         --  the commit. A commit that fails leaves the last entry where
         --  it was, and the log has said why (#984).

         --  Only a caller of Take_Checkpoint waits for this. Meanwhile
         --  the server is in a call, and the sends that don't wait,
         --  from the timer and the memory server, are refused.

         Result := not Wait_For_Commit or else Last_Entry /= Committed;
      else
         Rose.Console_IO.Put_Line
           ("checkpoint: kernel tables not saved; skipping checkpoint");
         Result := False;
      end if;

      Set_Timer;
      return Result;
   end Checkpoint;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

      Console_Cap : constant Rose.Capabilities.Capability := Get_Cap (1);
      Timer_Cap   : constant Rose.Capabilities.Capability := Get_Cap (2);
   begin

      Enter_Checkpoint_Cap := Get_Cap (3);
      Leave_Checkpoint_Cap := Get_Cap (4);
      Memory_Checkpoint_Cap := Get_Cap (5);
      Append_Log_Cap := Get_Cap (6);
      Store_Checkpoint_Cap := Get_Cap (7);
      Hold_Cap := Get_Cap (8);
      Last_Entry_Cap := Get_Cap (9);

      Rose.Console_IO.Open (Console_Cap);
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);

      --  Nothing reads the cap set again (#826).

      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);

      Rose.Interfaces.Time.Timer.Client.Open_Cap_Set
        (Client    => Timer,
         Set_Timer => Timer_Cap);

      Rose.Interfaces.Time.Timeout.Server.Create_Server
        (Server_Context => Server_Context,
         On_Timeout     => On_Timeout'Access);
      Rose.Interfaces.Checkpoints.Checkpointer.Server.Attach_Interface
        (Server_Context     => Server_Context,
         Take_Checkpoint    => Take_Checkpoint'Access,
         Request_Checkpoint => Request_Checkpoint'Access);

      Set_Timer;

   end Create_Server;

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   function Enter_Checkpoint (Page_Count : out Natural) return Boolean is

      function Try (Needed : out Natural) return Boolean;
      --  Offer the kernel the first Ready_Pages of Kernel_Tables. True if
      --  it entered checkpoint mode; Needed is then the pages its tables
      --  took, and otherwise the pages they need, which is more than
      --  Ready_Pages if they didn't fit.

      ---------
      -- Try --
      ---------

      function Try (Needed : out Natural) return Boolean is
         use type Rose.Invocation.Invocation_Error;
         Params : aliased Rose.Invocation.Invocation_Record;
         Length : constant System.Storage_Elements.Storage_Count :=
                    System.Storage_Elements.Storage_Count
                      (Ready_Pages * Rose.Limits.Page_Size);
      begin
         --  The kernel waits until the memory server is in none of its
         --  operations, and then says to ask again (#1174). Each wait
         --  ends when it next receives, so a few are enough.

         for Attempt in 1 .. Max_Enter_Attempts loop

            --  The kernel writes into Kernel_Tables directly, so every
            --  page that it is offered must be mapped and writable
            --  first.

            Kernel_Tables (1 .. Length) := (others => 0);

            Rose.System_Calls.Initialize_Send
              (Params, Enter_Checkpoint_Cap);
            Rose.System_Calls.Send_Buffer
              (Params, Length, Kernel_Tables'Address, True);
            Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
            Params.Control.Last_Recv_Word := 0;
            Rose.System_Calls.Invoke_Capability (Params);

            exit when not Params.Control.Flags (Rose.Invocation.Error)
              or else Params.Control.Error
                /= Rose.Invocation.Request_Would_Block;

            if Attempt = Max_Enter_Attempts then
               Rose.Console_IO.Put_Line
                 ("checkpoint: the memory server stayed busy");
               Needed := 0;
               return False;
            end if;
         end loop;

         Needed := Natural (Params.Data (0));
         return not Params.Control.Flags (Rose.Invocation.Error);
      end Try;

   begin
      if Try (Page_Count) then
         return True;
      end if;

      --  The kernel says how many pages its tables need, and they can
      --  have more of Kernel_Tables once: before checkpoint mode, the
      --  memory server maps the new pages when they are touched (#913).

      if Page_Count <= Ready_Pages or else Page_Count > Max_Table_Pages
      then
         Page_Count := 0;
         return False;
      end if;

      Ready_Pages := Natural'Min (Page_Count + Grow_Slack, Max_Table_Pages);
      Rose.Console_IO.Put ("checkpoint: kernel tables now have ");
      Rose.Console_IO.Put (Integer (Ready_Pages));
      Rose.Console_IO.Put_Line (" pages");

      if Try (Page_Count) then
         return True;
      end if;

      Page_Count := 0;
      return False;
   end Enter_Checkpoint;

   ------------------------
   -- Execute_Checkpoint --
   ------------------------

   procedure Execute_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      --  The memory server appends to the log through its own Page_Log,
      --  and keeps this cap, to ask for a checkpoint when its table of
      --  changes fills (ADR 0028, section 6).
      Rose.System_Calls.Initialize_Send (Params, Memory_Checkpoint_Cap);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Interfaces.Checkpoints.Checkpointer.Server
           .Get_Request_Checkpoint_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Execute_Checkpoint;

   ----------------
   -- Hold_Store --
   ----------------

   procedure Hold_Store is
      use type Rose.Objects.Endpoint_Id;
      Params : aliased Rose.Invocation.Invocation_Record;
      Except : constant Rose.Objects.Endpoint_Id :=
                 Rose.Interfaces.Checkpoints.Storage_Checkpoint
                   .Checkpoint_Endpoint;
   begin
      --  The store is named by its Checkpoint endpoint, which is the
      --  endpoint that gets through, and mem by the cap of its own
      --  checkpoint endpoint. If the hold is refused, the checkpoint
      --  goes on without it, as before.

      Rose.System_Calls.Initialize_Send (Params, Hold_Cap);
      Rose.System_Calls.Send_Cap (Params, Store_Checkpoint_Cap);
      Rose.System_Calls.Send_Cap (Params, Memory_Checkpoint_Cap);
      Rose.System_Calls.Send_Word
        (Params, Rose.Words.Word (Except mod 2 ** 32));
      Rose.System_Calls.Send_Word
        (Params, Rose.Words.Word (Except / 2 ** 32));
      Rose.System_Calls.Invoke_Capability (Params);
   end Hold_Store;

   ----------------
   -- Last_Entry --
   ----------------

   function Last_Entry return Rose.Words.Word_64 is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Last_Entry_Cap);
      Rose.System_Calls.Receive_Words (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return 0;
      end if;
      return Rose.System_Calls.Get_Word_64 (Params, 0);
   end Last_Entry;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Leave_Checkpoint_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Leave_Checkpoint;

   ----------------
   -- On_Timeout --
   ----------------

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier)
   is
      use type Rose.Objects.Capability_Identifier;
      Taken : Boolean with Unreferenced;
   begin
      --  A timeout destroyed by the last checkpoint, but already sent,
      --  has nothing to do: that checkpoint set the timer again.

      if Id /= Timeout_Id or else not Has_Timeout then
         return;
      end if;

      Rose.Interfaces.Caps.Cap.Client.Close (Timeout_Cap);
      Has_Timeout := False;

      Taken := Checkpoint (Wait_For_Commit => False);
   end On_Timeout;

   ------------------------
   -- Request_Checkpoint --
   ------------------------

   procedure Request_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
      Taken : Boolean with Unreferenced;
   begin
      --  There's no reply: the memory server didn't wait for one.
      Taken := Checkpoint (Wait_For_Commit => False);
   end Request_Checkpoint;

   ---------------
   -- Set_Timer --
   ---------------

   procedure Set_Timer is
      use type Rose.Objects.Capability_Identifier;
      Cap : Rose.Capabilities.Capability;
   begin
      if Has_Timeout then
         Rose.Interfaces.Caps.Cap.Client.Destroy (Timeout_Cap);
         Rose.Interfaces.Caps.Cap.Client.Close (Timeout_Cap);
      end if;

      Timeout_Id := Timeout_Id + 1;
      Cap := Rose.Server.Create_Endpoint
        (Rose.Interfaces.Time.Timeout.On_Timeout_Endpoint, Timeout_Id);

      Timeout_Cap :=
        Rose.Interfaces.Time.Timer.Client.Set_Timer
          (Item         => Timer,
           Milliseconds => 30_000,
           Cap          => Cap);
      Has_Timeout := True;

      --  The timer has its own copy of the cap.

      Rose.System_Calls.Delete_Cap (Cap);
   end Set_Timer;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("checkpoint: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

   ----------------------
   -- Store_Checkpoint --
   ----------------------

   procedure Store_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Store_Checkpoint_Cap);
      Rose.System_Calls.Send_Cap (Params, Append_Log_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Store_Checkpoint;

   ---------------------
   -- Take_Checkpoint --
   ---------------------

   function Take_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
      pragma Unreferenced (Id);
   begin
      return (if Checkpoint (Wait_For_Commit => True) then 1 else 0);
   end Take_Checkpoint;

end Checkpoint.Server;
