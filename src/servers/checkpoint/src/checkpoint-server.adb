with System.Storage_Elements;

with Rose.Invocation;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Checkpointer.Server;
with Rose.Interfaces.Timeout.Server;

with Rose.Interfaces.Cap.Client;
with Rose.Interfaces.Timer.Client;

package body Checkpoint.Server is

   use type System.Storage_Elements.Storage_Offset;

   Timer : Rose.Interfaces.Timer.Client.Timer_Client;

   Timeout_Cap           : Rose.Interfaces.Cap.Client.Cap_Client;
   Enter_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Leave_Checkpoint_Cap  : Rose.Capabilities.Capability;
   Memory_Checkpoint_Cap : Rose.Capabilities.Capability;
   Append_Log_Cap        : Rose.Capabilities.Capability;
   Store_Checkpoint_Cap  : Rose.Capabilities.Capability;

   Max_Table_Pages : constant := 1 + 2 * Rose.Limits.Max_Processes;
   --  The header page, and for each process its record and one page of
   --  caps (Rose.Checkpoints.Caps_Per_Page of them).  A process with more
   --  caps needs more pages; if the tables don't fit, the kernel skips
   --  the checkpoint.

   Kernel_Tables : System.Storage_Elements.Storage_Array
     (1 .. Max_Table_Pages * Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   --  The kernel writes the snapshot of its tables here when the
   --  checkpoint starts.

   procedure On_Timeout
     (Id    : in     Rose.Objects.Capability_Identifier);

   function Take_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;
   --  Checkpointer.Take_Checkpoint (#413).

   function Checkpoint return Boolean;
   --  Take a checkpoint: False if the kernel could not take its snapshot,
   --  and there is none.

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

   function Checkpoint return Boolean is
      Page_Count : Natural;
   begin

      --  The kernel's tables come first in the log entry, then the
      --  storage server's region table, then the memory server's tables
      --  and pages.  Without the kernel's tables the entry would be
      --  no use for a restore, so if the kernel can't take its snapshot
      --  there is no checkpoint this time, and the memory server keeps
      --  its dirty pages for the next one.

      if Enter_Checkpoint (Page_Count) then
         Append_Kernel_Tables (Page_Count);
         Store_Checkpoint;
         Execute_Checkpoint;
         Leave_Checkpoint;
         return True;
      else
         Rose.Console_IO.Put_Line
           ("checkpoint: kernel tables not saved; skipping checkpoint");
         return False;
      end if;
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
      Delete_Cap  : constant Rose.Capabilities.Capability := Get_Cap (5);
   begin

      Enter_Checkpoint_Cap := Get_Cap (3);
      Leave_Checkpoint_Cap := Get_Cap (4);
      Memory_Checkpoint_Cap := Get_Cap (6);
      Append_Log_Cap := Get_Cap (7);
      Store_Checkpoint_Cap := Get_Cap (8);

      Rose.Console_IO.Open (Console_Cap);
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap => Delete_Cap);

      Rose.Interfaces.Timer.Client.Open_Cap_Set
        (Client    => Timer,
         Set_Timer => Timer_Cap);

      Rose.Interfaces.Timeout.Server.Create_Server
        (Server_Context => Server_Context,
         On_Timeout     => On_Timeout'Access);
      Rose.Interfaces.Checkpointer.Server.Attach_Interface
        (Server_Context  => Server_Context,
         Take_Checkpoint => Take_Checkpoint'Access);

      Timeout_Cap :=
        Rose.Interfaces.Timer.Client.Set_Timer
          (Item         => Timer,
           Milliseconds => 30_000,
           Cap          => Rose.Interfaces.Timeout.Server.Get_On_Timeout_Cap);

   end Create_Server;

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   function Enter_Checkpoint (Page_Count : out Natural) return Boolean is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      --  The kernel writes into Kernel_Tables directly, so every page
      --  of it must be mapped and writable first.
      Kernel_Tables := (others => 0);

      Rose.System_Calls.Initialize_Send (Params, Enter_Checkpoint_Cap);
      Rose.System_Calls.Send_Buffer
        (Params, Kernel_Tables'Length, Kernel_Tables'Address, True);
      Params.Control.Flags (Rose.Invocation.Recv_Words) := True;
      Params.Control.Last_Recv_Word := 0;
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         Page_Count := 0;
         return False;
      end if;

      Page_Count := Natural (Params.Data (0));
      return True;
   end Enter_Checkpoint;

   ------------------------
   -- Execute_Checkpoint --
   ------------------------

   procedure Execute_Checkpoint is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Memory_Checkpoint_Cap);
      Rose.System_Calls.Send_Cap (Params, Append_Log_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Execute_Checkpoint;

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
      pragma Unreferenced (Id);
      Taken : Boolean with Unreferenced;
   begin
      Rose.Interfaces.Cap.Client.Destroy (Timeout_Cap);
      Rose.Interfaces.Cap.Client.Close (Timeout_Cap);

      Taken := Checkpoint;

      Timeout_Cap :=
        Rose.Interfaces.Timer.Client.Set_Timer
          (Item         => Timer,
           Milliseconds => 30_000,
           Cap          => Rose.Interfaces.Timeout.Server.Get_On_Timeout_Cap);
   end On_Timeout;

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
      return (if Checkpoint then 1 else 0);
   end Take_Checkpoint;

end Checkpoint.Server;
