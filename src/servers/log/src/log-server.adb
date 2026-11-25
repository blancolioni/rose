with System.Storage_Elements;

with Rose.Console_IO;
with Rose.Invocation;
with Rose.Limits;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Storage.Block_Array.Client;
with Rose.Interfaces.Checkpoints.Kernel_Log.Server;
with Rose.Interfaces.Checkpoints.Log_Reader.Server;
with Rose.Interfaces.Checkpoints.Page_Log.Server;
with Rose.Interfaces.Checkpoints.State.Server;

with Rose.Server;
with Rose.System_Calls.Client;

with Log.Header;

package body Log.Server is

   use type System.Storage_Elements.Storage_Count;

   use Rose.Interfaces.Storage.Block_Array;
   subtype Block_Array_Client is
     Rose.Interfaces.Storage.Block_Array.Client.Block_Array_Client;

   Max_Log_Devices : constant := 4;

   type Log_Device_Count is range 0 .. Max_Log_Devices;
   subtype Log_Device_Index is
     Log_Device_Count range 1 .. Log_Device_Count'Last;

   type Log_Device_Record is
      record
         Block_Count    : Block_Address_Type;
         Block_Size     : Block_Size_Type;
         Multiplier     : Positive;
         Client         : Block_Array_Client;
         First_Location : Log_Location_Id;
         Last_Location  : Log_Location_Id;
      end record;

   Log_Devices : array (Log_Device_Index) of Log_Device_Record;
   Last_Log_Device : Log_Device_Count := 0;

   Log_Server   : Rose.Server.Server_Context;

   Active             : Boolean := False;
   Writing            : Boolean := False;
   Entry_Failed       : Boolean := False;
   --  Set when a write to the disk fails.  log can't tell which of the
   --  entry's pages reached the disk, so Commit leaves the whole entry
   --  out of the log, and the previous entry is still the last one.

   Append_Failed      : Boolean := False;
   --  Set when an append fails, because the log is full or the page is
   --  the wrong size, or when the memory server fails the checkpoint.
   --  Only the pages since the checkpoint started, or since the memory
   --  server's batch started, are lost (ADR 0028, section 11): Commit
   --  rolls the entry back to Checkpoint_Mark, and between checkpoints
   --  the memory server drops its batch with Page_Log.Discard_Pages.
   --  Appends fail until then.

   Full_Reported      : Boolean := False;
   Below_Reserve      : Boolean := False;
   --  Commit has said that the log is full, or that fewer pages than
   --  Rose.Limits.Log_Reserve_Pages are free, and hasn't said yet that
   --  this is over. Each is said once, and not at every checkpoint
   --  (#1029).

   Checkpoint_Mark    : Log_Location_Id := 0;
   --  The open entry's last location when the checkpoint being written
   --  started, or 0 if no checkpoint is being written.  An entry with no
   --  pages ends at its header, so a mark is never 0.

   --  Nothing is reported to the console when a problem happens.  The
   --  kernel's Append and Create, and Page_Log, are on the checkpoint's
   --  path, and mem calls Page_Log while it handles a fault.  A message
   --  can wait on a persistent console that checkpoint mode has blocked,
   --  or that is waiting for mem, and the system would hang (#971).
   --  Commit reports them instead, when the kernel is leaving checkpoint
   --  mode.

   type Problem_Type is
     (No_Problem, No_Open_Entry, Bad_Page_Size, Log_Full,
      Failed_By_Memory_Server, No_Checkpoint_To_Fail,
      Cannot_Write_Pages, Cannot_Write_Header, No_Checkpoint_Started,
      Unreported_Failed_Append, Bad_Read, Bad_Discard,
      Discard_During_Checkpoint, Bad_Free, Cannot_Write_Log_Header);

   Failure       : Problem_Type := No_Problem;
   --  Why the open entry or the checkpoint failed, if it did.

   Other_Problem : Problem_Type := No_Problem;
   Problem_Page  : Log_Location_Id := 0;
   --  The first problem since the last commit that failed nothing, and
   --  the log page it was about, or 0.

   Current_Log_Header : Log.Header.Entry_Header_Type;
   Header             : Log.Header.Log_Header_Type;
   Next_Log_Location  : Log_Location_Id;
   Next_Sequence      : Rose.Words.Word_64 := 1;
   --  Where the open entry's header goes, and its sequence number.

   First_Committed    : Log_Location_Id := 0;
   Last_Committed     : Log_Location_Id := 0;
   --  The locations of the first and last committed entries, or 0 if
   --  there are none.  Readers only see entries in this range.  The log
   --  header's First_Entry is First_Committed if there is a committed
   --  entry, and otherwise where the first entry will go.

   New_First_Entry    : Log_Location_Id := 0;
   New_First_Sequence : Rose.Words.Word_64 := 0;
   --  The entry that Free_Before asked the next commit to start the log
   --  at, and its sequence number, or 0.

   First_Restorable   : Log_Location_Id := 0;
   --  The first committed entry whose header's Start_Sequence is the
   --  log's start, or 0 if there is none: only it and the entries after
   --  it can be restored (Log_Reader.First_Checkpoint, #953).

   --  The log's locations run from 1 to the end of the last device, and
   --  then wrap around to 1 (#951).  An entry is its header and the
   --  pages after it, which can wrap.  Space is free from the open
   --  entry's header up to the log header's First_Entry.

   Cached_Location    : Log_Location_Id := 0;
   Cached_Header      : Log.Header.Entry_Header_Type;
   --  The header of the committed entry at Cached_Location, if that is
   --  not 0, so that reading an entry's pages doesn't read its header
   --  each time.

   Log_Page_Buffer_Count : constant := 8;
   type Page_Buffer_Count is range 0 .. Log_Page_Buffer_Count;
   subtype Page_Buffer_Index is
     Page_Buffer_Count range 1 .. Page_Buffer_Count'Last;

   subtype Log_Page is
     System.Storage_Elements.Storage_Array (1 .. Log_Page_Size);

   Page_Buffer           : array (Page_Buffer_Index) of Log_Page;
   Page_Buffer_Last      : Page_Buffer_Count := 0;
   Page_Buffer_Loc       : Log_Location_Id;

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Log_Page_Buffer_Count * Log_Page_Size)
     with Alignment => 4096;

   procedure Flush;

   procedure Reset
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Create
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Commit
     (Id          : in     Rose.Objects.Capability_Identifier);

   procedure Append
     (Id   : Rose.Objects.Capability_Identifier;
      Data : System.Storage_Elements.Storage_Array);

   function Append_Data
     (Data : System.Storage_Elements.Storage_Array)
      return Log_Location_Id;
   --  Append Data to the open entry, and return the location it was
   --  written to, or 0 if the entry has failed.

   procedure Open_Entry;
   --  Open the next entry, at Next_Log_Location.  An entry is open from
   --  one commit to the next (ADR 0028, section 4), so that the memory
   --  server can append pages between checkpoints.

   function Append_Page
     (Id   : Rose.Objects.Capability_Identifier;
      Page : System.Storage_Elements.Storage_Array)
      return Rose.Words.Word;
   --  Page_Log.Append_Page.

   procedure Append_Batch
     (Id    : Rose.Objects.Capability_Identifier;
      Pages : System.Storage_Elements.Storage_Array;
      First : out Rose.Words.Word;
      Wrap  : out Rose.Words.Word);
   --  Page_Log.Append_Batch.

   procedure Read_Log_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Log_Page : Rose.Words.Word;
      Page     : out System.Storage_Elements.Storage_Array);
   --  Page_Log.Read_Page.

   procedure Add_Log_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Device : Rose.Capabilities.Capability);

   function Get_Device_Index
     (Location : Log_Location_Id)
      return Log_Device_Count;

   function Last_Location return Log_Location_Id
   is (if Last_Log_Device = 0 then 0
       else Log_Devices (Last_Log_Device).Last_Location);
   --  The log's last location, after which it wraps around to 1.

   function Is_Location (Location : Log_Location_Id) return Boolean
   is (Location in 1 .. Last_Location);
   --  True if Location is a page of the log other than its header.

   function Next_Location
     (Location : Log_Location_Id)
      return Log_Location_Id
   is (if Location >= Last_Location then 1 else Location + 1);

   function Previous_Location
     (Location : Log_Location_Id)
      return Log_Location_Id
   is (if Location <= 1 then Last_Location else Location - 1);

   function Distance
     (From, To : Log_Location_Id)
      return Log_Location_Id
   is (if To >= From then To - From else Last_Location - From + To);
   --  How many locations To is after From, going forwards round the log.

   function Advance
     (Location : Log_Location_Id;
      Count    : Log_Location_Id)
      return Log_Location_Id
   is (if Count <= Last_Location - Location then Location + Count
       else Count - (Last_Location - Location));
   --  The location Count after Location, going forwards round the log,
   --  where Count is less than the log's size.  (A 64-bit mod would need
   --  libgcc, which boot servers don't link.)

   function In_Range
     (Location    : Log_Location_Id;
      First, Last : Log_Location_Id)
      return Boolean
   is (Is_Location (Location)
       and then Distance (First, Location) <= Distance (First, Last));
   --  True if Location is one of First .. Last, going forwards round the
   --  log from First.

   function Is_Log_Entry
     (Location     : Log_Location_Id;
      Entry_Header : Log.Header.Entry_Header_Type;
      Sequence     : Rose.Words.Word_64)
      return Boolean;
   --  True if Entry_Header, read from Location, is an entry of this
   --  generation of the log with sequence number Sequence, or with any
   --  sequence number if Sequence is 0.

   function Find_Entry
     (Location : Log_Location_Id;
      Found    : out Log_Location_Id;
      Sequence : out Rose.Words.Word_64)
      return Boolean;
   --  If Location is a page of a committed entry, or of the open one,
   --  set Found to that entry's location and Sequence to its sequence
   --  number, and return True.

   function Readable (Location : Log_Location_Id) return Boolean;
   --  True if Location is a page of a committed entry or of the open
   --  one.

   procedure Free_Before
     (Id       : Rose.Objects.Capability_Identifier;
      Log_Page : Rose.Words.Word);
   --  Page_Log.Free_Before.

   procedure Get_Space
     (Id    : Rose.Objects.Capability_Identifier;
      First : out Rose.Words.Word;
      Open  : out Rose.Words.Word;
      Last  : out Rose.Words.Word;
      Free  : out Rose.Words.Word);
   --  Page_Log.Get_Space.

   function Free_Pages return Log_Location_Id;
   --  How many pages can be appended before the log is full.

   procedure Report_Space;
   --  After a commit: say once that the log has room again after it was
   --  full, and once when the free pages go below the reserve or back
   --  above it (#1029).

   function Entry_Last
     (Id     : Rose.Objects.Capability_Identifier;
      Header : Rose.Words.Word)
      return Rose.Words.Word;
   --  Page_Log.Entry_Last.

   procedure Move_First_Entry;
   --  Start the log at New_First_Entry, once the entry is committed.

   procedure Finish_Start
     (Last_Start : Rose.Words.Word_64;
      Run_First  : Log_Location_Id);
   --  After the startup scan: Last_Start is the last committed entry's
   --  Start_Sequence, and Run_First the first entry of the run that
   --  ends with it, in which every Start_Sequence is Last_Start or 0.
   --  If the log's header doesn't start at Last_Start yet, because the
   --  system stopped between writing that entry's header and the log's
   --  (#953), move it there now: the entry is on the medium, and its
   --  pages are all from there on.  Then set First_Restorable.

   procedure Fail_Entry (Problem : Problem_Type);
   --  Leave the whole open entry out of the log, because a write to the
   --  disk failed.

   procedure Fail_Append (Problem : Problem_Type);
   --  Fail appends until the checkpoint commits or the memory server
   --  discards its batch.

   procedure Note_Problem
     (Problem : Problem_Type;
      Page    : Log_Location_Id := 0);
   --  Keep Problem for Commit to report, if it is the first.

   procedure Put_Problem (Problem : Problem_Type);
   --  Write a description of Problem, with no new line.

   procedure Report_Problems;
   --  Report the failure and the other problem, if any, and forget them.

   procedure Discard_After (Last : Log_Location_Id);
   --  Drop the open entry's pages after location Last, which is at or
   --  after its header.

   procedure Fail_Checkpoint
     (Id : Rose.Objects.Capability_Identifier);
   --  Page_Log.Fail_Checkpoint.

   procedure Discard_Pages
     (Id   : Rose.Objects.Capability_Identifier;
      From : Rose.Words.Word);
   --  Page_Log.Discard_Pages.

   procedure Sync_Devices;
   --  Return when everything written to the log devices is on the medium,
   --  and not only in a disk's write cache (#505).

   procedure Notify_Mem_Committed;
   --  Tell the memory server that an entry is committed, without
   --  waiting: it cleans the log then, and calls the log server to
   --  do it (#952).

   procedure Notify_Store_Committed;
   --  Tell the store that the entry is committed, and wait for it.

   function Load_Entry (Location : Log_Location_Id) return Boolean;
   --  If Location is a committed entry, put its header in Cached_Header
   --  and return True.

   function First_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Last_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function First_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64;

   function Next_Entry
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Rose.Words.Word_64;

   function Page_Count
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Natural;

   procedure Read_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64;
      Index    : Positive;
      Page     : out System.Storage_Elements.Storage_Array);

   type Device_Operation is (Read, Write);

   procedure Operate
     (Location   : Log_Location_Id;
      Addr       : System.Address;
      Operation  : Device_Operation;
      Page_Count : Natural;
      Success    : out Boolean);

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean);

   procedure Write
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean);

   --------------------
   -- Add_Log_Device --
   --------------------

   procedure Add_Log_Device
     (Id     : Rose.Objects.Capability_Identifier;
      Device : Rose.Capabilities.Capability)
   is
   begin
      if Last_Log_Device < Log_Device_Count'Last then
         Last_Log_Device := Last_Log_Device + 1;

         declare
            Client      : Block_Array_Client;
            Block_Count : Block_Address_Type;
            Block_Size  : Block_Size_Type;
            First_Location : Log_Location_Id := 0;
            Last_Location  : Log_Location_Id := 0;
            Page_Count     : Log_Location_Id := 0;
         begin
            Rose.Interfaces.Storage.Block_Array.Client.Open (Client, Device);
            Rose.Interfaces.Storage.Block_Array.Client.Get_Parameters
              (Item        => Client,
               Block_Count => Block_Count,
               Block_Size  => Block_Size);

            Page_Count := Log_Location_Id (Block_Count)
              * Log_Location_Id (Block_Size) / Log_Page_Size;

            if Last_Log_Device > 1 then
               First_Location :=
                 Log_Devices (Last_Log_Device - 1).Last_Location + 1;
            end if;

            Last_Location := First_Location + Page_Count - 1;

            Log_Devices (Last_Log_Device) :=
              Log_Device_Record'
                (Block_Count    => Block_Count,
                 Block_Size     => Block_Size,
                 Multiplier     =>
                   Natural'Max (1, 4096 / Natural (Block_Size)),
                 Client         => Client,
                 First_Location => First_Location,
                 Last_Location  => Last_Location);

            Rose.Console_IO.Put ("log: added device ");
            Rose.Console_IO.Put (Natural (Last_Log_Device));
            Rose.Console_IO.Put (": block size ");
            Rose.Console_IO.Put (Natural (Block_Size));
            Rose.Console_IO.Put ("/");
            Rose.Console_IO.Put (Log_Devices (Last_Log_Device).Multiplier);
            Rose.Console_IO.Put ("; count ");
            Rose.Console_IO.Put (Natural (Block_Count));
            Rose.Console_IO.Put ("; range ");
            Rose.Console_IO.Put (Natural (First_Location));
            Rose.Console_IO.Put ("..");
            Rose.Console_IO.Put (Natural (Last_Location));
            Rose.Console_IO.New_Line;

         end;

         if Last_Log_Device = 1 then

            declare
               use type Rose.Words.Word_64;
               Success    : Boolean;
               Last_Start : Rose.Words.Word_64 := 0;
               Run_First  : Log_Location_Id := 0;
            begin
               Read (0, Header'Address, Success);
               --  A header from before version 3 has no sequence number,
               --  and its entries are of an older version, so a new log
               --  replaces it.

               if Success
                 and then Header.Magic = Log.Header.Log_Magic
                 and then Header.First_Sequence /= 0
                 and then Is_Location (Header.First_Entry)
               then
                  Rose.Console_IO.Put_Line ("log: found valid log device");

                  --  The log starts at the header's First_Entry, and each
                  --  entry is followed by the next, with the next sequence
                  --  number, until a page that isn't.  A header from an
                  --  earlier lap of the log has an older sequence number.

                  Next_Log_Location := Header.First_Entry;
                  Next_Sequence := Header.First_Sequence;
                  loop
                     Read (Next_Log_Location, Current_Log_Header'Address,
                           Success);
                     exit when not Success
                       or else not Is_Log_Entry
                         (Next_Log_Location, Current_Log_Header,
                          Next_Sequence);

                     Rose.Console_IO.Put
                       ("log: skipping entry at location ");
                     Rose.Console_IO.Put (Natural (Next_Log_Location));
                     Rose.Console_IO.New_Line;

                     if First_Committed = 0 then
                        First_Committed := Next_Log_Location;
                        Run_First := Next_Log_Location;
                     end if;
                     Last_Committed := Next_Log_Location;

                     --  An entry with another start begins a new run;
                     --  one that didn't record its start (0) carries on
                     --  the run it is in.

                     declare
                        Start : constant Rose.Words.Word_64 :=
                                  Current_Log_Header.Start_Sequence;
                     begin
                        if Start /= 0 then
                           if Last_Start /= 0 and then Start /= Last_Start
                           then
                              Run_First := Next_Log_Location;
                           end if;
                           Last_Start := Start;
                        end if;
                     end;

                     Next_Log_Location :=
                       Next_Location (Current_Log_Header.Log_Last);
                     Next_Sequence := Next_Sequence + 1;

                     if Next_Log_Location = Header.First_Entry then
                        Rose.Console_IO.Put_Line ("log: log is full");
                        exit;
                     end if;
                  end loop;

                  Rose.Console_IO.Put
                    ("log: next log at location ");
                  Rose.Console_IO.Put (Natural (Next_Log_Location));
                  Rose.Console_IO.New_Line;

                  Finish_Start (Last_Start, Run_First);

               else

                  Rose.Console_IO.Put_Line ("log: creating new log");
                  Reset (Id);

               end if;

               Active := True;
               Open_Entry;

            end;

            declare
               use Rose.Interfaces.Checkpoints.Kernel_Log.Server;
               Params : aliased Rose.Invocation.Invocation_Record;
            begin
               Rose.System_Calls.Initialize_Send
                 (Params, Start_Log_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Create_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Append_Cap);
               Rose.System_Calls.Send_Cap (Params, Get_Commit_Cap);
               Rose.System_Calls.Invoke_Capability (Params);
            end;

         end if;

         Rose.Console_IO.Put_Line ("log: ready");

      end if;
   end Add_Log_Device;

   ------------
   -- Append --
   ------------

   procedure Append
     (Id   : Rose.Objects.Capability_Identifier;
      Data : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
   begin
      --  A failed entry has said so already, and is left out when the
      --  checkpoint commits.
      if Append_Data (Data) = 0 then
         null;
      end if;
   end Append;

   ------------------
   -- Append_Batch --
   ------------------

   procedure Append_Batch
     (Id    : Rose.Objects.Capability_Identifier;
      Pages : System.Storage_Elements.Storage_Array;
      First : out Rose.Words.Word;
      Wrap  : out Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      use System.Storage_Elements;
      use type Rose.Words.Word;
      Count : constant Storage_Count := Pages'Length / Log_Page_Size;
      Start : constant Log_Location_Id := Current_Log_Header.Log_Last;
      --  Where the batch is dropped back to if an append fails.
      Location : Log_Location_Id;
   begin
      First := 0;
      Wrap := 0;

      --  A checkpoint being written owns the end of the entry, and the
      --  memory server's batch waits for the next one. That is not a
      --  problem to report.

      if not Writing or else Checkpoint_Mark /= 0 then
         return;
      end if;

      if Count = 0 or else Pages'Length mod Log_Page_Size /= 0 then
         Note_Problem (Bad_Page_Size);
         return;
      end if;

      for Index in 0 .. Count - 1 loop
         Location :=
           Append_Data
             (Pages (Pages'First + Index * Log_Page_Size
                     .. Pages'First + (Index + 1) * Log_Page_Size - 1));

         if Location = 0 then

            --  As Discard_Pages does: the batch is dropped, and its
            --  failure fails nothing else, unless a write to the disk
            --  failed, which fails the entry.

            if Writing then
               Discard_After (Start);
            end if;
            if Append_Failed and then not Entry_Failed then
               Note_Problem (Failure);
               Failure := No_Problem;
               Append_Failed := False;
            end if;
            First := 0;
            Wrap := 0;
            return;
         end if;

         if Index = 0 then
            First := Rose.Words.Word (Location);
         elsif Location = 1 and then Wrap = 0 then
            Wrap := Rose.Words.Word (Index);
         end if;
      end loop;
   end Append_Batch;

   -----------------
   -- Append_Data --
   -----------------

   function Append_Data
     (Data : System.Storage_Elements.Storage_Array)
      return Log_Location_Id
   is
   begin
      if not Writing then
         Note_Problem (No_Open_Entry);
         return 0;
      end if;

      if Entry_Failed or else Append_Failed then
         return 0;
      end if;

      if Data'Length /= Log_Page_Size then
         Fail_Append (Bad_Page_Size);
         return 0;
      end if;

      declare
         Location : constant Log_Location_Id :=
                      Next_Location (Current_Log_Header.Log_Last);
      begin
         --  The space from the log's first entry on is still in use.

         if Location = Header.First_Entry then
            Fail_Append (Log_Full);
            return 0;
         end if;

         Current_Log_Header.Log_Last := Location;

         --  The buffer is written to the disk in one go, so its pages
         --  have to be next to each other on one device: not on both
         --  sides of where the log wraps around.

         if Page_Buffer_Last = Log_Page_Buffer_Count
           or else (Page_Buffer_Last > 0
                    and then
                      (Location
                       /= Page_Buffer_Loc
                       + Log_Location_Id (Page_Buffer_Last)
                       or else Get_Device_Index (Location)
                       /= Get_Device_Index (Page_Buffer_Loc)))
         then
            Flush;
            if Entry_Failed then
               return 0;
            end if;
         end if;

         if Page_Buffer_Last = 0 then
            Page_Buffer_Loc := Location;
         end if;
      end;

      Page_Buffer_Last := Page_Buffer_Last + 1;
      Page_Buffer (Page_Buffer_Last) := Data;

      return Current_Log_Header.Log_Last;
   end Append_Data;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
     (Id   : Rose.Objects.Capability_Identifier;
      Page : System.Storage_Elements.Storage_Array)
      return Rose.Words.Word
   is
      pragma Unreferenced (Id);
   begin
      --  A log page fits in a word: a partition of 2 ** 32 pages would be
      --  16 TiB.
      return Rose.Words.Word (Append_Data (Page));
   end Append_Page;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Id          : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin

      if not Writing then
         Rose.Console_IO.Put_Line ("log: commit: not writing");
         return;
      end if;

      --  A checkpoint that failed loses only its own pages.  The pages
      --  appended before it started may hold the only copy of pages that
      --  the memory server has paged out, so the entry is rolled back to
      --  where the checkpoint started, and stays open for the next
      --  checkpoint to commit (ADR 0028, section 11).  A write to the
      --  disk that failed still fails the whole entry.

      if Append_Failed and then not Entry_Failed then
         if Checkpoint_Mark = 0 then
            Fail_Entry (No_Checkpoint_Started);
         else
            declare
               Repeated : constant Boolean :=
                            Failure = Log_Full and then Full_Reported;
               --  A full log is said once, not at each checkpoint that
               --  it fails (#1029).
            begin
               Discard_After (Checkpoint_Mark);
               Append_Failed := False;
               Checkpoint_Mark := 0;
               New_First_Entry := 0;
               Report_Problems;
               if not Repeated then
                  Rose.Console_IO.Put
                    ("log: checkpoint not committed; the open entry keeps ");
                  Rose.Console_IO.Put
                    (Natural (Current_Log_Header.Log_Last
                     - Current_Log_Header.Log_First + 1));
                  Rose.Console_IO.Put_Line (" pages");
               end if;
            end;
            return;
         end if;
      end if;

      --  The entry's pages go to the disk before its header, so that a
      --  header is only ever found with all of its pages.  A disk can
      --  report a write complete while it is still in its write cache, and
      --  write the cache in any order, so the pages are flushed to the
      --  medium before the header is written, and the header before the
      --  entry counts as committed (#505).

      if not Entry_Failed then
         Flush;
         Sync_Devices;
      end if;

      if not Entry_Failed then
         declare
            use type Rose.Words.Word_64;
            Success : Boolean;
         begin
            --  The entry records where the log starts once it is
            --  committed: a restore can use it only while the log starts
            --  there (#953).

            Current_Log_Header.Start_Sequence :=
              (if New_First_Entry /= 0
               then New_First_Sequence
               else Header.First_Sequence);

            Write (Next_Log_Location, Current_Log_Header'Address, Success);
            if Success then
               Sync_Devices;
               if First_Committed = 0 then
                  First_Committed := Next_Log_Location;
               end if;
               if First_Restorable = 0 then
                  First_Restorable := Next_Log_Location;
               end if;
               Last_Committed := Next_Log_Location;
               Next_Log_Location :=
                 Next_Location (Current_Log_Header.Log_Last);
               Next_Sequence := Next_Sequence + 1;
            else
               Fail_Entry (Cannot_Write_Header);
            end if;
         end;
      end if;

      --  The log's header moves its first entry only once the entry is
      --  on the medium, so a crash before then leaves the log starting
      --  where it did, and nothing has been overwritten yet.  A crash
      --  between the two writes leaves an entry that starts later than
      --  the log's header, and the next startup finishes the move
      --  (Finish_Start).  From then on, the entries before this one can't
      --  be restored.

      if not Entry_Failed and then New_First_Entry /= 0 then
         Move_First_Entry;
         if Header.First_Entry = New_First_Entry then
            First_Restorable := Last_Committed;
         end if;
      end if;
      New_First_Entry := 0;

      Report_Problems;

      if Entry_Failed then
         Rose.Console_IO.Put_Line ("log: checkpoint not committed");
      else
         Report_Space;
      end if;

      Page_Buffer_Last := 0;
      Writing := False;

      --  The store holds the space it freed before the checkpoint until
      --  the entry is committed (ADR 0038, section 6).  It is told here,
      --  and this waits for it, so that it hears of the commit before
      --  the next entry is created.

      if not Entry_Failed then
         Notify_Store_Committed;
      end if;

      --  The next entry opens now, not when the next checkpoint starts,
      --  so that the memory server can append to it in between. After a
      --  failed entry, it starts where that one did.

      Open_Entry;

      --  The memory server is told after the entry opens: it may call
      --  back at once to append the pages it copies forward. It is
      --  sent without waiting, since the memory server may be waiting
      --  on the log server itself.

      if not Entry_Failed then
         Notify_Mem_Committed;
      end if;

   end Commit;

   ------------
   -- Create --
   ------------

   procedure Create
     (Id : in Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin

      if not Active then
         Rose.Console_IO.Put_Line ("log: create: not active");
         return;
      end if;

      --  The kernel calls this when a checkpoint starts. The entry that
      --  the checkpoint completes has been open since the last commit
      --  (ADR 0028, section 4), and may already hold pages.

      if not Writing then
         Open_Entry;
      end if;

      --  If the checkpoint fails, Commit rolls the entry back to here.
      --  A batch from the memory server is appended in one call, and
      --  dropped if it fails (Append_Batch, #974), so a failed append
      --  here is one that it never discarded, and this checkpoint fails
      --  too.

      Checkpoint_Mark := Current_Log_Header.Log_Last;

      if Append_Failed then
         Note_Problem (Unreported_Failed_Append);
      end if;

   end Create;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);

      Rose.System_Calls.Use_Buffer
        (Buffer_Address => Invoke_Buffer'Address,
         Buffer_Size    => Invoke_Buffer'Length);

      Console_Cap := Get_Cap (1);
      Start_Log_Cap := Get_Cap (2);
      Store_Committed_Cap := Get_Cap (3);
      Mem_Committed_Cap := Get_Cap (4);

      --  Nothing reads the cap set again (#826).

      Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);

      Rose.Console_IO.Open (Console_Cap);

      Rose.Console_IO.Put_Line ("log: starting");

      Rose.Interfaces.Checkpoints.Kernel_Log.Server.Create_Server
        (Server_Context => Log_Server,
         Create         => Create'Access,
         Append         => Append'Access,
         Commit         => Commit'Access,
         Add_Log_Device => Add_Log_Device'Access,
         Instanced      => False);

      Rose.Interfaces.Checkpoints.State.Server.Attach_Interface
        (Server_Context => Log_Server,
         Reset          => Reset'Access);

      Rose.Interfaces.Checkpoints.Log_Reader.Server.Attach_Interface
        (Server_Context => Log_Server,
         First_Entry    => First_Entry'Access,
         Last_Entry     => Last_Entry'Access,
         First_Checkpoint => First_Checkpoint'Access,
         Next_Entry     => Next_Entry'Access,
         Page_Count     => Page_Count'Access,
         Read_Page      => Read_Page'Access);

      Rose.Interfaces.Checkpoints.Page_Log.Server.Attach_Interface
        (Server_Context => Log_Server,
         Append_Page    => Append_Page'Access,
         Append_Batch   => Append_Batch'Access,
         Read_Page      => Read_Log_Page'Access,
         Fail_Checkpoint => Fail_Checkpoint'Access,
         Discard_Pages  => Discard_Pages'Access,
         Free_Before    => Free_Before'Access,
         Get_Space      => Get_Space'Access,
         Entry_Last     => Entry_Last'Access);

      for Page of Page_Buffer loop
         for Element of Page loop
            Element := 0;
         end loop;
      end loop;

   end Create_Server;

   -------------------
   -- Discard_After --
   -------------------

   procedure Discard_After (Last : Log_Location_Id) is
      Start : constant Log_Location_Id := Next_Log_Location;
      --  The open entry's header.  Its pages are compared by how far
      --  they are from it, since they can wrap around.
   begin
      if Distance (Start, Last)
        >= Distance (Start, Current_Log_Header.Log_Last)
      then
         return;
      end if;

      Current_Log_Header.Log_Last := Last;

      --  The buffer holds the entry's pages from Page_Buffer_Loc to its
      --  last, next to each other on the disk.  Pages after Last that
      --  were already written stay on the disk until later pages
      --  overwrite them: nothing reads them, since the entry's header is
      --  written last and names its end.

      if Page_Buffer_Last = 0 then
         null;
      elsif Distance (Start, Last) < Distance (Start, Page_Buffer_Loc) then
         Page_Buffer_Last := 0;
      else
         Page_Buffer_Last := Page_Buffer_Count (Last - Page_Buffer_Loc + 1);
      end if;
   end Discard_After;

   -------------------
   -- Discard_Pages --
   -------------------

   procedure Discard_Pages
     (Id   : Rose.Objects.Capability_Identifier;
      From : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Location : constant Log_Location_Id := Log_Location_Id (From);
   begin
      if not Writing then
         Note_Problem (No_Open_Entry);
         return;
      end if;

      --  During a checkpoint, a failed append fails the checkpoint, and
      --  only Commit rolls it back.

      if Checkpoint_Mark /= 0 then
         Note_Problem (Discard_During_Checkpoint);
         return;
      end if;

      if Location /= 0 then
         if Location = Next_Log_Location
           or else not In_Range
             (Location, Next_Log_Location, Current_Log_Header.Log_Last)
         then
            Note_Problem (Bad_Discard, Location);
            return;
         end if;

         Discard_After (Previous_Location (Location));
      end if;

      --  After a failed write to the disk, the entry stays failed.
      --  Otherwise the batch's failure no longer fails anything, but is
      --  still worth reporting.

      if Append_Failed and then not Entry_Failed then
         Note_Problem (Failure);
         Failure := No_Problem;
      end if;

      Append_Failed := False;
   end Discard_Pages;

   ----------------
   -- Entry_Last --
   ----------------

   function Entry_Last
     (Id     : Rose.Objects.Capability_Identifier;
      Header : Rose.Words.Word)
      return Rose.Words.Word
   is
      pragma Unreferenced (Id);
      Location : constant Log_Location_Id := Log_Location_Id (Header);
   begin
      if Is_Location (Location) and then Load_Entry (Location) then
         return Rose.Words.Word (Cached_Header.Log_Last);
      else
         return 0;
      end if;
   end Entry_Last;

   -----------------
   -- Fail_Append --
   -----------------

   procedure Fail_Append (Problem : Problem_Type) is
   begin
      if not Append_Failed then
         Append_Failed := True;
         if Failure = No_Problem then
            Failure := Problem;
         end if;
      end if;
   end Fail_Append;

   ---------------------
   -- Fail_Checkpoint --
   ---------------------

   procedure Fail_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin
      if Writing and then Checkpoint_Mark /= 0 then
         Fail_Append (Failed_By_Memory_Server);
      else
         Note_Problem (No_Checkpoint_To_Fail);
      end if;
   end Fail_Checkpoint;

   ----------------
   -- Fail_Entry --
   ----------------

   procedure Fail_Entry (Problem : Problem_Type) is
   begin
      if not Entry_Failed then
         Entry_Failed := True;
         Failure := Problem;
      end if;
   end Fail_Entry;

   ----------------
   -- Find_Entry --
   ----------------

   function Find_Entry
     (Location : Log_Location_Id;
      Found    : out Log_Location_Id;
      Sequence : out Rose.Words.Word_64)
      return Boolean
   is
      use type Rose.Words.Word_64;
   begin
      Found := 0;
      Sequence := 0;

      if not Is_Location (Location) then
         return False;
      end if;

      if Writing and then not Entry_Failed
        and then In_Range
          (Location, Next_Log_Location, Current_Log_Header.Log_Last)
      then
         Found := Next_Log_Location;
         Sequence := Next_Sequence;
         return True;
      end if;

      if First_Committed = 0 then
         return False;
      end if;

      declare
         Current          : Log_Location_Id := First_Committed;
         Current_Sequence : Rose.Words.Word_64 := Header.First_Sequence;
      begin
         while Load_Entry (Current) loop
            if In_Range (Location, Current, Cached_Header.Log_Last) then
               Found := Current;
               Sequence := Current_Sequence;
               return True;
            end if;

            exit when Current = Last_Committed;
            Current := Next_Location (Cached_Header.Log_Last);
            Current_Sequence := Current_Sequence + 1;
         end loop;
      end;

      return False;
   end Find_Entry;

   ------------------
   -- Finish_Start --
   ------------------

   procedure Finish_Start
     (Last_Start : Rose.Words.Word_64;
      Run_First  : Log_Location_Id)
   is
      use type Rose.Words.Word_64;
   begin
      First_Restorable := 0;
      if Last_Committed = 0 then
         return;
      end if;

      --  The last entry's start is between the header's and the last
      --  entry itself if the header's write was cut short; anything else
      --  is not a start that this log can move to.

      if Last_Start > Header.First_Sequence
        and then Last_Start < Next_Sequence
      then
         declare
            Location : Log_Location_Id := First_Committed;
            Sequence : Rose.Words.Word_64 := Header.First_Sequence;
         begin
            while Sequence /= Last_Start loop
               exit when not Load_Entry (Location);
               Location := Next_Location (Cached_Header.Log_Last);
               Sequence := Sequence + 1;
            end loop;

            if Sequence = Last_Start then
               New_First_Entry := Location;
               New_First_Sequence := Last_Start;
               Move_First_Entry;
               New_First_Entry := 0;
               Rose.Console_IO.Put
                 ("log: the last entry's commit had moved the start to ");
               Rose.Console_IO.Put (Natural (Header.First_Entry));
               Rose.Console_IO.New_Line;
               Report_Problems;
            end if;
         end;
      end if;

      --  The run's entries name pages from their start on, which are
      --  still in the log if the log starts there or before.  An entry
      --  before the run has an earlier start, which may have been freed.

      if Last_Start = 0 or else Last_Start >= Header.First_Sequence then
         First_Restorable := Run_First;
      else
         Rose.Console_IO.Put_Line
           ("log: the last entry needs entries that were freed");
      end if;
   end Finish_Start;

   ----------------------
   -- First_Checkpoint --
   ----------------------

   function First_Checkpoint
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Words.Word_64 (First_Restorable);
   end First_Checkpoint;

   -----------------
   -- First_Entry --
   -----------------

   function First_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Words.Word_64 (First_Committed);
   end First_Entry;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      if Page_Buffer_Last > 0 then
         --  Rose.Console_IO.Put ("log: writing ");
         --  Rose.Console_IO.Put (Natural (Page_Buffer_Last));
         --  Rose.Console_IO.Put (" page");
         --  if Page_Buffer_Last /= 1 then
         --     Rose.Console_IO.Put ("s");
         --  end if;
         --  Rose.Console_IO.Put (": location ");
         --  Rose.Console_IO.Put (Natural (Page_Buffer_Loc));
         --  Rose.Console_IO.New_Line;

         declare
            Success : Boolean;
         begin
            Operate
              (Location   => Page_Buffer_Loc,
               Addr       => Page_Buffer'Address,
               Operation  => Write,
               Page_Count => Natural (Page_Buffer_Last),
               Success    => Success);
            Page_Buffer_Last := 0;
            if not Success then
               Fail_Entry (Cannot_Write_Pages);
            end if;
         end;

         --  Rose.Console_IO.Put_Line ("done");

      end if;
   end Flush;

   -----------------
   -- Free_Before --
   -----------------

   procedure Free_Before
     (Id       : Rose.Objects.Capability_Identifier;
      Log_Page : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Location : constant Log_Location_Id := Log_Location_Id (Log_Page);
      Found    : Log_Location_Id;
      Sequence : Rose.Words.Word_64;
   begin
      if not Find_Entry (Location, Found, Sequence) then
         Note_Problem (Bad_Free, Location);
      elsif Found = Header.First_Entry then
         New_First_Entry := 0;
      else
         New_First_Entry := Found;
         New_First_Sequence := Sequence;
      end if;
   end Free_Before;

   ----------------
   -- Free_Pages --
   ----------------

   function Free_Pages return Log_Location_Id is
      Last_Used : constant Log_Location_Id :=
                    (if Writing then Current_Log_Header.Log_Last
                     else Previous_Location (Next_Log_Location));
      --  The open entry's last page, or before where the next entry's
      --  header goes.
   begin
      if Last_Log_Device = 0 then
         return 0;
      elsif First_Committed = 0 then

         --  Nothing is committed: only the open entry is in use.

         return (if Writing
                 then Last_Location - 1
                   - Distance (Next_Log_Location, Last_Used)
                 else Last_Location);
      else

         --  From the page after the last one in use up to the log's
         --  first entry, which an append never reaches.

         declare
            Gap : constant Log_Location_Id :=
                    Distance (Last_Used, Header.First_Entry);
         begin
            return (if Gap = 0 then 0 else Gap - 1);
         end;
      end if;
   end Free_Pages;

   ----------------------
   -- Get_Device_Index --
   ----------------------

   function Get_Device_Index
     (Location : Log_Location_Id)
      return Log_Device_Count
   is
   begin
      for Index in 1 .. Last_Log_Device loop
         if Location <= Log_Devices (Index).Last_Location then
            return Index;
         end if;
      end loop;
      Rose.Console_IO.Put ("log: no device for location ");
      Rose.Console_IO.Put (Natural (Location));
      Rose.Console_IO.New_Line;

      return 0;
   end Get_Device_Index;

   ---------------
   -- Get_Space --
   ---------------

   procedure Get_Space
     (Id    : Rose.Objects.Capability_Identifier;
      First : out Rose.Words.Word;
      Open  : out Rose.Words.Word;
      Last  : out Rose.Words.Word;
      Free  : out Rose.Words.Word)
   is
      pragma Unreferenced (Id);
   begin
      Open := (if Writing then Rose.Words.Word (Next_Log_Location) else 0);
      First :=
        (if First_Committed = 0 then Open
         else Rose.Words.Word (First_Committed));
      Last := Rose.Words.Word (Last_Location);
      Free := Rose.Words.Word (Free_Pages);
   end Get_Space;

   ------------------
   -- Is_Log_Entry --
   ------------------

   function Is_Log_Entry
     (Location     : Log_Location_Id;
      Entry_Header : Log.Header.Entry_Header_Type;
      Sequence     : Rose.Words.Word_64)
      return Boolean
   is
      use type Rose.Words.Word_32;
      use type Rose.Words.Word_64;
   begin
      return Entry_Header.Magic = Log.Header.Entry_Magic
        and then Entry_Header.Version = Log_Version
        and then Entry_Header.Generation = Header.Generation
        and then (Sequence = 0 or else Entry_Header.Sequence = Sequence)
        and then Entry_Header.Log_First = Next_Location (Location)
        and then Is_Location (Entry_Header.Log_Last);
   end Is_Log_Entry;

   ----------------
   -- Last_Entry --
   ----------------

   function Last_Entry
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Words.Word_64 (Last_Committed);
   end Last_Entry;

   ----------------
   -- Load_Entry --
   ----------------

   function Load_Entry (Location : Log_Location_Id) return Boolean is
      Success : Boolean;
   begin
      if First_Committed = 0
        or else not In_Range (Location, First_Committed, Last_Committed)
      then
         return False;
      end if;

      if Location = Cached_Location then
         return True;
      end if;

      Cached_Location := 0;
      Read (Location, Cached_Header'Address, Success);
      if Success and then Is_Log_Entry (Location, Cached_Header, 0) then
         Cached_Location := Location;
      end if;

      return Cached_Location /= 0;
   end Load_Entry;

   ----------------------
   -- Move_First_Entry --
   ----------------------

   procedure Move_First_Entry is
      Old_First    : constant Log_Location_Id := Header.First_Entry;
      Old_Sequence : constant Rose.Words.Word_64 := Header.First_Sequence;
      Success      : Boolean;
   begin
      Header.First_Entry := New_First_Entry;
      Header.First_Sequence := New_First_Sequence;
      Write (0, Header'Address, Success);

      --  The space before the new first entry can only be written again
      --  once the header that frees it is on the medium.

      if Success then
         Sync_Devices;
         First_Committed := New_First_Entry;
         Cached_Location := 0;
      else
         Header.First_Entry := Old_First;
         Header.First_Sequence := Old_Sequence;
         Note_Problem (Cannot_Write_Log_Header);
      end if;
   end Move_First_Entry;

   ----------------
   -- Next_Entry --
   ----------------

   function Next_Entry
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Id);
      Current : constant Log_Location_Id := Log_Location_Id (Location);
   begin
      if Current = Last_Committed or else not Load_Entry (Current) then
         return 0;
      end if;

      --  The next entry has to have the next sequence number, or it is
      --  left over from an earlier lap of the log.

      declare
         use type Rose.Words.Word_64;
         Sequence : constant Rose.Words.Word_64 := Cached_Header.Sequence;
         Next     : constant Log_Location_Id :=
                      Next_Location (Cached_Header.Log_Last);
      begin
         return (if Load_Entry (Next)
                 and then Cached_Header.Sequence = Sequence + 1
                 then Rose.Words.Word_64 (Next)
                 else 0);
      end;
   end Next_Entry;

   ------------------
   -- Note_Problem --
   ------------------

   procedure Note_Problem
     (Problem : Problem_Type;
      Page    : Log_Location_Id := 0)
   is
   begin
      if Other_Problem = No_Problem then
         Other_Problem := Problem;
         Problem_Page := Page;
      end if;
   end Note_Problem;

   --------------------------
   -- Notify_Mem_Committed --
   --------------------------

   procedure Notify_Mem_Committed is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Mem_Committed_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Initialize_Send (Params, Mem_Committed_Cap);
         Params.Control.Flags (Rose.Invocation.Block) := False;
         Params.Control.Flags (Rose.Invocation.Create_Reply_Cap) := False;
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Notify_Mem_Committed;

   ----------------------------
   -- Notify_Store_Committed --
   ----------------------------

   procedure Notify_Store_Committed is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Store_Committed_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Initialize_Send (Params, Store_Committed_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Notify_Store_Committed;

   ----------------
   -- Open_Entry --
   ----------------

   procedure Open_Entry is
   begin
      Writing := True;
      Entry_Failed := False;
      Append_Failed := False;
      Checkpoint_Mark := 0;
      --  The startup scan reads into Current_Log_Header, so after a
      --  restart it holds whatever page followed the last entry.  Set
      --  each field, rather than assigning a whole header: that would
      --  need a page-sized temporary on the stack.
      Current_Log_Header.Magic := Log.Header.Entry_Magic;
      Current_Log_Header.Version := Log_Version;
      Current_Log_Header.Flags := Log.Header.Is_Active_Entry;
      Current_Log_Header.Generation := Header.Generation;
      Current_Log_Header.Log_First := Next_Location (Next_Log_Location);
      Current_Log_Header.Log_Last := Next_Log_Location;
      Current_Log_Header.Sequence := Next_Sequence;
      Page_Buffer_Last := 0;
      Page_Buffer_Loc := Current_Log_Header.Log_First;

      --  The header can't go where the first committed entry is: the log
      --  has wrapped around to it, and is full.

      if Last_Log_Device = 0
        or else (First_Committed /= 0
                 and then Next_Log_Location = Header.First_Entry)
      then
         Fail_Append (Log_Full);
      end if;
   end Open_Entry;

   -------------
   -- Operate --
   -------------

   procedure Operate
     (Location   : Log_Location_Id;
      Addr       : System.Address;
      Operation  : Device_Operation;
      Page_Count : Natural;
      Success    : out Boolean)
   is
      Device_Index : constant Log_Device_Count :=
                       Get_Device_Index (Location);
   begin
      Success := False;

      if Device_Index = 0
        or else Location + Log_Location_Id (Page_Count) - 1
          > Log_Devices (Device_Index).Last_Location
      then
         return;
      end if;

      declare
         use System.Storage_Elements;

         Device  : Log_Device_Record renames Log_Devices (Device_Index);
         Count   : constant Natural := Device.Multiplier * Page_Count;
         Start   : constant Block_Address_Type :=
           Block_Address_Type (Location - Device.First_Location)
           * Block_Address_Type (Device.Multiplier);
         Data_Last : constant Storage_Count :=
           Log_Page_Size * Storage_Count (Page_Count);
         Data    : System.Storage_Elements.Storage_Array (1 .. Data_Last);
         pragma Import (Ada, Data);
         for Data'Address use Addr;
      begin
         case Operation is
            when Read =>
               Rose.Interfaces.Storage.Block_Array.Client.Read_Blocks
                 (Item   => Device.Client,
                  Start  => Start,
                  Count  => Count,
                  Blocks => Data);
            when Write =>
               --  Rose.Console_IO.Put ("Write_Blocks: start=");
               --  Rose.Console_IO.Put (Natural (Start));
               --  Rose.Console_IO.Put ("; count=");
               --  Rose.Console_IO.Put (Count);
               --  Rose.Console_IO.Put ("; bytes=");
               --  Rose.Console_IO.Put (Natural (Data_Last));
               --  Rose.Console_IO.New_Line;

               Rose.Interfaces.Storage.Block_Array.Client.Write_Blocks
                 (Item   => Device.Client,
                  Start  => Start,
                  Count  => Count,
                  Blocks => Data);

               --  Rose.Console_IO.Put_Line ("finished writing");

         end case;
      end;

      Success := True;
   end Operate;

   ----------------
   -- Page_Count --
   ----------------

   function Page_Count
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64)
      return Natural
   is
      pragma Unreferenced (Id);
      Item : constant Log_Location_Id := Log_Location_Id (Location);
   begin
      if Load_Entry (Item) then
         return Natural (Distance (Item, Cached_Header.Log_Last));
      else
         return 0;
      end if;
   end Page_Count;

   -----------------
   -- Put_Problem --
   -----------------

   procedure Put_Problem (Problem : Problem_Type) is
      procedure Put (Text : String) renames Rose.Console_IO.Put;
   begin
      case Problem is
         when No_Problem =>
            null;
         when No_Open_Entry =>
            Put ("no entry is open");
         when Bad_Page_Size =>
            Put ("a page was the wrong size");
         when Log_Full =>
            Put ("log is full");
         when Failed_By_Memory_Server =>
            Put ("checkpoint failed by the memory server");
         when No_Checkpoint_To_Fail =>
            Put ("the memory server failed a checkpoint, "
                 & "but none was being written");
         when Cannot_Write_Pages =>
            Put ("cannot write entry pages");
         when Cannot_Write_Header =>
            Put ("cannot write entry header");
         when No_Checkpoint_Started =>
            Put ("commit, but no checkpoint was started");
         when Unreported_Failed_Append =>
            Put ("checkpoint started after a failed append");
         when Bad_Read =>
            Put ("read of a page outside the log");
         when Bad_Discard =>
            Put ("discard of a page outside the open entry");
         when Discard_During_Checkpoint =>
            Put ("discard during a checkpoint");
         when Bad_Free =>
            Put ("free before a page outside the log");
         when Cannot_Write_Log_Header =>
            Put ("cannot write the log header; its first entry stays");
      end case;
   end Put_Problem;

   ----------
   -- Read --
   ----------

   procedure Read
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean)
   is
   begin
      Operate (Location, Addr, Read, 1, Success);
   end Read;

   -------------------
   -- Read_Log_Page --
   -------------------

   procedure Read_Log_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Log_Page : Rose.Words.Word;
      Page     : out System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      Location : constant Log_Location_Id := Log_Location_Id (Log_Page);
      Success  : Boolean := False;
   begin
      if Page'Length /= Log_Page_Size then
         Note_Problem (Bad_Page_Size);
      elsif not Active or else not Readable (Location) then
         Note_Problem (Bad_Read, Location);
      elsif Writing
        and then Location >= Page_Buffer_Loc
        and then Location
          < Page_Buffer_Loc + Log_Location_Id (Page_Buffer_Last)
      then
         --  A page of the open entry that is still in the buffer.
         Page :=
           Page_Buffer (Page_Buffer_Index (Location - Page_Buffer_Loc + 1));
         Success := True;
      else
         Read (Location, Page'Address, Success);
      end if;

      if not Success then
         Page := (others => 0);
      end if;
   end Read_Log_Page;

   ---------------
   -- Read_Page --
   ---------------

   procedure Read_Page
     (Id       : Rose.Objects.Capability_Identifier;
      Location : Rose.Words.Word_64;
      Index    : Positive;
      Page     : out System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      Item    : constant Log_Location_Id := Log_Location_Id (Location);
      Success : Boolean := False;
   begin
      if Page'Length /= Log_Page_Size then
         Rose.Console_IO.Put_Line ("log: read: buffer is not one page");
      elsif not Load_Entry (Item) then
         Rose.Console_IO.Put_Line ("log: read: not an entry");
      elsif Log_Location_Id (Index) > Distance (Item, Cached_Header.Log_Last)
      then
         Rose.Console_IO.Put_Line ("log: read: no such page in entry");
      else
         Read (Advance (Item, Log_Location_Id (Index)), Page'Address,
               Success);
      end if;

      if not Success then
         Page := (others => 0);
      end if;
   end Read_Page;

   --------------
   -- Readable --
   --------------

   function Readable (Location : Log_Location_Id) return Boolean is
      Start : constant Log_Location_Id :=
                (if First_Committed /= 0 then First_Committed
                 else Next_Log_Location);
   begin
      if Writing and then not Entry_Failed then
         return In_Range (Location, Start, Current_Log_Header.Log_Last);
      elsif First_Committed /= 0 then
         return In_Range
           (Location, Start, Previous_Location (Next_Log_Location));
      else
         return False;
      end if;
   end Readable;

   ---------------------
   -- Report_Problems --
   ---------------------

   procedure Report_Problems is
   begin
      if Failure = Log_Full and then Full_Reported then
         null;
      elsif Failure /= No_Problem then
         Rose.Console_IO.Put ("log: ");
         Put_Problem (Failure);
         Rose.Console_IO.New_Line;
         if Failure = Log_Full then
            Full_Reported := True;
         end if;
      end if;

      if Other_Problem /= No_Problem then
         Rose.Console_IO.Put ("log: ");
         Put_Problem (Other_Problem);
         if Problem_Page /= 0 then
            Rose.Console_IO.Put (": log page ");
            Rose.Console_IO.Put (Natural (Problem_Page));
         end if;
         Rose.Console_IO.New_Line;
      end if;

      Failure := No_Problem;
      Other_Problem := No_Problem;
      Problem_Page := 0;
   end Report_Problems;

   ------------------
   -- Report_Space --
   ------------------

   procedure Report_Space is
      Free : constant Log_Location_Id := Free_Pages;
   begin
      if Full_Reported then
         Rose.Console_IO.Put_Line ("log: the log has room again");
         Full_Reported := False;
      end if;

      if not Below_Reserve
        and then Free < Rose.Limits.Log_Reserve_Pages
      then
         Rose.Console_IO.Put ("log: ");
         Rose.Console_IO.Put (Natural (Free));
         Rose.Console_IO.Put (" pages free, less than the reserve of ");
         Rose.Console_IO.Put (Natural (Rose.Limits.Log_Reserve_Pages));
         Rose.Console_IO.New_Line;
         Below_Reserve := True;
      elsif Below_Reserve
        and then Free >= Rose.Limits.Log_Reserve_Pages
      then
         Rose.Console_IO.Put ("log: ");
         Rose.Console_IO.Put (Natural (Free));
         Rose.Console_IO.Put_Line (" pages free, the reserve is whole again");
         Below_Reserve := False;
      end if;
   end Report_Space;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Id          : in     Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
      Data   : System.Storage_Elements.Storage_Array (1 .. 4096);
      pragma Import (Ada, Data);
      for Data'Address use Header'Address;
   begin
      if Last_Log_Device = 0 then
         Rose.Console_IO.Put_Line ("log: reset: no log device");
      else
         declare
            use type Rose.Words.Word_64;
            Generation : constant Rose.Words.Word_64 :=
                           (if Header.Magic = Log.Header.Log_Magic
                            then Header.Generation + 1
                            else 1);

            --  The new log starts where the old one did, which is
            --  location 1 unless something moved it.  The test of a log
            --  that wraps around (scripts/test-log-wrap) starts one near
            --  the end of the partition, and an install resets it.

            Start      : constant Log_Location_Id :=
                           (if Header.Magic = Log.Header.Log_Magic
                            and then Is_Location (Header.First_Entry)
                            then Header.First_Entry
                            else 1);
            Success    : Boolean;
         begin
            Header := (others => <>);
            Header.Generation := Generation;
            Header.First_Entry := Start;
            Write (0, Header'Address, Success);
            if not Success then
               Rose.Console_IO.Put_Line ("log: reset: cannot write header");
            end if;
            Next_Log_Location := Header.First_Entry;
            Next_Sequence := Header.First_Sequence;
            First_Committed := 0;
            Last_Committed := 0;
            First_Restorable := 0;
            Cached_Location := 0;
            New_First_Entry := 0;
         end;

         --  An open entry is dropped, with anything appended to it, and
         --  the first entry of the new log opens.

         Writing := False;
         Page_Buffer_Last := 0;
         if Active then
            Open_Entry;
         end if;
      end if;
   end Reset;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Log_Server);
   end Start_Server;

   ------------------
   -- Sync_Devices --
   ------------------

   procedure Sync_Devices is
   begin
      for Device of Log_Devices (1 .. Last_Log_Device) loop
         Rose.Interfaces.Storage.Block_Array.Client.Flush (Device.Client);
      end loop;
   end Sync_Devices;

   -----------
   -- Write --
   -----------

   procedure Write
     (Location : Log_Location_Id;
      Addr     : System.Address;
      Success  : out Boolean)
   is
   begin
      Operate (Location, Addr, Write, 1, Success);
   end Write;

end Log.Server;
