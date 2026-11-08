with Rose.Arch.Interrupt_Table;
with Rose.Boot.Console;
with Rose.Checkpoints;
with Rose.Interfaces.Restorable;
with Rose.Limits;

with Rose.Kernel.Cap_Tables;
with Rose.Kernel.Derivations;
with Rose.Kernel.Interrupts;
with Rose.Kernel.Processes.Debug;
with Rose.Kernel.Processes.Queue;

package body Rose.Kernel.Processes.Snapshot is

   use System.Storage_Elements;
   use Rose.Words;

   Page_Bytes : constant := Rose.Limits.Page_Size;

   Records_Held : Boolean := False;
   --  The derivation records have been restored, and each has a slot that
   --  Release_Restore_Holds has not dropped.

   function Is_Saved (P : Kernel_Process_Entry) return Boolean
   is (P.State not in Available | Starting | Killed
       and then P.Flags (Persistent));
   --  A Starting process is still being built by the server that launches
   --  it, which is not saved, and has no entry point yet (#543). A Killed
   --  one is waiting for mem to end it (#703). Neither can be restored.

   function Active_Cap_Count (P : Kernel_Process_Entry) return Natural;

   --  The callers that were waiting for a restored process to receive
   --  when the checkpoint was taken, from the waiting lists saved with
   --  the processes. Their calls were never delivered (#519).

   Max_Undelivered : constant := 64;

   Undelivered          : array (1 .. Max_Undelivered)
     of Rose.Objects.Object_Id;
   Undelivered_Count    : Natural := 0;
   Undelivered_Overflow : Boolean := False;

   function Is_Undelivered (Oid : Rose.Objects.Object_Id) return Boolean;

   --  The shared slots that the restored processes had, as servers: the
   --  slot's page, and the caller whose buffer was mapped there. A caller
   --  that goes on waiting has its buffer mapped there again.

   Max_Restored_Shares : constant := 64;

   type Restored_Share is
      record
         Server : Rose.Objects.Object_Id;
         Page   : Rose.Addresses.Virtual_Page_Address;
         Caller : Rose.Objects.Object_Id;
      end record;

   Restored_Shares     : array (1 .. Max_Restored_Shares) of Restored_Share;
   Restored_Share_Count : Natural := 0;

   procedure Restore_Shared_Buffer
     (Caller  : Process_Id;
      Server  : Process_Id;
      Success : out Boolean);
   --  Map the buffer of Caller's current invocation into the slots of
   --  Server that it had at the checkpoint. Success is False, and nothing
   --  is mapped, if they aren't known, or a page of the buffer isn't
   --  mapped in Caller.

   function Called_Server (Pid : Process_Id) return Process_Id;
   --  The process that Pid's current invocation was sent to, or
   --  Null_Process_Id if it wasn't sent to a process's endpoint.

   function Holds_Reply_Cap
     (Pid    : Process_Id;
      Caller : Rose.Objects.Object_Id)
      return Boolean;
   --  True if Pid has a reply cap for Caller.

   procedure Delete_Reply_Caps
     (Pid    : Process_Id;
      Caller : Rose.Objects.Object_Id);
   --  Delete the reply caps that Pid has for Caller.

   procedure Resume_Blocked_Call
     (Pid     : Process_Id;
      Waiting : out Boolean);
   --  Decide what happens to the call that the restored process Pid was
   --  blocked in: Waiting is True if it goes on waiting for the reply,
   --  and False if it makes the call again.

   function Cap_Page_Count (Cap_Count : Natural) return Natural
   is ((Cap_Count + Rose.Checkpoints.Caps_Per_Page - 1)
       / Rose.Checkpoints.Caps_Per_Page);

   procedure Save_Caps
     (P     : Kernel_Process_Entry;
      Pages : System.Address);
   --  Write the active caps of P into the cap pages that start at Pages.

   procedure Save_Process
     (P       : Kernel_Process_Entry;
      Item    : in out Rose.Checkpoints.Process_Record;
      Success : out Boolean);

   ----------------------
   -- Active_Cap_Count --
   ----------------------

   function Active_Cap_Count (P : Kernel_Process_Entry) return Natural is
      Count : Natural := 0;
   begin
      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Cap_Count;

   -------------------
   -- Called_Server --
   -------------------

   function Called_Server (Pid : Process_Id) return Process_Id is
      use Rose.Capabilities.Layout;
      P   : Kernel_Process_Entry renames Process_Table (Pid);
      Cap : constant Rose.Capabilities.Capability := P.Current_Params.Cap;
   begin
      if not Cap_Tables.Is_Active (P.Caps, Cap) then
         return Null_Process_Id;
      end if;

      declare
         Layout : constant Capability_Layout := Cap_Layout (Pid, Cap);
      begin
         if Layout.Header.Cap_Type = Endpoint_Cap
           and then Rose.Objects.Is_Process_Object_Id (Layout.Payload)
         then
            return To_Process_Id (Layout.Payload);
         else
            return Null_Process_Id;
         end if;
      end;
   end Called_Server;

   -----------------------
   -- Delete_Reply_Caps --
   -----------------------

   procedure Delete_Reply_Caps
     (Pid    : Process_Id;
      Caller : Rose.Objects.Object_Id)
   is
      use Rose.Capabilities.Layout;
      use type Rose.Objects.Object_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            declare
               Layout : constant Capability_Layout := Cap_Layout (Pid, Cap);
            begin
               if Layout.Header.Cap_Type = Reply_Cap
                 and then Layout.Payload = Caller
               then
                  Delete_Cap (Pid, Cap);
               end if;
            end;
         end if;
      end loop;
   end Delete_Reply_Caps;

   ---------------------
   -- Holds_Reply_Cap --
   ---------------------

   function Holds_Reply_Cap
     (Pid    : Process_Id;
      Caller : Rose.Objects.Object_Id)
      return Boolean
   is
      use Rose.Capabilities.Layout;
      use type Rose.Objects.Object_Id;
      P : Kernel_Process_Entry renames Process_Table (Pid);
   begin
      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            declare
               Layout : constant Capability_Layout := Cap_Layout (Pid, Cap);
            begin
               if Layout.Header.Cap_Type = Reply_Cap
                 and then Layout.Payload = Caller
               then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Holds_Reply_Cap;

   --------------------
   -- Is_Undelivered --
   --------------------

   function Is_Undelivered (Oid : Rose.Objects.Object_Id) return Boolean is
      use type Rose.Objects.Object_Id;
   begin
      for I in 1 .. Undelivered_Count loop
         if Undelivered (I) = Oid then
            return True;
         end if;
      end loop;
      return False;
   end Is_Undelivered;

   ---------------------------
   -- Release_Restore_Holds --
   ---------------------------

   procedure Release_Restore_Holds is
      use Rose.Kernel.Derivations;
      Restored : Natural := 0;
      Used     : Natural := 0;
   begin
      if not Records_Held then
         return;
      end if;

      Records_Held := False;

      for Id in 1 .. Max_Records loop
         if Is_Used (Derivation_Records, Record_Id (Id)) then
            Restored := Restored + 1;
         end if;
      end loop;

      for Id in 1 .. Max_Records loop
         if Is_Used (Derivation_Records, Record_Id (Id))
           and then Slots (Derivation_Records, Record_Id (Id)) > 0
         then
            Remove_Slot (Derivation_Records, Record_Id (Id));
         end if;
      end loop;

      for Id in 1 .. Max_Records loop
         if Is_Used (Derivation_Records, Record_Id (Id)) then
            Used := Used + 1;
         end if;
      end loop;

      Rose.Boot.Console.Put ("kernel: derivation records restored: ");
      Rose.Boot.Console.Put (Restored);
      Rose.Boot.Console.Put ("; named by restored capabilities: ");
      Rose.Boot.Console.Put (Used);
      Rose.Boot.Console.New_Line;
   end Release_Restore_Holds;

   -------------------------
   -- Restore_Derivations --
   -------------------------

   procedure Restore_Derivations
     (Pages        : System.Storage_Elements.Storage_Array;
      Record_Count : Natural;
      Success      : out Boolean)
   is
      use Rose.Kernel.Derivations;
      Saved : Rose.Checkpoints.Saved_Record_Array (1 .. Record_Count)
        with Import, Address => Pages'Address;

      function Is_Saved_Record (Id : Natural) return Boolean
      is (Id in Saved'Range
          and then (Saved (Id).Flags and Rose.Checkpoints.Record_Used) /= 0);

      function Level (Id : Natural) return Natural;
      --  How many records there are from Id up to the root, counting both,
      --  or 0 if Id, or a record above it, is not a saved record, or if
      --  there are more than Max_Depth, which a cycle would have.

      -----------
      -- Level --
      -----------

      function Level (Id : Natural) return Natural is
         Current : Natural := Id;
         Steps   : Natural := 0;
      begin
         while Current /= 0 loop
            if not Is_Saved_Record (Current) or else Steps = Max_Depth then
               return 0;
            end if;

            Steps := Steps + 1;
            Current := Natural (Saved (Current).Parent);
         end loop;

         return Steps;
      end Level;

   begin
      Success := False;

      if Record_Count > Max_Records
        or else Records_Held
        or else Pages'Length
          < Storage_Count
              ((Record_Count + Rose.Checkpoints.Records_Per_Page - 1)
               / Rose.Checkpoints.Records_Per_Page) * Page_Bytes
      then
         return;
      end if;

      for Id in 1 .. Max_Records loop
         if Is_Used (Derivation_Records, Record_Id (Id)) then
            return;
         end if;
      end loop;

      for Id in Saved'Range loop
         if Is_Saved_Record (Id) and then Level (Id) = 0 then
            return;
         end if;
      end loop;

      --  A record goes in after the one that it was derived from.

      for Wanted in 1 .. Max_Depth loop
         for Id in Saved'Range loop
            if Is_Saved_Record (Id) and then Level (Id) = Wanted then
               Restore_Record
                 (Derivation_Records,
                  Record_Id (Id),
                  Record_Id (Saved (Id).Parent),
                  (Saved (Id).Flags and Rose.Checkpoints.Record_Revoked)
                  /= 0);
               Records_Held := True;
            end if;
         end loop;
      end loop;

      Success := True;
   end Restore_Derivations;

   -----------------------
   -- Restore_Interrupt --
   -----------------------

   procedure Restore_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Object    : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability;
      Success   : out Boolean)
   is
      Pid : constant Process_Id := To_Process_Id (Object);
   begin
      Success := Interrupt in 32 .. 47
        and then Pid in Process_Table'Range
        and then Process_Table (Pid).State /= Available
        and then Process_Table (Pid).Flags (Persistent)
        and then not Rose.Kernel.Interrupts.Has_Handler (Interrupt);

      if Success then
         Rose.Kernel.Interrupts.Set_Handler (Interrupt, Object, Cap);
         Rose.Arch.Interrupt_Table.Enable_Interrupt (Interrupt);
      end if;
   end Restore_Interrupt;

   ---------------------
   -- Restore_Process --
   ---------------------

   procedure Restore_Process
     (Pages          : System.Storage_Elements.Storage_Array;
      Next_Object_Id : Rose.Objects.Object_Id;
      Pid            : out Process_Id;
      Success        : out Boolean)
   is
      Item : Rose.Checkpoints.Process_Record
        with Import, Address => Pages'Address;
   begin
      Pid := Null_Process_Id;
      Success := False;

      if Pages'Length < Page_Bytes
        or else Item.Magic /= Rose.Checkpoints.Process_Record_Magic
        or else Pages'Length
          < Storage_Count (1 + Item.Cap_Page_Count) * Page_Bytes
        or else Item.Cap_Count > Item.Last_Cap
        or else Item.Last_Cap > Cap_Tables.Max_Capability_Index
        or else Item.State > Process_State'Pos (Process_State'Last)
        or else Item.Priority not in
          Process_Priority'Pos (Process_Priority'First)
            .. Process_Priority'Pos (Process_Priority'Last)
      then
         return;
      end if;

      declare
         use type Rose.Objects.Object_Id;
         Oid : constant Rose.Objects.Object_Id :=
                 Rose.Objects.Object_Id (Item.Oid);
      begin
         if Oid < Rose.Objects.First_Process_Object_Id
           or else not Rose.Objects.Is_Process_Object_Id (Oid)
           or else not Rose.Objects.Is_Process_Object_Id (Next_Object_Id)
           or else To_Process_Id (Oid) /= Null_Process_Id
         then
            return;
         end if;

         for P of Process_Table.all loop
            if P.State = Available then
               Pid := P.Pid;
               exit;
            end if;
         end loop;

         if Pid = Null_Process_Id then
            return;
         end if;

         if Processes.Next_Object_Id < Next_Object_Id then
            Processes.Next_Object_Id := Next_Object_Id;
         end if;

         if Processes.Next_Object_Id <= Oid then
            Processes.Next_Object_Id := Oid + 1;
         end if;
      end;

      declare
         Last : Natural := 0;
      begin
         for I in Item.Name'Range loop
            if Item.Name (I) /= ' ' then
               Last := I;
            end if;
         end loop;
         Create_Process_Table_Entry (Pid, Item.Name (1 .. Last));
         Process_Table (Pid).Oid := Rose.Objects.Object_Id (Item.Oid);
      end;

      declare
         P         : Kernel_Process_Entry renames Process_Table (Pid);
         Registers : Storage_Array (1 .. P.Stack'Size / 8)
           with Import, Address => P.Stack'Address;
         Current   : Storage_Array (1 .. P.Current_Params'Size / 8)
           with Import, Address => P.Current_Params'Address;
         Queued    : Storage_Array (1 .. P.Queued_Params'Size / 8)
           with Import, Address => P.Queued_Params'Address;
         FPU_Bytes : Storage_Array (1 .. P.FPU_State'Size / 8)
           with Import, Address => P.FPU_State'Address;
         State     : constant Process_State :=
                       Process_State'Val (Item.State);
         Flags     : Process_Flag_Array := (others => False);
      begin
         for Flag in Process_Flag loop
            Flags (Flag) := (Item.Flags and 2 ** Process_Flag'Pos (Flag)) /= 0;
         end loop;

         P.Priority := Process_Priority'Val (Item.Priority);
         P.Default_Cap := Rose.Capabilities.Capability (Item.Default_Cap);
         P.Receive_Cap := Rose.Capabilities.Capability (Item.Receive_Cap);
         P.Send_Cap := Rose.Capabilities.Capability (Item.Send_Cap);
         P.Command_Endpoint :=
           Rose.Objects.Endpoint_Index (Item.Command_Endpoint);
         Registers := Item.Registers (1 .. Registers'Length);

         --  The process's first FPU or SSE instruction loads this state
         --  (Switch_FPU_State), or starts it clean if it had none.

         P.FPU_Valid := Item.FPU_Valid = 1;
         FPU_Bytes := Item.FPU_State;
         P.Signals := Item.Signals;

         for I in Page_Range_Index loop
            declare
               Saved : Rose.Checkpoints.Saved_Page_Range renames
                         Item.Page_Ranges (Positive (I));
               function Has (Flag : Word_32) return Boolean
               is ((Saved.Flags and Flag) /= 0);
            begin
               P.Page_Ranges (I) :=
                 (Base  => Virtual_Page_Address (Saved.Base),
                  Bound => Virtual_Page_Address (Saved.Bound));
               P.Page_Flags (I) :=
                 (Valid         => Has (Rose.Checkpoints.Range_Valid),
                  Readable      => Has (Rose.Checkpoints.Range_Readable),
                  Writable      => Has (Rose.Checkpoints.Range_Writable),
                  Executable    => Has (Rose.Checkpoints.Range_Executable),
                  Disable_Cache =>
                    Has (Rose.Checkpoints.Range_Disable_Cache),
                  Write_Through =>
                    Has (Rose.Checkpoints.Range_Write_Through),
                  Fixed_Address =>
                    Has (Rose.Checkpoints.Range_Fixed_Address));
            end;
         end loop;

         for I in Registered_Endpoint_Index loop
            declare
               Saved : Rose.Checkpoints.Saved_Endpoint renames
                         Item.Endpoints (Positive (I));
            begin
               P.Endpoints (I) :=
                 (Endpoint    => Rose.Objects.Endpoint_Id (Saved.Endpoint),
                  Receive_Cap =>
                    Rose.Capabilities.Capability (Saved.Receive_Cap),
                  Send_Cap    =>
                    Rose.Capabilities.Capability (Saved.Send_Cap));
            end;
         end loop;

         --  The caps go back at the indices they had, since the process's
         --  code and data refer to them by index.

         Cap_Tables.Set_Last_Cap
           (P.Caps, Rose.Capabilities.Capability (Item.Last_Cap));

         for I in 1 .. Natural (Item.Cap_Count) loop
            declare
               Page   : Rose.Checkpoints.Cap_Page
                 with Import,
                 Address => Pages'Address
                   + Storage_Offset (1 + (I - 1)
                                     / Rose.Checkpoints.Caps_Per_Page)
                 * Page_Bytes;
               Saved  : Rose.Checkpoints.Saved_Cap renames
                          Page.Caps
                            ((I - 1) mod Rose.Checkpoints.Caps_Per_Page + 1);
               Cap    : constant Rose.Capabilities.Capability :=
                          Rose.Capabilities.Capability (Saved.Index);
               Layout : Rose.Capabilities.Layout.Capability_Layout
                 with Import, Address => Saved.Layout'Address;
            begin
               if Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) then
                  Cap_Tables.Activate (P.Caps, Cap);
                  Set_Cap (Pid, Cap, Layout);
               end if;
            end;
         end loop;

         --  Whether a blocked process goes on waiting or makes its call
         --  again depends on the server it called, which may not be
         --  restored yet, so Resume_Restored_Process decides (#519).
         --  The saved call is kept until then.

         case State is
            when Blocked =>
               Current := Item.Current_Params (1 .. Current'Length);
               Flags (Restored_Blocked) := True;
               Flags (Invoke_Reply) := False;
               Flags (Interrupt_Resume) := False;
            when Interrupted =>

               --  A process that was interrupted for a page of its call's
               --  buffer makes the call again (#713). One that was
               --  interrupted for a page of its invocation record is given
               --  the reply it was waiting for (#715).

               if Flags (Reply_Fault) then
                  Current := Item.Current_Params (1 .. Current'Length);
               end if;
               Flags (Interrupt_Resume) :=
                 not Flags (Call_Restart) and then not Flags (Reply_Fault);
               Flags (Wait_Reply) := False;
            when Ready | Running =>
               Current := Item.Current_Params (1 .. Current'Length);
            when Faulted =>
               null;
            when Available | Starting | Killed =>
               P.State := Available;
               Pid := Null_Process_Id;
               return;
         end case;

         --  A queued call is kept, since its caller may go on waiting
         --  for the reply. Anything else, such as an interrupt, belongs
         --  to the boot before the restart.

         Queued := Item.Queued_Params (1 .. Queued'Length);
         P.Queued_Endpoint :=
           Rose.Objects.Endpoint_Index (Item.Queued_Endpoint);
         P.Queued_Cap_Id :=
           Rose.Objects.Capability_Identifier (Item.Queued_Cap_Id);
         Flags (Message_Queued) := Flags (Message_Queued)
           and then P.Queued_Params.Control.Flags
             (Rose.Invocation.Create_Reply_Cap);

         --  The buffers of the calls that this process had received.

         for Saved of Item.Shared loop
            if Saved.Caller /= 0
              and then Restored_Share_Count < Max_Restored_Shares
            then
               Restored_Share_Count := Restored_Share_Count + 1;
               Restored_Shares (Restored_Share_Count) :=
                 (Server => Rose.Objects.Object_Id (Item.Oid),
                  Page   =>
                    Rose.Addresses.Virtual_Page_Address (Saved.Page),
                  Caller => Rose.Objects.Object_Id (Saved.Caller));
            end if;
         end loop;

         --  The callers that were waiting for this process to receive
         --  make their calls again.

         for I in 1 .. Natural'Min (Natural (Item.Waiting_Count),
                                    Rose.Checkpoints.Max_Saved_Waiting)
         loop
            if Undelivered_Count = Max_Undelivered then
               if not Undelivered_Overflow then
                  Rose.Boot.Console.Put_Line
                    ("restore: too many waiting callers;"
                     & " every blocked call is made again");
               end if;
               Undelivered_Overflow := True;
            else
               Undelivered_Count := Undelivered_Count + 1;
               Undelivered (Undelivered_Count) :=
                 Rose.Objects.Object_Id (Item.Waiting (I));
            end if;
         end loop;

         P.Lent_Buffer_Pages := 0;
         Flags (Checkpoint_Page_Fault) := False;
         Flags (Checkpoint_Reply_Held) := False;
         Flags (Fault_Pending) := False;
         Flags (Exit_Pending) := False;
         Flags (Memory_Registered) := False;
         Flags (Boot_Module) := False;
         Flags (Persistent) := True;
         P.Flags := Flags;
         P.State := (if State = Faulted then Faulted else Starting);
      end;

      Success := True;
   end Restore_Process;

   ---------------------------
   -- Restore_Shared_Buffer --
   ---------------------------

   procedure Restore_Shared_Buffer
     (Caller  : Process_Id;
      Server  : Process_Id;
      Success : out Boolean)
   is
      use Rose.Addresses;
      use type Rose.Objects.Object_Id;
      P         : Kernel_Process_Entry renames Process_Table (Caller);
      S         : Kernel_Process_Entry renames Process_Table (Server);
      Params    : Rose.Invocation.Invocation_Record renames
                    P.Current_Params;
      First     : constant Virtual_Page_Address :=
                    Virtual_Address_To_Page
                      (To_Virtual_Address (Params.Buffer_Address));
      Count     : constant Natural :=
                    Natural ((Params.Buffer_Length + Page_Bytes - 1)
                             / Page_Bytes);
      Base      : constant Virtual_Page_Address :=
                    S.Page_Ranges (Invocation_Range_Index).Base;
      Writable  : constant Boolean :=
                    Params.Control.Flags (Rose.Invocation.Writable_Buffer);

      procedure Iterate
        (Process : not null access procedure
           (Slot : Shared_Page_Index;
            Page : Virtual_Page_Address;
            Index : Natural));
      --  Call Process for each of Server's saved slots that holds a page
      --  of Caller's buffer, with the buffer's Index'th page, from 0.

      procedure Check
        (Slot  : Shared_Page_Index;
         Page  : Virtual_Page_Address;
         Index : Natural);

      procedure Map
        (Slot  : Shared_Page_Index;
         Page  : Virtual_Page_Address;
         Index : Natural);

      -----------
      -- Check --
      -----------

      procedure Check
        (Slot  : Shared_Page_Index;
         Page  : Virtual_Page_Address;
         Index : Natural)
      is
         pragma Unreferenced (Page);
      begin
         if S.Shared_Pages (Slot).Pid /= Null_Process_Id
           or else Mapped_Physical_Page
             (Caller, First + Virtual_Page_Address (Index)) = 0
         then
            Success := False;
         end if;
      end Check;

      -------------
      -- Iterate --
      -------------

      procedure Iterate
        (Process : not null access procedure
           (Slot : Shared_Page_Index;
            Page : Virtual_Page_Address;
            Index : Natural))
      is
         Index : Natural := 0;
      begin
         for Share of Restored_Shares (1 .. Restored_Share_Count) loop
            if Share.Server = S.Oid and then Share.Caller = P.Oid then
               if Share.Page < Base
                 or else Share.Page - Base >= Max_Shared_Pages
               then
                  Success := False;
               else
                  Process
                    (Shared_Page_Index (Share.Page - Base + 1),
                     Share.Page, Index);
               end if;
               Index := Index + 1;
            end if;
         end loop;
         if Index /= Count then
            Success := False;
         end if;
      end Iterate;

      ---------
      -- Map --
      ---------

      procedure Map
        (Slot  : Shared_Page_Index;
         Page  : Virtual_Page_Address;
         Index : Natural)
      is
      begin
         S.Shared_Pages (Slot) := (Page => Page, Pid => Caller);
         Map_Page
           (Pid           => Server,
            Virtual_Page  => Page,
            Physical_Page =>
              Mapped_Physical_Page
                (Caller, First + Virtual_Page_Address (Index)),
            Readable      => True,
            Writable      => Writable,
            Executable    => False,
            User          => True);
      end Map;

   begin
      Success := Count > 0;
      if Success then
         Iterate (Check'Access);
      end if;
      if Success then
         Iterate (Map'Access);
      end if;
   end Restore_Shared_Buffer;

   -------------------------
   -- Resume_Blocked_Call --
   -------------------------

   procedure Resume_Blocked_Call
     (Pid     : Process_Id;
      Waiting : out Boolean)
   is
      P      : Kernel_Process_Entry renames Process_Table (Pid);
      Server : constant Process_Id :=
                 (if P.Flags (Wait_Reply)
                  then Called_Server (Pid)
                  else Null_Process_Id);
   begin
      Waiting := False;

      --  A call that is waiting for its reply has a reply cap in the
      --  server. If the server is restored and the call was delivered,
      --  the server answers it. If it wasn't delivered, the call is made
      --  again, and gets a new reply cap.

      if Server /= Null_Process_Id
        and then Process_Table (Server).State /= Available
        and then Process_Table (Server).Flags (Persistent)
        and then not Undelivered_Overflow
      then
         if Is_Undelivered (P.Oid) then
            Delete_Reply_Caps (Server, P.Oid);
         else
            Waiting := Holds_Reply_Cap (Server, P.Oid);
         end if;

         --  The server's request has the address of the caller's buffer
         --  in its shared slots, which the restart unmapped. If it can't
         --  be mapped there again, the call is made again, and the
         --  server's reply to the first one goes nowhere.

         if Waiting
           and then P.Current_Params.Control.Flags
             (Rose.Invocation.Send_Buffer)
         then
            Restore_Shared_Buffer (Pid, Server, Waiting);
            if not Waiting then
               Debug.Put (Pid);
               Rose.Boot.Console.Put_Line
                 (": restore: cannot map its buffer again");
               Delete_Reply_Caps (Server, P.Oid);
            end if;
         end if;
      end if;

      Debug.Put (Pid);
      if Waiting then
         Rose.Boot.Console.Put (": restore: waits for reply from ");
         Debug.Put (Server);
         Rose.Boot.Console.New_Line;
      else
         Rose.Boot.Console.Put_Line (": restore: makes its call again");
         Restart_Call (P);
      end if;
   end Resume_Blocked_Call;

   -----------------------------
   -- Resume_Restored_Process --
   -----------------------------

   procedure Resume_Restored_Process
     (Pid     : Process_Id;
      Success : out Boolean)
   is
   begin
      Release_Restore_Holds;

      Success := Pid in Process_Table'Range
        and then Process_Table (Pid).State = Starting
        and then Process_Table (Pid).Flags (Persistent);

      if Success then

         --  Set_Current_State works out how the process returns to user
         --  mode from the state it leaves, but Restore_Process has set
         --  that already, from the state the process was in.

         declare
            use type Rose.Objects.Endpoint_Index;
            P       : Kernel_Process_Entry renames Process_Table (Pid);
            Waiting : Boolean := False;
         begin
            if P.Flags (Restored_Blocked) then
               P.Flags (Restored_Blocked) := False;
               Resume_Blocked_Call (Pid, Waiting);
            end if;

            --  A process with a Restorable interface is told, the next
            --  time it receives, that it has been restored.

            P.Flags (Restore_Notice) :=
              Find_Endpoint
                (Pid, Rose.Interfaces.Restorable.Restored_Endpoint) /= 0;

            if Waiting then

               --  The server's reply makes it ready, as before the
               --  restart. The scheduler's queue holds blocked processes
               --  too.

               P.State := Blocked;
               Rose.Kernel.Processes.Queue.Queue_Process (Pid);
               return;
            end if;

            declare
               Invoke_Reply_Set : constant Boolean :=
                                    P.Flags (Invoke_Reply);
               Interrupted_Set  : constant Boolean :=
                                    P.Flags (Interrupt_Resume);
            begin
               Set_Current_State (Pid, Ready);
               P.Flags (Invoke_Reply) := Invoke_Reply_Set;
               P.Flags (Interrupt_Resume) := Interrupted_Set;
            end;
         end;

         Rose.Kernel.Processes.Queue.Queue_Process (Pid);
      end if;
   end Resume_Restored_Process;

   ---------------
   -- Save_Caps --
   ---------------

   procedure Save_Caps
     (P     : Kernel_Process_Entry;
      Pages : System.Address)
   is
      Page_Index : Storage_Offset := 0;
      Cap_Index  : Natural := 0;
   begin
      for Cap in 1 .. Cap_Tables.Last_Cap (P.Caps) loop
         if Cap_Tables.Is_Active (P.Caps, Cap) then
            if Cap_Index = Rose.Checkpoints.Caps_Per_Page then
               Page_Index := Page_Index + 1;
               Cap_Index := 0;
            end if;

            Cap_Index := Cap_Index + 1;

            declare
               Page   : Rose.Checkpoints.Cap_Page
                 with Import, Address => Pages + Page_Index * Page_Bytes;
               Layout : Rose.Capabilities.Layout.Capability_Layout;
               Bytes  : Storage_Array
                          (1 .. Rose.Checkpoints.Saved_Layout_Bytes)
                 with Import, Address => Layout'Address;
            begin
               --  Get_Cap reads through the cap cache, which may hold a
               --  newer layout than the cap pages.
               Get_Cap (P.Pid, Cap, Layout);
               Page.Caps (Cap_Index).Index := Word_32 (Cap);
               Page.Caps (Cap_Index).Layout := Bytes;
            end;
         end if;
      end loop;
   end Save_Caps;

   ------------------
   -- Save_Process --
   ------------------

   procedure Save_Process
     (P       : Kernel_Process_Entry;
      Item    : in out Rose.Checkpoints.Process_Record;
      Success : out Boolean)
   is
      Registers : Storage_Array (1 .. P.Stack'Size / 8)
        with Import, Address => P.Stack'Address;
      Current   : Storage_Array (1 .. P.Current_Params'Size / 8)
        with Import, Address => P.Current_Params'Address;
      Queued    : Storage_Array (1 .. P.Queued_Params'Size / 8)
        with Import, Address => P.Queued_Params'Address;
      FPU_Bytes : Storage_Array (1 .. P.FPU_State'Size / 8)
        with Import, Address => P.FPU_State'Address;
      Flags     : Word_32 := 0;
      Caps      : constant Natural := Active_Cap_Count (P);
      Waiting   : Kernel_Process_Access := P.Waiting_First;
   begin
      Success := True;

      for Flag in Process_Flag loop
         if P.Flags (Flag) then
            Flags := Flags or 2 ** Process_Flag'Pos (Flag);
         end if;
      end loop;

      Item.Magic := Rose.Checkpoints.Process_Record_Magic;
      Item.Oid := Word_64 (P.Oid);
      Item.Name := P.Name;
      Item.State := Process_State'Pos (P.State);
      Item.Flags := Flags;
      Item.Priority := Word_32 (P.Priority);
      Item.Default_Cap := Word_32 (P.Default_Cap);
      Item.Receive_Cap := Word_32 (P.Receive_Cap);
      Item.Send_Cap := Word_32 (P.Send_Cap);
      Item.Last_Cap := Word_32 (Cap_Tables.Last_Cap (P.Caps));
      Item.Cap_Count := Word_32 (Caps);
      Item.Cap_Page_Count := Word_32 (Cap_Page_Count (Caps));
      Item.Command_Endpoint := Word_32 (P.Command_Endpoint);
      Item.Waiting_Endpoint := Word_32 (P.Waiting_Endpoint);
      Item.Waiting_Cap_Id := Word_32 (P.Waiting_Cap_Id);
      Item.Queued_Endpoint := Word_32 (P.Queued_Endpoint);
      Item.Queued_Cap_Id := Word_32 (P.Queued_Cap_Id);
      Item.Checkpoint_Fault := Word_32 (P.Checkpoint_Fault);

      while Waiting /= null loop
         if Item.Waiting_Count = Rose.Checkpoints.Max_Saved_Waiting then
            Success := False;
            return;
         end if;
         Item.Waiting_Count := Item.Waiting_Count + 1;
         Item.Waiting (Natural (Item.Waiting_Count)) :=
           Word_64 (Waiting.Oid);
         Waiting := Waiting.Waiting_Next;
      end loop;

      Item.Registers (1 .. Registers'Length) := Registers;
      Item.Current_Params (1 .. Current'Length) := Current;
      Item.Queued_Params (1 .. Queued'Length) := Queued;
      Item.FPU_Valid := (if P.FPU_Valid then 1 else 0);
      Item.FPU_State := FPU_Bytes;
      Item.Signals := P.Signals;

      for I in Page_Range_Index loop
         declare
            Range_Flags : constant Page_Flags := P.Page_Flags (I);
            Saved       : Rose.Checkpoints.Saved_Page_Range renames
                            Item.Page_Ranges (Positive (I));
         begin
            Saved.Base := Word_32 (P.Page_Ranges (I).Base);
            Saved.Bound := Word_32 (P.Page_Ranges (I).Bound);
            Saved.Flags :=
              (if Range_Flags.Valid
               then Rose.Checkpoints.Range_Valid else 0)
              or (if Range_Flags.Readable
                  then Rose.Checkpoints.Range_Readable else 0)
              or (if Range_Flags.Writable
                  then Rose.Checkpoints.Range_Writable else 0)
              or (if Range_Flags.Executable
                  then Rose.Checkpoints.Range_Executable else 0)
              or (if Range_Flags.Disable_Cache
                  then Rose.Checkpoints.Range_Disable_Cache else 0)
              or (if Range_Flags.Write_Through
                  then Rose.Checkpoints.Range_Write_Through else 0)
              or (if Range_Flags.Fixed_Address
                  then Rose.Checkpoints.Range_Fixed_Address else 0);
         end;
      end loop;

      for I in Shared_Page_Index loop
         if P.Shared_Pages (I).Pid /= Null_Process_Id then
            Item.Shared (Positive (I)) :=
              (Page   => Word_32 (P.Shared_Pages (I).Page),
               Unused => 0,
               Caller =>
                 Word_64 (Process_Table (P.Shared_Pages (I).Pid).Oid));
         end if;
      end loop;

      for I in Registered_Endpoint_Index loop
         declare
            Endpoint : Registered_Endpoint_Record renames P.Endpoints (I);
            Saved    : Rose.Checkpoints.Saved_Endpoint renames
                         Item.Endpoints (Positive (I));
         begin
            Saved.Endpoint := Word_64 (Endpoint.Endpoint);
            Saved.Receive_Cap := Word_32 (Endpoint.Receive_Cap);
            Saved.Send_Cap := Word_32 (Endpoint.Send_Cap);
         end;
      end loop;
   end Save_Process;

   ------------------
   -- Write_Tables --
   ------------------

   procedure Write_Tables
     (Storage    : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean)
   is
      Available     : constant Natural :=
                        Natural (Storage'Length / Page_Bytes);
      Process_Count : Natural := 0;
      Next_Page     : Natural := 1;
      Record_Count  : Natural := 0;
      Record_Pages  : Natural;

      function Page_Address (Index : Positive) return System.Address
      is (Storage (Storage'First
                   + Storage_Offset (Index - 1) * Page_Bytes)'Address);

   begin
      --  The FPU owner's state is in the registers, not its entry.

      Save_Owner_FPU_State;

      --  The records are those up to the last that is used.

      for Id in 1 .. Rose.Kernel.Derivations.Max_Records loop
         if Rose.Kernel.Derivations.Is_Used
           (Derivation_Records, Rose.Kernel.Derivations.Record_Id (Id))
         then
            Record_Count := Id;
         end if;
      end loop;

      Record_Pages :=
        (Record_Count + Rose.Checkpoints.Records_Per_Page - 1)
        / Rose.Checkpoints.Records_Per_Page;

      Page_Count := 1 + Record_Pages;
      for P of Process_Table.all loop
         if Is_Saved (P) then
            Process_Count := Process_Count + 1;
            Page_Count := Page_Count + 1
              + Cap_Page_Count (Active_Cap_Count (P));
         end if;
      end loop;

      if Page_Count > Available then
         Success := False;
         Page_Count := 0;
         return;
      end if;

      Storage (Storage'First
               .. Storage'First + Storage_Offset (Page_Count) * Page_Bytes
                 - 1) := (others => 0);

      declare
         Header : Rose.Checkpoints.Kernel_Table_Header
           with Import, Address => Page_Address (1);

         procedure Save_Interrupt
           (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
            Object    : Rose.Objects.Object_Id;
            Cap       : Rose.Capabilities.Capability);

         --------------------
         -- Save_Interrupt --
         --------------------

         procedure Save_Interrupt
           (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
            Object    : Rose.Objects.Object_Id;
            Cap       : Rose.Capabilities.Capability)
         is
            Pid : constant Process_Id := To_Process_Id (Object);
         begin
            if Pid in Process_Table'Range
              and then Is_Saved (Process_Table (Pid))
            then
               if Header.Interrupt_Count
                 = Rose.Checkpoints.Max_Saved_Interrupts
               then
                  Success := False;
               else
                  Header.Interrupt_Count := Header.Interrupt_Count + 1;
                  Header.Interrupts (Natural (Header.Interrupt_Count)) :=
                    (Vector => Word_32 (Interrupt),
                     Cap    => Word_32 (Cap),
                     Oid    => Word_64 (Object));
               end if;
            end if;
         end Save_Interrupt;

      begin
         Success := True;
         Header.Magic := Rose.Checkpoints.Kernel_Table_Magic;
         Header.Version := Rose.Checkpoints.Kernel_Table_Version;
         Header.Page_Count := Word_32 (Page_Count);
         Header.Process_Count := Word_32 (Process_Count);
         Header.Next_Object_Id := Word_64 (Next_Object_Id);
         Header.Record_Page_Count := Word_32 (Record_Pages);
         Header.Record_Count := Word_32 (Record_Count);
         Rose.Kernel.Interrupts.Iterate_Cap_Handlers (Save_Interrupt'Access);
         if not Success then
            Page_Count := 0;
            return;
         end if;
      end;

      if Record_Count > 0 then
         declare
            use Rose.Kernel.Derivations;
            Saved : Rose.Checkpoints.Saved_Record_Array (1 .. Record_Count)
              with Import, Address => Page_Address (2);
         begin
            for Id in Saved'Range loop
               if Is_Used (Derivation_Records, Record_Id (Id)) then
                  Saved (Id) :=
                    (Parent => Word_16 (Parent (Derivation_Records,
                                                Record_Id (Id))),
                     Flags  =>
                       Word_16 (Rose.Checkpoints.Record_Used)
                       + (if Is_Revoked (Derivation_Records,
                                         Record_Id (Id))
                          then Word_16 (Rose.Checkpoints.Record_Revoked)
                          else 0));
               end if;
            end loop;
         end;
      end if;

      Next_Page := 1 + Record_Pages;

      for P of Process_Table.all loop
         if Is_Saved (P) then
            Next_Page := Next_Page + 1;
            declare
               Item : Rose.Checkpoints.Process_Record
                 with Import, Address => Page_Address (Next_Page);
            begin
               Save_Process (P, Item, Success);
               if not Success then
                  Page_Count := 0;
                  return;
               end if;
               if Item.Cap_Page_Count > 0 then
                  Save_Caps (P, Page_Address (Next_Page + 1));
               end if;
               Next_Page := Next_Page + Natural (Item.Cap_Page_Count);
            end;
         end if;
      end loop;

      Success := True;
   end Write_Tables;

end Rose.Kernel.Processes.Snapshot;
