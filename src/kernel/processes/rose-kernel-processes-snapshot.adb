with Rose.Arch.Interrupt_Table;
with Rose.Checkpoints;
with Rose.Limits;

with Rose.Kernel.Interrupts;
with Rose.Kernel.Processes.Queue;

package body Rose.Kernel.Processes.Snapshot is

   use System.Storage_Elements;
   use Rose.Words;

   Page_Bytes : constant := Rose.Limits.Page_Size;

   function Is_Saved (P : Kernel_Process_Entry) return Boolean
   is (P.State /= Available and then P.Flags (Persistent));

   function Active_Cap_Count (P : Kernel_Process_Entry) return Natural;

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
      for Cap in 1 .. P.Last_Cap loop
         if P.Is_Active (Cap) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Cap_Count;

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
     (Pages   : System.Storage_Elements.Storage_Array;
      Pid     : out Process_Id;
      Success : out Boolean)
   is
      use type Rose.Capabilities.Capability;
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
        or else Item.Last_Cap > Max_Capability_Index
        or else Item.State > Process_State'Pos (Process_State'Last)
        or else Item.Priority not in
          Process_Priority'Pos (Process_Priority'First)
            .. Process_Priority'Pos (Process_Priority'Last)
      then
         return;
      end if;

      Pid := To_Process_Id (Rose.Objects.Object_Id (Item.Oid));

      if Pid not in Process_Table'Range
        or else Process_Table (Pid).State /= Available
        or else Process_Table (Pid).Last_Cap /= 0
      then
         Pid := Null_Process_Id;
         return;
      end if;

      declare
         Last : Natural := 0;
      begin
         for I in Item.Name'Range loop
            if Item.Name (I) /= ' ' then
               Last := I;
            end if;
         end loop;
         Create_Process_Table_Entry (Pid, Item.Name (1 .. Last));
      end;

      declare
         P         : Kernel_Process_Entry renames Process_Table (Pid);
         Registers : Storage_Array (1 .. P.Stack'Size / 8)
           with Import, Address => P.Stack'Address;
         Current   : Storage_Array (1 .. P.Current_Params'Size / 8)
           with Import, Address => P.Current_Params'Address;
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

         P.Last_Cap := Rose.Capabilities.Capability (Item.Last_Cap);

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
               if Cap in 1 .. P.Last_Cap then
                  P.Is_Active (Cap) := True;
                  Set_Cap (Pid, Cap, Layout);
               end if;
            end;
         end loop;

         --  Whatever a blocked process was waiting for was lost in the
         --  restart: a server that is not persistent has no record of
         --  its request, and one that is was restarted at its own call.
         --  So it makes its call again: the system call page pushed three
         --  words before entering the kernel, and starts at FFFF_0000.

         case State is
            when Blocked =>
               P.Stack.EIP := 16#FFFF_0000#;
               P.Stack.ESP := P.Stack.ESP + 12;
               Flags (Receive_Any) := False;
               Flags (Receive_Cap) := False;
               Flags (Invoke_Reply) := False;
               Flags (Wait_Reply) := False;
               Flags (Interrupt_Resume) := False;
            when Interrupted =>
               Flags (Interrupt_Resume) := True;
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

         Flags (Message_Queued) := False;
         Flags (Checkpoint_Page_Fault) := False;
         Flags (Fault_Pending) := False;
         Flags (Boot_Module) := False;
         Flags (Persistent) := True;
         P.Flags := Flags;
         P.State := (if State = Faulted then Faulted else Starting);
      end;

      if Pid >= Next_Pid then
         Next_Pid := Pid + 1;
      end if;

      Success := True;
   end Restore_Process;

   -----------------------------
   -- Resume_Restored_Process --
   -----------------------------

   procedure Resume_Restored_Process
     (Pid     : Process_Id;
      Success : out Boolean)
   is
   begin
      Success := Pid in Process_Table'Range
        and then Process_Table (Pid).State = Starting
        and then Process_Table (Pid).Flags (Persistent);

      if Success then

         --  Set_Current_State works out how the process returns to user
         --  mode from the state it leaves, but Restore_Process has set
         --  that already, from the state the process was in.

         declare
            P                : Kernel_Process_Entry renames
                                 Process_Table (Pid);
            Invoke_Reply_Set : constant Boolean := P.Flags (Invoke_Reply);
            Interrupted_Set  : constant Boolean :=
                                 P.Flags (Interrupt_Resume);
         begin
            Set_Current_State (Pid, Ready);
            P.Flags (Invoke_Reply) := Invoke_Reply_Set;
            P.Flags (Interrupt_Resume) := Interrupted_Set;
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
      for Cap in 1 .. P.Last_Cap loop
         if P.Is_Active (Cap) then
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
      Item.Last_Cap := Word_32 (P.Last_Cap);
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

      function Page_Address (Index : Positive) return System.Address
      is (Storage (Storage'First
                   + Storage_Offset (Index - 1) * Page_Bytes)'Address);

   begin
      Page_Count := 1;
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
         Rose.Kernel.Interrupts.Iterate_Cap_Handlers (Save_Interrupt'Access);
         if not Success then
            Page_Count := 0;
            return;
         end if;
      end;

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
