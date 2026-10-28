with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Console_IO;
with Rose.Invocation;
with Rose.Limits;
with Rose.Objects;
with Rose.System_Calls;

with Rose.Interfaces.Log_Reader.Client;
with Rose.Interfaces.Memory_Restore.Client;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage_Checkpoint.Client;

package body Restore.Checkpoints is

   use System.Storage_Elements;
   use Rose.Interfaces.Log_Reader.Client;
   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;
   use type Rose.Capabilities.Capability;

   Page_Size : constant := Rose.Limits.Page_Size;

   Page : Storage_Array (1 .. Page_Size)
     with Alignment => Page_Size;
   --  Read_Page returns its page here, in the invocation buffer.

   Kernel_Header : Rose.Checkpoints.Kernel_Table_Header
     with Import, Address => Page'Address;
   Store_Header  : Rose.Checkpoints.Store_Table_Header
     with Import, Address => Page'Address;
   Region_Table  : Rose.Checkpoints.Region_Page
     with Import, Address => Page'Address;
   Mem_Header    : Rose.Checkpoints.Mem_Table_Header
     with Import, Address => Page'Address;
   Mem_Process   : Rose.Checkpoints.Mem_Process_Record
     with Import, Address => Page'Address;
   Header        : Rose.Checkpoints.Header_Page
     with Import, Address => Page'Address;

   Send_Page : Storage_Array (1 .. Page_Size)
     with Alignment => Page_Size;
   --  A page to send: sending from the invocation buffer would clear it
   --  first.

   Max_Process_Pages : constant := 8;

   Process_Pages : Storage_Array (1 .. Max_Process_Pages * Page_Size)
     with Alignment => Page_Size;
   Process_Item  : Rose.Checkpoints.Process_Record
     with Import, Address => Process_Pages'Address;
   --  A process record page and its cap pages, for the kernel.

   Max_Processes : constant := Rose.Limits.Max_Processes;

   type Restored_Process is
      record
         Oid     : Rose.Words.Word_64 := 0;
         Process : Rose.Capabilities.Capability := 0;
         Mem_Pid : Rose.Words.Word_32 := 0;
      end record;
   --  Process is the restored process's Kernel_Process interface.

   Restored       : array (1 .. Max_Processes) of Restored_Process;
   Restored_Count : Natural := 0;

   function Find_Restored (Oid : Rose.Words.Word_64) return Natural;
   --  The index in Restored of the process Oid, or zero.

   type Entry_Process is
      record
         Mem_Pid : Rose.Words.Word_32;
         Oid     : Rose.Words.Word_64;
      end record;

   Entry_Processes : array (1 .. Max_Processes) of Entry_Process;
   Entry_Process_Count : Natural := 0;
   --  The memory server's processes in the entry being read.

   type Page_Source is
      record
         Oid      : Rose.Words.Word_64;
         Page     : Rose.Words.Word_32;
         Location : Rose.Words.Word_64;
         Index    : Positive;
      end record;
   --  The newest saved contents of virtual page Page of process Oid are
   --  page Index of the entry at Location.  Pages are named by object
   --  id, which is never used again, not by the memory server's process
   --  id, which can be.

   Max_Sources  : constant := 4096;
   Sources      : array (1 .. Max_Sources) of Page_Source;
   Source_Count : Natural := 0;

   type Page_Reference is
      record
         Mem_Pid : Rose.Words.Word_32;
         Page    : Rose.Words.Word_32;
      end record;

   Max_References  : constant := 4096;
   References      : array (1 .. Max_References) of Page_Reference;
   Reference_Count : Natural := 0;
   --  The pages listed in the header pages of the entry being read.

   function Read
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Index    : Positive)
      return Boolean;
   --  Read page Index of the entry at Location into Page.

   procedure Read_Table_Sizes
     (Reader       : Log_Reader_Client;
      Location     : Rose.Words.Word_64;
      Kernel_Pages : out Natural;
      Store_Pages  : out Natural;
      Mem_Pages    : out Natural;
      Success      : out Boolean);
   --  The pages that the kernel's, storage server's and memory server's
   --  tables take at the start of the entry at Location.

   procedure Add_Entry_Pages
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Success  : out Boolean);
   --  Record the pages saved in the entry at Location in Sources, as the
   --  newest contents of those pages so far.

   procedure Fail (Message : String);

   ---------------------
   -- Add_Entry_Pages --
   ---------------------

   procedure Add_Entry_Pages
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Success  : out Boolean)
   is
      use type Rose.Checkpoints.Header_Page_Flags;
      Kernel_Pages, Store_Pages, Mem_Pages : Natural;
      Index : Positive;
   begin
      Read_Table_Sizes
        (Reader, Location, Kernel_Pages, Store_Pages, Mem_Pages, Success);
      if not Success then
         return;
      end if;
      Success := False;

      --  Which object each of the memory server's ids meant in this
      --  entry.

      Entry_Process_Count := 0;
      for I in 1 .. Mem_Pages - 1 loop
         if not Read (Reader, Location, Kernel_Pages + Store_Pages + 1 + I)
           or else Entry_Process_Count = Max_Processes
         then
            return;
         end if;
         Entry_Process_Count := Entry_Process_Count + 1;
         Entry_Processes (Entry_Process_Count) :=
           (Mem_Process.Mem_Pid, Mem_Process.Oid);
      end loop;

      --  The header pages, then the pages they list, in order.

      Reference_Count := 0;
      Index := Kernel_Pages + Store_Pages + Mem_Pages + 1;
      loop
         if not Read (Reader, Location, Index) then
            return;
         end if;
         for I in 1 .. Header.Count loop
            if Reference_Count = Max_References then
               return;
            end if;
            Reference_Count := Reference_Count + 1;
            References (Reference_Count) :=
              (Rose.Words.Word_32 (Header.Pages (I).Process),
               Rose.Words.Word_32 (Header.Pages (I).Page));
         end loop;
         Index := Index + 1;
         exit when (Header.Flags and Rose.Checkpoints.Is_Last_Header_Page)
           /= 0;
      end loop;

      for I in 1 .. Reference_Count loop
         declare
            Ref : Page_Reference renames References (I);
            Oid : Rose.Words.Word_64 := 0;
            Found : Boolean := False;
         begin
            for P of Entry_Processes (1 .. Entry_Process_Count) loop
               if P.Mem_Pid = Ref.Mem_Pid then
                  Oid := P.Oid;
               end if;
            end loop;

            if Oid /= 0 then
               for Source of Sources (1 .. Source_Count) loop
                  if Source.Oid = Oid and then Source.Page = Ref.Page then
                     Source.Location := Location;
                     Source.Index := Index + I - 1;
                     Found := True;
                  end if;
               end loop;

               if not Found then
                  if Source_Count = Max_Sources then
                     return;
                  end if;
                  Source_Count := Source_Count + 1;
                  Sources (Source_Count) :=
                    (Oid, Ref.Page, Location, Index + I - 1);
               end if;
            end if;
         end;
      end loop;

      Success := True;
   end Add_Entry_Pages;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String) is
   begin
      Rose.Console_IO.Put ("restore: ");
      Rose.Console_IO.Put_Line (Message);
   end Fail;

   -------------------
   -- Find_Restored --
   -------------------

   function Find_Restored (Oid : Rose.Words.Word_64) return Natural is
   begin
      for I in 1 .. Restored_Count loop
         if Restored (I).Oid = Oid then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Restored;

   ----------
   -- Read --
   ----------

   function Read
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Index    : Positive)
      return Boolean
   is
   begin
      Read_Page (Reader, Location, Index, Page);
      return not Has_Error;
   end Read;

   ----------------------
   -- Read_Table_Sizes --
   ----------------------

   procedure Read_Table_Sizes
     (Reader       : Log_Reader_Client;
      Location     : Rose.Words.Word_64;
      Kernel_Pages : out Natural;
      Store_Pages  : out Natural;
      Mem_Pages    : out Natural;
      Success      : out Boolean)
   is
   begin
      Kernel_Pages := 0;
      Store_Pages := 0;
      Mem_Pages := 0;
      Success := False;

      if not Read (Reader, Location, 1)
        or else Kernel_Header.Magic /= Rose.Checkpoints.Kernel_Table_Magic
      then
         return;
      end if;
      Kernel_Pages := Natural (Kernel_Header.Page_Count);

      if not Read (Reader, Location, Kernel_Pages + 1)
        or else Store_Header.Magic /= Rose.Checkpoints.Store_Table_Magic
      then
         return;
      end if;
      Store_Pages := Natural (Store_Header.Page_Count);

      if not Read (Reader, Location, Kernel_Pages + Store_Pages + 1)
        or else Mem_Header.Magic /= Rose.Checkpoints.Mem_Table_Magic
      then
         return;
      end if;
      Mem_Pages := Natural (Mem_Header.Page_Count);

      Success := True;
   end Read_Table_Sizes;

   -------------
   -- Restore --
   -------------

   procedure Restore
     (Reader_Cap : Rose.Capabilities.Capability;
      Checkpoint : Rose.Words.Word_64;
      Success    : out Boolean)
   is
      use Rose.Interfaces.Memory_Restore.Client;
      use Rose.Interfaces.Storage_Checkpoint.Client;
      Reader          : Log_Reader_Client;
      Store           : Storage_Checkpoint_Client;
      Mem             : Memory_Restore_Client;
      Kernel_Pages    : Natural;
      Store_Pages     : Natural;
      Mem_Pages       : Natural;
      Interrupts      : Rose.Checkpoints.Saved_Interrupt_Array;
      Interrupt_Count : Natural;
      Process_Count   : Natural;
      Next_Object_Id  : Rose.Objects.Object_Id;
      Restored_Pages  : Natural := 0;
   begin
      Success := False;
      Restored_Count := 0;
      Source_Count := 0;

      Open (Reader, Reader_Cap);
      Open (Store, Store_Restore_Cap);
      Open (Mem, Memory_Restore_Cap);

      Rose.System_Calls.Use_Buffer (Page'Address, Page'Length);

      Read_Table_Sizes
        (Reader, Checkpoint, Kernel_Pages, Store_Pages, Mem_Pages, Success);
      if not Success then
         Fail ("cannot read the checkpoint's tables");
         Rose.System_Calls.Use_Buffer (System.Null_Address, 0);
         return;
      end if;
      Success := False;

      --  The kernel's header page lists the processes and interrupts.

      if not Read (Reader, Checkpoint, 1) then
         Fail ("cannot read the kernel's tables");
         return;
      end if;
      Process_Count := Natural (Kernel_Header.Process_Count);
      Interrupt_Count := Natural (Kernel_Header.Interrupt_Count);
      Interrupts := Kernel_Header.Interrupts;
      Next_Object_Id := Rose.Objects.Object_Id (Kernel_Header.Next_Object_Id);

      --  The regions first: the memory server's segments use them.

      if not Read (Reader, Checkpoint, Kernel_Pages + 1) then
         Fail ("cannot read the storage tables");
         return;
      end if;

      declare
         Region_Count : constant Natural :=
                          Natural (Store_Header.Region_Count);
      begin
         for I in 1 .. Region_Count loop
            if (I - 1) mod Rose.Checkpoints.Regions_Per_Page = 0
              and then not Read
                (Reader, Checkpoint,
                 Kernel_Pages + 2
                 + (I - 1) / Rose.Checkpoints.Regions_Per_Page)
            then
               Fail ("cannot read the storage tables");
               return;
            end if;

            declare
               Region : Rose.Checkpoints.Saved_Region renames
                          Region_Table.Regions
                            ((I - 1) mod Rose.Checkpoints.Regions_Per_Page
                             + 1);
            begin
               Restore_Region
                 (Item        => Store,
                  Index       => Natural (Region.Index),
                  Device      => Natural (Region.Device),
                  Base        => Rose.Objects.Object_Id (Region.Base),
                  Bound       => Rose.Objects.Object_Id (Region.Bound),
                  Alloc_Index => Natural (Region.Alloc_Index),
                  Alloc_Banks => Natural (Region.Alloc_Banks));
            end;
         end loop;

         Rose.Console_IO.Put ("restore: regions: ");
         Rose.Console_IO.Put (Region_Count);
         Rose.Console_IO.New_Line;
      end;

      --  Then the storage server is given its tables back, to read the
      --  rest of its state from them: which pages are fresh (#327).

      for I in 1 .. Store_Pages loop
         if not Read (Reader, Checkpoint, Kernel_Pages + I) then
            Fail ("cannot read the storage tables");
            return;
         end if;
         Send_Page := Page;
         Restore_Table_Page (Store, I, Send_Page);
      end loop;

      --  The kernel's processes, which can't run until the memory server
      --  has their pages.

      declare
         Index : Positive := 2;
      begin
         for P in 1 .. Process_Count loop
            if not Read (Reader, Checkpoint, Index) then
               Fail ("cannot read a process record");
               return;
            end if;
            Process_Pages (1 .. Page_Size) := Page;

            declare
               Cap_Pages : constant Natural :=
                             Natural (Process_Item.Cap_Page_Count);
               Params    : aliased Rose.Invocation.Invocation_Record;
            begin
               if 1 + Cap_Pages > Max_Process_Pages then
                  Fail ("a process has too many caps");
                  return;
               end if;

               for C in 1 .. Cap_Pages loop
                  if not Read (Reader, Checkpoint, Index + C) then
                     Fail ("cannot read a process's caps");
                     return;
                  end if;
                  Process_Pages
                    (Storage_Offset (C) * Page_Size + 1
                     .. Storage_Offset (C + 1) * Page_Size) := Page;
               end loop;

               Rose.System_Calls.Initialize_Send (Params, Restore_Process_Cap);
               Rose.System_Calls.Send_Object_Id (Params, Next_Object_Id);
               Rose.System_Calls.Send_Buffer
                 (Params, Storage_Count (1 + Cap_Pages) * Page_Size,
                  Process_Pages'Address, False);
               Rose.System_Calls.Receive_Caps (Params, 4);
               Rose.System_Calls.Receive_Words (Params, 2);
               Rose.System_Calls.Invoke_Capability (Params);

               if Params.Control.Flags (Rose.Invocation.Error) then
                  Fail ("the kernel cannot restore a process");
                  return;
               end if;

               Restored_Count := Restored_Count + 1;
               Restored (Restored_Count) :=
                 (Oid     => Process_Item.Oid,
                  Process => Params.Caps (0),
                  Mem_Pid => 0);

               Index := Index + 1 + Cap_Pages;
            end;
         end loop;
      end;

      --  The memory server's processes and their segments.

      for I in 1 .. Mem_Pages - 1 loop
         if not Read (Reader, Checkpoint, Kernel_Pages + Store_Pages + 1 + I)
         then
            Fail ("cannot read a memory process record");
            return;
         end if;

         declare
            R : constant Natural := Find_Restored (Mem_Process.Oid);
         begin
            if R = 0 then
               Fail ("a memory process has no kernel process");
               return;
            end if;

            Restored (R).Mem_Pid := Mem_Process.Mem_Pid;
            Restore_Process
              (Mem, Restored (R).Process, Natural (Mem_Process.Mem_Pid));

            for Segment of
              Mem_Process.Segments
                (1 .. Natural (Mem_Process.Segment_Count))
            loop
               if Segment.Region_Base /= 0 or else Segment.Region_Bound /= 0
               then
                  declare
                     Region : constant Rose.Interfaces.Region.Client
                       .Region_Client :=
                         Find_Region
                           (Store,
                            Rose.Objects.Object_Id (Segment.Region_Base),
                            Rose.Objects.Object_Id (Segment.Region_Bound));
                  begin
                     Restore_Segment
                       (Item          => Mem,
                        Mem_Pid       => Natural (Mem_Process.Mem_Pid),
                        Base          => Segment.Base,
                        Bound         => Segment.Bound,
                        Limit         => Segment.Limit,
                        Flags         => Segment.Flags,
                        Region        =>
                          Rose.Interfaces.Region.Client.Get_Interface_Cap
                            (Region),
                        Region_Offset =>
                          Rose.Words.Word (Segment.Region_Offset));
                  end;
               else
                  Restore_Zero_Segment
                    (Item    => Mem,
                     Mem_Pid => Natural (Mem_Process.Mem_Pid),
                     Base    => Segment.Base,
                     Bound   => Segment.Bound,
                     Limit   => Segment.Limit,
                     Flags   => Segment.Flags);
               end if;
            end loop;
         end;
      end loop;

      --  The newest saved contents of each page, from all the entries up
      --  to the checkpoint: a page saved in an older entry and not
      --  changed since is only there.

      declare
         Location : Rose.Words.Word_64 := First_Entry (Reader);
         Added    : Boolean;
      begin
         while Location /= 0 and then Location <= Checkpoint loop
            Add_Entry_Pages (Reader, Location, Added);
            if not Added then
               Fail ("cannot read the pages of an entry");
               return;
            end if;
            exit when Location = Checkpoint;
            Location := Next_Entry (Reader, Location);
         end loop;
      end;

      for Source of Sources (1 .. Source_Count) loop
         declare
            R : constant Natural := Find_Restored (Source.Oid);
         begin
            if R /= 0 and then Restored (R).Mem_Pid /= 0 then
               if not Read (Reader, Source.Location, Source.Index) then
                  Fail ("cannot read a saved page");
                  return;
               end if;
               Send_Page := Page;
               Restore_Page
                 (Mem, Natural (Restored (R).Mem_Pid),
                  Rose.Words.Word (Source.Page), Send_Page);
               Restored_Pages := Restored_Pages + 1;
            end if;
         end;
      end loop;

      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);

      --  The interrupts the restored processes handle.

      for Saved of Interrupts (1 .. Interrupt_Count) loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Restore_Interrupt_Cap);
            Rose.System_Calls.Send_Word (Params, Saved.Vector);
            Rose.Invocation.Send_Object_Id
              (Params, Rose.Objects.Object_Id (Saved.Oid));
            Rose.System_Calls.Send_Word (Params, Saved.Cap);
            Rose.System_Calls.Invoke_Capability (Params);
            if Params.Control.Flags (Rose.Invocation.Error) then
               Fail ("the kernel cannot restore an interrupt");
            end if;
         end;
      end loop;

      Rose.Console_IO.Put ("restore: processes: ");
      Rose.Console_IO.Put (Restored_Count);
      Rose.Console_IO.Put ("; pages: ");
      Rose.Console_IO.Put (Restored_Pages);
      Rose.Console_IO.Put ("; interrupts: ");
      Rose.Console_IO.Put (Interrupt_Count);
      Rose.Console_IO.New_Line;

      Success := True;
   end Restore;

   ----------------------
   -- Resume_Processes --
   ----------------------

   procedure Resume_Processes is
   begin
      for P of Restored (1 .. Restored_Count) loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Resume_Process_Cap);
            Rose.Invocation.Send_Object_Id
              (Params, Rose.Objects.Object_Id (P.Oid));
            Rose.System_Calls.Invoke_Capability (Params);
            if Params.Control.Flags (Rose.Invocation.Error) then
               Fail ("the kernel cannot resume a process");
            end if;
         end;
      end loop;
   end Resume_Processes;

end Restore.Checkpoints;
