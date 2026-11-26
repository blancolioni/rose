with Rose.Boot.Console;

with Rose.Limits;
with Rose.Words;

with Rose.Kernel.Checkpoint_Copies;
with Rose.Kernel.Clock;
with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Capabilities.Page_Table is

   use type Rose.Kernel.Processes.Process_Id;

   procedure Map_Process_Page
     (Params : Rose.Invocation.Invocation_Access;
      Pid    : Rose.Kernel.Processes.Process_Id;
      Start  : Rose.Invocation.Parameter_Word_Index);

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
   begin
      case Cap.Header.Endpoint is
         when Map_Page =>
            declare
               use Rose.Invocation;
               Pid         : constant Rose.Kernel.Processes.Process_Id :=
                               Rose.Kernel.Processes.To_Process_Id
                                 (Rose.Invocation.Get_Object_Id
                                    (Params.all, 0));
               Start_Index : constant Parameter_Word_Index :=
                               (if Object_Fits_In_Word
                                then 1 else 2);
            begin
               if Pid = Rose.Kernel.Processes.Null_Process_Id then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Invalid_Object);
               else
                  Map_Process_Page (Params, Pid, Start_Index);
               end if;
            end;

         when Unmap_Page =>

            --  Data: the process's object id and the virtual page, which
            --  the memory server no longer maps (ADR 0025). A page of a
            --  buffer that the process has lent to a server in its call in
            --  progress is still mapped in the server's shared slot, so it
            --  is refused with Request_Would_Block, and stays mapped
            --  (#972).

            declare
               use Rose.Invocation;
               Pid         : constant Rose.Kernel.Processes.Process_Id :=
                               Rose.Kernel.Processes.To_Process_Id
                                 (Rose.Invocation.Get_Object_Id
                                    (Params.all, 0));
               Page_Index  : constant Parameter_Word_Index :=
                               (if Object_Fits_In_Word
                                then 1 else 2);
               Page        : constant Rose.Addresses.Virtual_Page_Address :=
                               Rose.Addresses.Virtual_Page_Address
                                 (Params.Data (Page_Index));
            begin
               if Pid = Rose.Kernel.Processes.Null_Process_Id then
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Invalid_Object);
               elsif Rose.Kernel.Processes.Is_Pinned_Page (Pid, Page) then

                  --  Said here, not by mem, which writes nothing to the
                  --  console while it pages out (#973).

                  Rose.Kernel.Processes.Debug.Put (Pid);
                  Rose.Boot.Console.Put (": unmap refused: page ");
                  Rose.Boot.Console.Put (Rose.Words.Word (Page));
                  Rose.Boot.Console.Put (" pinned for a call");
                  Rose.Boot.Console.New_Line;
                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Request_Would_Block);
               elsif Rose.Kernel.Checkpoint_Copies.Copying
                 and then Rose.Kernel.Checkpoint_Copies.Is_Written
                   (Rose.Invocation.Get_Object_Id (Params.all, 0), Page)
               then

                  --  The process wrote the page during the checkpoint,
                  --  but mem's tables say it is clean until mem has the
                  --  list of written pages: dropping it would lose the
                  --  write (ADR 0030, section 5).

                  Rose.Kernel.Processes.Return_Error
                    (Params, Rose.Invocation.Request_Would_Block);
               else
                  Rose.Kernel.Processes.Unmap_Page
                    (Pid          => Pid,
                     Virtual_Page => Page);
                  Params.Control :=
                    (Flags  => (Reply => True, others => False),
                     others => <>);
               end if;
               Rose.Kernel.Processes.Set_Current_State
                 (Rose.Kernel.Processes.Current_Process_Id,
                  Rose.Kernel.Processes.Ready);
            end;

         when Set_Page =>
            declare
               use type Rose.Invocation.Parameter_Word_Index;
               Pid : constant Rose.Kernel.Processes.Process_Id :=
                       Rose.Kernel.Processes.Current_Process_Id;
            begin
               if Params.Control.Last_Sent_Word = 0 then
                  Rose.Kernel.Processes.Unmap_Page
                    (Pid          => Pid,
                     Virtual_Page =>
                       Rose.Addresses.Virtual_Page_Address
                         (Params.Data (0)));
                  Rose.Kernel.Processes.Set_Current_State
                    (Pid,
                     Rose.Kernel.Processes.Ready);
               else
                  Map_Process_Page (Params, Pid, 0);
               end if;
            end;

         when others =>
            Rose.Boot.Console.Put
              ("kernel: page table: unknown endpoint: ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;
      end case;

   end Handle;

   ----------------------
   -- Map_Process_Page --
   ----------------------

   procedure Map_Process_Page
     (Params : Rose.Invocation.Invocation_Access;
      Pid    : Rose.Kernel.Processes.Process_Id;
      Start  : Rose.Invocation.Parameter_Word_Index)
   is
      use type Rose.Words.Word;
      use Rose.Invocation;

      Page_Index      : constant Parameter_Word_Index := Start;
      Physical_Page   : constant Physical_Page_Address :=
                          Rose.Addresses.Physical_Page_Address
                            (Params.Data (Page_Index + 0));
      Virtual_Word    : constant Rose.Words.Word :=
                          Params.Data (Page_Index + 1);
      --  The virtual page, with the permissions in its top three bits
      --  (#985). Mem's Map (Map_Page) and Load_Page (Set_Page) send
      --  them so; only Map sends the statistics, so they are read only
      --  when they were sent (#892).
      Has_Statistics  : constant Boolean :=
                          Sent_Word_Count (Params.all)
                          >= Natural (Page_Index) + 6;
      Virtual_Page    : constant Virtual_Page_Address :=
                          Virtual_Page_Address
                            (Virtual_Word mod 2 ** 20);
      Permissions     : constant Rose.Words.Word :=
                          Virtual_Word / 2 ** 29;
      Available_Pages : constant Rose.Words.Word :=
                          (if Has_Statistics
                           then Params.Data (Page_Index + 2) else 0);
      Allocated_Pages : constant Rose.Words.Word :=
                          (if Has_Statistics
                           then Params.Data (Page_Index + 3) else 0);
      Dirty_Pages     : constant Rose.Words.Word :=
                          (if Has_Statistics
                           then Params.Data (Page_Index + 4) else 0);
      Store_Reads     : constant Rose.Words.Word :=
                          (if Has_Statistics
                           then Params.Data (Page_Index + 5) else 0);
      Readable        : constant Boolean :=
                          (Permissions and 1) /= 0;
      Writable        : constant Boolean :=
                          (Permissions and 2) /= 0;
      Executable      : constant Boolean :=
                          (Permissions and 4) /= 0;
      Write_Tracked   : constant Boolean :=
                          (Virtual_Word / 2 ** 28) mod 2 = 1;
      --  Map: a clean page of a writable segment, which the kernel makes
      --  writable itself during a checkpoint (ADR 0030, section 3).
      Fill            : constant Boolean :=
                          (Virtual_Word / 2 ** 27) mod 2 = 1;
      --  Load_Page: mem is about to fill the frame with new contents, so
      --  it is no longer in a checkpoint that is running (ADR 0030,
      --  section 1).

   begin
      if Fill then
         Rose.Kernel.Checkpoint_Copies.Mark_Fresh (Physical_Page);
      end if;

      --  A frame that a checkpoint still has to save is copied before a
      --  process can write it through a new mapping (ADR 0030, section
      --  2). Mem's own mappings (Load_Page) are not: it reads and fills
      --  frames through them, and is never saved.

      if Writable
        and then Pid /= Rose.Kernel.Processes.Current_Process_Id
        and then Rose.Kernel.Checkpoint_Copies.Copying
      then
         declare
            Ready : Boolean;
         begin
            Rose.Kernel.Checkpoint_Copies.Prepare_Write
              (Physical_Page, Ready);
            if not Ready then
               Rose.Kernel.Checkpoint_Copies.Report_Full ("copy reserve");
            end if;
         end;
      end if;

      Rose.Kernel.Processes.Map_Page
        (Pid           => Pid,
         Virtual_Page  => Virtual_Page,
         Physical_Page => Physical_Page,
         Readable      => Readable,
         Writable      => Writable,
         Executable    => Executable,
         User          => True,
         Managed       => True,
         Write_Tracked => Write_Tracked);

      if Available_Pages > 0 then
         Rose.Kernel.Clock.Update_Mem
           (Allocated =>
              Physical_Bytes (Allocated_Pages * Rose.Limits.Page_Size),
            Available =>
              Physical_Bytes (Available_Pages * Rose.Limits.Page_Size),
            Dirty_Pages => Natural (Dirty_Pages),
            Store_Reads => Natural (Store_Reads));
      end if;

      Params.Control :=
        (Flags  => (Rose.Invocation.Reply => True, others => False),
         others => <>);

      Rose.Kernel.Processes.Set_Current_State
        (Rose.Kernel.Processes.Current_Process_Id,
         Rose.Kernel.Processes.Ready);

   end Map_Process_Page;

end Rose.Kernel.Capabilities.Page_Table;
