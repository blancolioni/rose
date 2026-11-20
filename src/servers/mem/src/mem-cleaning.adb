with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Limits;
with Rose.Words;

with Mem.Log_Index;
with Mem.Page_Log;
with Mem.Shared_Pages;
with Mem.Virtual_Map;

package body Mem.Cleaning is

   use Rose.Words;
   use type Rose.Checkpoints.Header_Page_Flags;

   Page : System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   --  The entry page read last, which is Loaded.

   Kernel_Header : Rose.Checkpoints.Kernel_Table_Header
     with Import, Address => Page'Address;
   Store_Header  : Rose.Checkpoints.Store_Table_Header
     with Import, Address => Page'Address;
   Mem_Header    : Rose.Checkpoints.Mem_Table_Header
     with Import, Address => Page'Address;
   Header        : Rose.Checkpoints.Header_Page
     with Import, Address => Page'Address;
   Index_Header  : Rose.Checkpoints.Index_Header_Page
     with Import, Address => Page'Address;

   Loaded : Natural := 0;
   --  The index in the entry of the page in Page, or 0.

   Log_Last : Word_32 := 0;
   --  The log's last page, after which it wraps round to log page 1.

   --  The cursor: the entry being walked, and the part of it reached. The
   --  pages of an entry are numbered from 1, after its header.

   Entry_Header : Word_32 := 0;
   --  The log page of the entry's header, or 0 if no entry is being
   --  walked.
   Entry_Pages  : Natural := 0;
   Next_Part    : Positive := 1;
   --  The page where the next part of the entry starts.
   After_Store  : Boolean := False;
   --  The kernel's and the store's tables have been passed.

   In_Section   : Boolean := False;
   Is_Index     : Boolean := False;
   --  A batch or an index section is being walked, from Next_Part.
   Header_Pages : Natural := 0;
   Listed       : Natural := 0;
   Done         : Natural := 0;
   --  Its header pages, the pages they list, and how many of those have
   --  been dealt with.

   Free_Target : Word_32 := 0;
   --  The header of the first entry that hasn't been walked, if the ones
   --  before it can be freed, or 0.
   Free_Asked  : Word_32 := 0;
   --  The entry that the last checkpoint asked the log to start at.

   Broken : Boolean := False;
   --  An entry didn't have the parts it should, so cleaning has stopped
   --  until mem starts again: nothing is freed that may still be needed.

   function Distance (From, To : Word_32) return Word_32
   is (if To >= From then To - From else Log_Last - From + To);
   --  How many log pages To is after From, going forwards round the log.

   function Log_Page_Of (Index : Positive) return Word_32
   is (if Word_32 (Index) <= Log_Last - Entry_Header
       then Entry_Header + Word_32 (Index)
       else Word_32 (Index) - (Log_Last - Entry_Header));
   --  The log page of page Index of the entry.

   function Load (Index : Positive) return Boolean;
   --  Read page Index of the entry into Page. False if it is past the
   --  entry's end.

   procedure Reset;
   --  Start walking again from the first entry.

   procedure Start_Entry (Header_Page : Word_32);
   --  Start walking the entry whose header is at Header_Page.

   procedure Start_Section;
   --  Find the size of the batch or index section that starts at
   --  Next_Part. Sets Broken if it isn't one.

   -----------
   -- Clean --
   -----------

   procedure Clean is
      Reads  : Natural := 0;
      Copies : Natural := 0;
      --  How many entry pages this round has read, and copied.

      First, Open : Word_32;

      Batch : Mem.Virtual_Map.Copied_Pages
        (1 .. Rose.Limits.Page_Out_Batch_Pages);
      Count : Natural := 0;
      Scan  : Natural;
      --  The live pages of the batch being walked that are still to be
      --  copied, and how far the walk has looked.

      function Flush return Boolean;
      --  Copy the pages of Batch forward. True if they were, or if there
      --  were none, and then the pages up to Scan are done.

      function Over_Limit return Boolean
      is (Reads >= Rose.Limits.Clean_Read_Limit
          or else Copies >= Rose.Limits.Clean_Copy_Limit
          or else not Mem.Virtual_Map.Can_Copy);
      --  The round has done enough, or copies would leave too little room
      --  in the table of changes for paging out.

      -----------
      -- Flush --
      -----------

      function Flush return Boolean is
         Success : Boolean;
      begin
         if Count > 0 then
            Mem.Virtual_Map.Copy_Forward (Batch (1 .. Count), Success);
            if not Success then
               return False;
            end if;
            Copies := Copies + Count;
            Count := 0;
         end if;
         Done := Scan;
         return True;
      end Flush;

   begin
      if Broken
        or else not Mem.Page_Log.Is_Available
        or else not Mem.Virtual_Map.Can_Copy
      then
         return;
      end if;

      Mem.Page_Log.Get_Space (First, Open, Log_Last);

      if Open = 0 or else Log_Last = 0 then
         return;
      end if;

      --  If the last checkpoint's commit didn't move the log's start, a
      --  copy or an index page it was to write may be missing, so the
      --  walk starts again from the first entry, and finds what is still
      --  live there.

      if Free_Asked /= 0 then
         if First /= Free_Asked then
            Reset;
         end if;
         Free_Asked := 0;
      end if;

      if Free_Target = First then
         Free_Target := 0;
      end if;

      if Entry_Header /= 0
        and then Distance (First, Entry_Header) > Distance (First, Open)
      then
         Reset;
      end if;

      if Entry_Header = 0 then
         if First = Open then
            return;
         end if;
         Start_Entry (First);
      end if;

      while not Broken
        and then Entry_Header /= Open
        and then not Over_Limit
      loop

         --  Cleaning stops once the log, without the entries walked
         --  already, is below the fill level.

         exit when Distance
           ((if Free_Target = 0 then First else Free_Target), Open) * 100
           < Rose.Limits.Clean_Start_Percent * Log_Last;

         if In_Section then
            Scan := Done;
            while Scan < Listed loop
               declare
                  Per_Page  : constant Positive :=
                                (if Is_Index
                                 then Rose.Checkpoints.Max_Index_References
                                 else Rose.Checkpoints.Max_Page_References);
                  Lister    : constant Positive := Next_Part + Scan / Per_Page;
                  --  The header page that lists the page at Scan.
                  Reference : constant Word_32 :=
                                Word_32 (Scan mod Per_Page) + 1;
                  Log_Page  : constant Word_32 :=
                                Log_Page_Of
                                  (Next_Part + Header_Pages + Scan);
               begin
                  if Loaded /= Lister then
                     Reads := Reads + 1;
                     if not Load (Lister) then
                        Broken := True;
                        return;
                     end if;
                  end if;

                  if Is_Index then
                     declare
                        Item    : constant Rose.Checkpoints.Index_Reference
                          := Index_Header.Pages (Reference);
                        Success : Boolean;
                     begin
                        if Mem.Log_Index.Is_Live_Index_Page
                          (Process_Id (Item.Process), Item.Slot, Log_Page)
                        then
                           Mem.Log_Index.Rewrite
                             (Process_Id (Item.Process), Item.Slot, Success);
                           exit when not Success;
                        end if;
                     end;
                     Scan := Scan + 1;
                     Done := Scan;
                  else
                     declare
                        Item : constant Rose.Checkpoints.Page_Reference :=
                                 Header.Pages (Reference);
                     begin
                        if Mem.Log_Index.Find
                          (Process_Id (Item.Process), Item.Page) = Log_Page
                        then
                           Count := Count + 1;
                           Batch (Count) :=
                             (Process  => Process_Id (Item.Process),
                              Page     => Item.Page,
                              Log_Page => Log_Page);
                        end if;
                     end;
                     Scan := Scan + 1;
                     if Count = Batch'Last or else Count = 0 then
                        exit when not Flush;
                     end if;
                  end if;
               end;

               exit when Over_Limit;
            end loop;

            if not Is_Index and then not Flush then
               return;
            end if;

            exit when Done < Listed;

            Next_Part := Next_Part + Header_Pages + Listed;
            In_Section := False;

         else
            Reads := Reads + 1;
            if not Load (Next_Part) then
               Broken := True;
            elsif not After_Store
              and then Kernel_Header.Magic
                = Rose.Checkpoints.Kernel_Table_Magic
            then
               --  The kernel's tables, then the store's.

               Next_Part := Next_Part + Natural (Kernel_Header.Page_Count);
               Reads := Reads + 1;
               if Kernel_Header.Page_Count = 0
                 or else not Load (Next_Part)
                 or else Store_Header.Magic
                   /= Rose.Checkpoints.Store_Table_Magic
                 or else Store_Header.Page_Count = 0
               then
                  Broken := True;
               else
                  Next_Part := Next_Part + Natural (Store_Header.Page_Count);
                  After_Store := True;
               end if;
            elsif After_Store
              and then Mem_Header.Magic = Rose.Checkpoints.Mem_Table_Magic
            then

               --  mem's tables and the shared pages: the entry is done,
               --  and nothing before the next one is needed.

               Free_Target :=
                 (if Log_Page_Of (Entry_Pages) = Log_Last then 1
                  else Log_Page_Of (Entry_Pages) + 1);
               if Free_Target = Open then
                  Entry_Header := Open;
               else
                  Start_Entry (Free_Target);
               end if;
            else
               Start_Section;
            end if;
         end if;
      end loop;
   end Clean;

   ------------------
   -- Free_Cleaned --
   ------------------

   procedure Free_Cleaned is
   begin
      if Free_Target /= 0 then
         Mem.Page_Log.Free_Before (Free_Target);
         Free_Asked := Free_Target;
      end if;
   end Free_Cleaned;

   ----------
   -- Load --
   ----------

   function Load (Index : Positive) return Boolean is
   begin
      if Index > Entry_Pages then
         return False;
      end if;
      Mem.Page_Log.Read (Log_Page_Of (Index), Page'Address);
      Loaded := Index;
      return True;
   end Load;

   ------------------------
   -- Prepare_Checkpoint --
   ------------------------

   procedure Prepare_Checkpoint is
   begin
      if Free_Target /= 0 then
         Mem.Shared_Pages.Unsave_Modified;
      end if;
   end Prepare_Checkpoint;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Entry_Header := 0;
      Free_Target := 0;
      In_Section := False;
      Loaded := 0;
   end Reset;

   -----------------
   -- Start_Entry --
   -----------------

   procedure Start_Entry (Header_Page : Word_32) is
      Last : constant Word_32 := Mem.Page_Log.Entry_Last (Header_Page);
   begin
      Entry_Header := Header_Page;
      Next_Part := 1;
      After_Store := False;
      In_Section := False;
      Loaded := 0;

      if Last = 0 then
         Broken := True;
         Entry_Pages := 0;
      else
         Entry_Pages := Natural (Distance (Header_Page, Last));
      end if;
   end Start_Entry;

   -------------------
   -- Start_Section --
   -------------------

   procedure Start_Section is
      Is_Last : Boolean := False;
   begin
      Header_Pages := 0;
      Listed := 0;
      Done := 0;

      if Header.Magic = Rose.Checkpoints.Header_Page_Magic then
         Is_Index := False;
      elsif Index_Header.Magic = Rose.Checkpoints.Index_Header_Magic then
         Is_Index := True;
      else
         Broken := True;
         return;
      end if;

      --  Every header page but the last is full, so the page that lists
      --  the Nth page of the section is found by dividing.

      while not Is_Last loop
         if not Load (Next_Part + Header_Pages) then
            Broken := True;
            return;
         end if;

         if Is_Index then
            if Index_Header.Magic /= Rose.Checkpoints.Index_Header_Magic
              or else Index_Header.Count
                > Rose.Checkpoints.Max_Index_References
            then
               Broken := True;
               return;
            end if;
            Is_Last :=
              (Index_Header.Flags and Rose.Checkpoints.Is_Last_Header_Page)
              /= 0;
            if not Is_Last
              and then Index_Header.Count
                /= Rose.Checkpoints.Max_Index_References
            then
               Broken := True;
               return;
            end if;
            Listed := Listed + Natural (Index_Header.Count);
         else
            if Header.Magic /= Rose.Checkpoints.Header_Page_Magic
              or else Header.Count > Rose.Checkpoints.Max_Page_References
            then
               Broken := True;
               return;
            end if;
            Is_Last :=
              (Header.Flags and Rose.Checkpoints.Is_Last_Header_Page) /= 0;
            if not Is_Last
              and then Header.Count /= Rose.Checkpoints.Max_Page_References
            then
               Broken := True;
               return;
            end if;
            Listed := Listed + Natural (Header.Count);
         end if;

         Header_Pages := Header_Pages + 1;
      end loop;

      if Next_Part + Header_Pages + Listed - 1 > Entry_Pages then
         Broken := True;
         return;
      end if;

      In_Section := True;
   end Start_Section;

end Mem.Cleaning;
