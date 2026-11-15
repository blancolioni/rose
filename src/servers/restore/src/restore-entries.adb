with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Limits;

package body Restore.Entries is

   use Rose.Interfaces.Checkpoints.Log_Reader.Client;
   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;
   use type Rose.Checkpoints.Header_Page_Flags;

   Page   : System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;

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

   function Read
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Index    : Positive)
      return Boolean;
   --  Read page Index of the entry at Location into Page.

   -----------------
   -- Find_Layout --
   -----------------

   procedure Find_Layout
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Layout   : out Entry_Layout;
      Status   : out Layout_Status)
   is
      Index   : Positive;
      Batches : Natural := 0;
   begin
      Layout := (others => <>);
      Layout.Page_Count := Page_Count (Reader, Location);

      if Layout.Page_Count = 0
        or else not Read (Reader, Location, 1)
        or else Kernel_Header.Magic /= Rose.Checkpoints.Kernel_Table_Magic
      then
         Status := No_Kernel_Tables;
         return;
      end if;

      Layout.Kernel_Pages := Natural (Kernel_Header.Page_Count);

      if Layout.Kernel_Pages = 0
        or else Layout.Kernel_Pages >= Layout.Page_Count
        or else not Read (Reader, Location, Layout.Kernel_Pages + 1)
        or else Store_Header.Magic /= Rose.Checkpoints.Store_Table_Magic
      then
         Status := No_Store_Tables;
         return;
      end if;

      Layout.Store_Pages := Natural (Store_Header.Page_Count);

      if Layout.Store_Pages = 0
        or else Layout.Kernel_Pages + Layout.Store_Pages >= Layout.Page_Count
      then
         Status := No_Store_Tables;
         return;
      end if;

      Layout.Batch_First := Layout.Kernel_Pages + Layout.Store_Pages + 1;

      --  The batches and index sections, up to the memory server's
      --  tables.

      Index := Layout.Batch_First;
      loop
         if Index > Layout.Page_Count then
            Status := No_Memory_Tables;
            return;
         end if;

         if not Read (Reader, Location, Index) then
            Status := Bad_Section;
            return;
         end if;

         exit when Mem_Header.Magic = Rose.Checkpoints.Mem_Table_Magic;

         declare
            Kind : Section_Kind;
            Size : Natural;
         begin
            Section_Size
              (Reader, Location, Index, Layout.Page_Count, Kind, Size,
               Status);
            if Status /= Found then
               return;
            end if;
            if Kind = Memory_Batch then
               Batches := Batches + 1;
            end if;
            Index := Index + Size;
         end;
      end loop;

      Layout.Mem_First := Index;
      Layout.Mem_Pages := Natural (Mem_Header.Page_Count);

      if Mem_Header.Version /= Rose.Checkpoints.Mem_Table_Version then
         Status := Other_Version;
         return;
      end if;

      if Batches = 0 then
         Status := No_Batch;
         return;
      end if;

      if Layout.Mem_Pages = 0
        or else Layout.Mem_First + Layout.Mem_Pages - 1 > Layout.Page_Count
      then
         Status := Runs_Past_Entry;
         return;
      end if;

      Layout.Has_Shared :=
        (Mem_Header.Flags and Rose.Checkpoints.Mem_Has_Shared_Pages) /= 0;
      Layout.Shared_First := Layout.Mem_First + Layout.Mem_Pages;

      if Layout.Has_Shared
        and then Layout.Shared_First > Layout.Page_Count
      then
         Status := Runs_Past_Entry;
         return;
      end if;

      Status := Found;
   end Find_Layout;

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

   ------------------
   -- Section_Size --
   ------------------

   procedure Section_Size
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      First    : Positive;
      Last     : Natural;
      Kind     : out Section_Kind;
      Size     : out Natural;
      Status   : out Layout_Status)
   is
      Header_Pages : Natural := 0;
      Listed       : Natural := 0;
      Is_Last      : Boolean;
   begin
      Kind := Memory_Batch;
      Size := 0;

      loop
         if First + Header_Pages > Last then
            Status := Runs_Past_Entry;
            return;
         end if;

         if not Read (Reader, Location, First + Header_Pages) then
            Status := Bad_Section;
            return;
         end if;

         if Header_Pages = 0 then
            if Header.Magic = Rose.Checkpoints.Header_Page_Magic then
               Kind := Memory_Batch;
            elsif Index_Header.Magic = Rose.Checkpoints.Index_Header_Magic
            then
               Kind := Index_Section;
            else
               Status := Bad_Section;
               return;
            end if;
         end if;

         --  Every header page of a section is of the same kind.

         case Kind is
            when Memory_Batch =>
               if Header.Magic /= Rose.Checkpoints.Header_Page_Magic
                 or else Header.Count > Rose.Checkpoints.Max_Page_References
               then
                  Status := Bad_Section;
                  return;
               end if;
               Listed := Listed + Natural (Header.Count);
               Is_Last :=
                 (Header.Flags and Rose.Checkpoints.Is_Last_Header_Page)
                 /= 0;

            when Index_Section =>
               if Index_Header.Magic /= Rose.Checkpoints.Index_Header_Magic
                 or else Index_Header.Count
                   > Rose.Checkpoints.Max_Index_References
               then
                  Status := Bad_Section;
                  return;
               end if;
               Listed := Listed + Natural (Index_Header.Count);
               Is_Last :=
                 (Index_Header.Flags and Rose.Checkpoints.Is_Last_Header_Page)
                 /= 0;
         end case;

         Header_Pages := Header_Pages + 1;
         exit when Is_Last;
      end loop;

      if First + Header_Pages + Listed - 1 > Last then
         Status := Runs_Past_Entry;
         return;
      end if;

      Size := Header_Pages + Listed;
      Status := Found;
   end Section_Size;

end Restore.Entries;
