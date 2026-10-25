with System.Storage_Elements;

with Rose.Checkpoints;
with Rose.Console_IO;
with Rose.Limits;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Log_Reader.Client;

package body Restore.Checkpoint_Log is

   use Rose.Interfaces.Log_Reader.Client;
   use type Rose.Words.Word_64;

   Page   : System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size)
     with Alignment => Rose.Limits.Page_Size;
   Header : Rose.Checkpoints.Header_Page
     with Import, Address => Page'Address;
   Kernel_Header : Rose.Checkpoints.Kernel_Table_Header
     with Import, Address => Page'Address;
   Process : Rose.Checkpoints.Process_Record
     with Import, Address => Page'Address;

   function Check_Entry
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64)
      return Boolean;
   --  Check the entry at Location, and report it if it is inconsistent.

   function Check_Kernel_Tables
     (Reader       : Log_Reader_Client;
      Location     : Rose.Words.Word_64;
      Pages        : Natural;
      Kernel_Pages : out Natural)
      return Boolean;
   --  Check the kernel's tables at the start of the entry at Location,
   --  which has Pages pages, and set Kernel_Pages to the number of pages
   --  they take.

   procedure Report_Bad_Entry
     (Location : Rose.Words.Word_64;
      Message  : String);

   -----------------
   -- Check_Entry --
   -----------------

   function Check_Entry
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64)
      return Boolean
   is
      use type Rose.Checkpoints.Header_Page_Flags;
      Pages        : constant Natural := Page_Count (Reader, Location);
      Kernel_Pages : Natural;
      Header_Pages : Natural := 0;
      Listed       : Natural := 0;
   begin
      if not Check_Kernel_Tables (Reader, Location, Pages, Kernel_Pages) then
         return False;
      end if;

      loop
         if Kernel_Pages + Header_Pages = Pages then
            Report_Bad_Entry (Location, "header pages run past the entry");
            return False;
         end if;

         Read_Page (Reader, Location, Kernel_Pages + Header_Pages + 1, Page);

         if Has_Error
           or else Header.Magic /= Rose.Checkpoints.Header_Page_Magic
         then
            Report_Bad_Entry (Location, "expected a checkpoint header page");
            return False;
         end if;

         Header_Pages := Header_Pages + 1;
         Listed := Listed + Natural (Header.Count);

         exit when (Header.Flags and Rose.Checkpoints.Is_Last_Header_Page)
           /= 0;
      end loop;

      if Kernel_Pages + Header_Pages + Listed /= Pages then
         Report_Bad_Entry (Location, "page count does not match its headers");
         return False;
      end if;

      return True;
   end Check_Entry;

   -------------------------
   -- Check_Kernel_Tables --
   -------------------------

   function Check_Kernel_Tables
     (Reader       : Log_Reader_Client;
      Location     : Rose.Words.Word_64;
      Pages        : Natural;
      Kernel_Pages : out Natural)
      return Boolean
   is
      use type Rose.Words.Word_32;
      Process_Count : Natural;
      Index         : Positive := 2;
   begin
      Kernel_Pages := 0;

      if Pages = 0 then
         Report_Bad_Entry (Location, "entry is empty");
         return False;
      end if;

      Read_Page (Reader, Location, 1, Page);

      if Has_Error
        or else Kernel_Header.Magic /= Rose.Checkpoints.Kernel_Table_Magic
      then
         Report_Bad_Entry (Location, "expected the kernel's tables");
         return False;
      end if;

      if Kernel_Header.Version /= Rose.Checkpoints.Kernel_Table_Version then
         Report_Bad_Entry (Location, "unknown kernel tables version");
         return False;
      end if;

      Kernel_Pages := Natural (Kernel_Header.Page_Count);
      Process_Count := Natural (Kernel_Header.Process_Count);

      if Kernel_Pages = 0 or else Kernel_Pages > Pages then
         Report_Bad_Entry (Location, "kernel tables run past the entry");
         return False;
      end if;

      for I in 1 .. Process_Count loop
         if Index > Kernel_Pages then
            Report_Bad_Entry (Location, "process records run past the "
                              & "kernel tables");
            return False;
         end if;

         Read_Page (Reader, Location, Index, Page);

         if Has_Error
           or else Process.Magic /= Rose.Checkpoints.Process_Record_Magic
         then
            Report_Bad_Entry (Location, "expected a process record");
            return False;
         end if;

         if Process.Cap_Count > Process.Last_Cap
           or else Process.Cap_Page_Count
             /= (Process.Cap_Count + Rose.Checkpoints.Caps_Per_Page - 1)
               / Rose.Checkpoints.Caps_Per_Page
         then
            Report_Bad_Entry (Location, "process caps do not fit its pages");
            return False;
         end if;

         Index := Index + 1 + Natural (Process.Cap_Page_Count);
      end loop;

      if Index /= Kernel_Pages + 1 then
         Report_Bad_Entry (Location, "kernel tables page count is wrong");
         return False;
      end if;

      return True;
   end Check_Kernel_Tables;

   ------------
   -- Report --
   ------------

   procedure Report (Reader_Cap : Rose.Capabilities.Capability) is
      Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64;
      Last     : Rose.Words.Word_64 := 0;
      Count    : Natural := 0;
      Bad      : Natural := 0;
   begin
      Open (Reader, Reader_Cap);

      --  Read_Page returns its page in the invocation buffer.  The
      --  installer doesn't expect one, so it is only set while the log
      --  is read.

      Rose.System_Calls.Use_Buffer (Page'Address, Page'Length);

      Location := First_Entry (Reader);
      while Location /= 0 loop
         Count := Count + 1;
         if not Check_Entry (Reader, Location) then
            Bad := Bad + 1;
         end if;
         Last := Location;
         Location := Next_Entry (Reader, Location);
      end loop;

      Rose.Console_IO.Put ("restore: log: ");
      Rose.Console_IO.Put (Count);
      Rose.Console_IO.Put (if Count = 1 then " entry" else " entries");

      if Count > 0 then
         declare
            Last_Pages : constant Natural := Page_Count (Reader, Last);
         begin
            Rose.Console_IO.Put ("; last at ");
            Rose.Console_IO.Put (Natural (Last));
            Rose.Console_IO.Put (" (");
            Rose.Console_IO.Put (Last_Pages);
            Rose.Console_IO.Put
              (if Last_Pages = 1 then " page)" else " pages)");
         end;
         if Bad = 0 then
            Rose.Console_IO.Put ("; all consistent");
         else
            Rose.Console_IO.Put ("; ");
            Rose.Console_IO.Put (Bad);
            Rose.Console_IO.Put (" inconsistent");
         end if;
      end if;
      Rose.Console_IO.New_Line;

      if Last /= Last_Entry (Reader) then
         Rose.Console_IO.Put ("restore: log: last entry should be ");
         Rose.Console_IO.Put (Natural (Last_Entry (Reader)));
         Rose.Console_IO.New_Line;
      end if;

      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);
   end Report;

   ----------------------
   -- Report_Bad_Entry --
   ----------------------

   procedure Report_Bad_Entry
     (Location : Rose.Words.Word_64;
      Message  : String)
   is
   begin
      Rose.Console_IO.Put ("restore: log: entry at ");
      Rose.Console_IO.Put (Natural (Location));
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Put_Line (Message);
   end Report_Bad_Entry;

end Restore.Checkpoint_Log;
