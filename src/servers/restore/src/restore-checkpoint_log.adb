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

   function Check_Entry
     (Reader   : Log_Reader_Client;
      Location : Rose.Words.Word_64)
      return Boolean;
   --  Check the entry at Location, and report it if it is inconsistent.

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
      Header_Pages : Natural := 0;
      Listed       : Natural := 0;
   begin
      loop
         if Header_Pages = Pages then
            Report_Bad_Entry (Location, "header pages run past the entry");
            return False;
         end if;

         Read_Page (Reader, Location, Header_Pages + 1, Page);

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

      if Header_Pages + Listed /= Pages then
         Report_Bad_Entry (Location, "page count does not match its headers");
         return False;
      end if;

      return True;
   end Check_Entry;

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
