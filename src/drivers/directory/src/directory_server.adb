with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with Rose.Core.Directory_Client;
with Rose.Core.File_Client;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_Directory.Server;
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Client;

package body Directory_Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;
   use type Rose.Words.Word_32;

   package Directory_Clients renames
     Rose.Interfaces.Writable_Directory.Client;
   package File_Clients renames Rose.Interfaces.Writable_File.Client;

   subtype Writable_Directory_Class is
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class;

   Format : constant Rose.Words.Word_32 := 1;
   --  The version of this server's part of the state (#831):
   --
   --    the directory's identifier
   --    the number of entries, then for each, in the directory's order:
   --      the length of its name, then its characters
   --      its kind, a File_Kind
   --
   --  and, for entry I, counting from 1, the caps that it names:
   --
   --    Entry.I.Served     its Writable_File or Writable_Directory
   --    Entry.I.Control    its Replaceable
   --    Entry.I.Bank       its bank
   --    Entry.I.Read_Only  its File or Directory, if one was given out
   --
   --  The launch caps and Cap_Set_Service are its command's arguments,
   --  which Rose.Replacement hands over (#901).

   Max_Name_Length : constant := 64;
   --  The longest name, which petal and the commands share.

   type Entry_Record is
      record
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Kind      : Rose.Interfaces.Directory.File_Kind;
         File      : File_Clients.Writable_File_Client;
         Child     : Directory_Clients.Writable_Directory_Client;
         --  The entry's writable interface: File for a file, and Child
         --  for a directory.
         Served    : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The cap of the writable interface, which a version that took
         --  over opens File or Child with when it is created.
         Control   : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The entry's Replaceable interface, which only the directory
         --  holds, for a new version of the file or directory (#867,
         --  #831).
         Bank_Ref  : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Bank      : Rose.Interfaces.Bank.Client.Bank_Client;
         --  The entry's own bank, a sub-bank of the directory's, which
         --  its process was launched against, and its cap. Its Destroy
         --  ends the process, and any version that replaced it, when the
         --  entry is deleted.
         Read_Only : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only interface of the file or directory, made the
         --  first time it is asked for.
         Kept      : Boolean := False;
         --  Read_Only came from the state of the version that this one
         --  took over from, so the client that File or Child is didn't
         --  fetch it, and closing it doesn't delete it.
      end record;

   --  The directory is one object, served through Writable_Directory
   --  with the user-mode IDL target (#528).

   type Directory_Object is limited new
     Rose.Interfaces.Writable_Directory.Server.Writable_Directory_Interface
     with null record;

   overriding function Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability;

   overriding function Writable_Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability;

   overriding function Directory_Entry_Count
     (Item : in out Directory_Object)
      return Natural;

   overriding function Directory_Entry_Name
     (Item  : in out Directory_Object;
      Index : Positive)
      return String;

   overriding function Directory_Entry_Kind
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Directory.File_Kind;

   overriding function Directory_Entry_Size
     (Item  : in out Directory_Object;
      Index : Positive)
      return System.Storage_Elements.Storage_Count;

   overriding function Find_Entry
     (Item : in out Directory_Object;
      Name : String)
      return Natural;

   overriding function Get_Ordinary_File
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.File.File_Object'Class;

   overriding function Get_Directory
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Directory.Directory_Object'Class;

   overriding function Get_Writable_File
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Writable_File.Writable_File_Object'Class;

   overriding function Get_Writable_Directory
     (Item  : in out Directory_Object;
      Index : Positive)
      return Writable_Directory_Class;

   overriding function Create_File
     (Item : in out Directory_Object;
      Name : String)
      return Rose.Interfaces.Writable_File.Writable_File_Object'Class;

   overriding function Create_Directory
     (Item : in out Directory_Object;
      Name : String)
      return Writable_Directory_Class;

   overriding procedure Rename
     (Item : in out Directory_Object;
      From : String;
      To   : String);

   overriding procedure Delete_File
     (Item : in out Directory_Object;
      Name : String);

   overriding procedure Delete_Directory
     (Item : in out Directory_Object;
      Name : String);

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Storage arrays, and strings in requests, go through the invoke
   --  buffer; without one, Send_Storage_Array clears the cap of the
   --  invocation.

   Context       : aliased Rose.Server.Server_Context;
   The_Directory : aliased Directory_Object;
   Directory_Id  : Rose.Objects.Capability_Identifier := 0;
   --  The identifier that the directory had in the version that this one
   --  took over from, or 0.

   type Entry_Access is access Entry_Record;

   procedure Free is
     new Ada.Unchecked_Deallocation (Entry_Record, Entry_Access);

   package Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Entry_Access);

   Entries : Entry_Vectors.Vector;
   --  The entries, in the directory's order. An entry holds clients,
   --  which can't be copied, so each is on the heap, and deleting an
   --  entry moves the ones after it down without copying them (#346).
   --  There is no limit on their number (#490).

   function Entry_Count return Natural
   is (Natural (Entries.Length));

   File_Launch_Cap      : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Directory_Launch_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Cap_Set_Service_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   --  The launch interfaces of the file and directory commands, and the
   --  cap set server's Constructor, which each entry is given.

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability;
   --  Launch a file or directory, add it as Name, and return its
   --  writable interface; or return the existing entry Name if it has
   --  the same kind. Null if Name is another kind, or too long, or the
   --  launch fails.

   function Cap_Name
     (Index : Positive;
      Part  : String)
      return String;
   --  The name of the cap Part of entry Index in the state.

   function Find (Name : String) return Natural;
   --  The index of the entry Name, or 0 if there is none.

   function Insert_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Boolean;
   --  Launch a file or directory, and add it as the entry Name. False,
   --  with nothing added, if the launch fails.

   procedure Launch_Entry (Item : in out Entry_Record);
   --  Make a bank for a new entry, and launch the file or directory
   --  command against it, as Item.Kind says. Raises Invocation_Error if
   --  either fails, and then destroys the bank, if there is one.

   function Is_Kind
     (Index : Positive;
      Kind  : Rose.Interfaces.Directory.File_Kind)
      return Boolean
   is (Index <= Entry_Count and then Entries (Index).Kind = Kind);

   procedure Remove_Entry (Index : Positive);
   --  End the process of entry Index, delete the caps for it, and remove
   --  the entry. Destroy raises Invocation_Error if the process can't be
   --  ended, and then nothing changes.

   ---------------
   -- Add_Entry --
   ---------------

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability
   is
      Existing   : constant Natural := Find (Name);
      Saved_Name : String (1 .. Max_Name_Length);
      Saved_Last : Natural;
   begin
      if Existing > 0 then
         if Entries (Existing).Kind /= Kind then
            return Rose.Capabilities.Null_Capability;
         else
            return Entries (Existing).Served;
         end if;
      end if;

      if Name'Length = 0 or else Name'Length > Max_Name_Length then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  Name is in the invoke buffer, which the invocations below can
      --  overwrite.

      Saved_Name (1 .. Name'Length) := Name;
      Saved_Last := Name'Length;

      if not Insert_Entry (Saved_Name (1 .. Saved_Last), Kind) then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Entries.Last_Element.Served;
   end Add_Entry;

   --------------
   -- Cap_Name --
   --------------

   function Cap_Name
     (Index : Positive;
      Part  : String)
      return String
   is ("Entry."
       & Ada.Strings.Fixed.Trim (Positive'Image (Index), Ada.Strings.Left)
       & "." & Part);

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding function Create_Directory
     (Item : in out Directory_Object;
      Name : String)
      return Writable_Directory_Class
   is
      pragma Unreferenced (Item);
   begin
      return Directory_Clients.From_Cap
        (Add_Entry (Name, Rose.Interfaces.Directory.Directory));
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   overriding function Create_File
     (Item : in out Directory_Object;
      Name : String)
      return Rose.Interfaces.Writable_File.Writable_File_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return File_Clients.From_Cap
        (Add_Entry (Name, Rose.Interfaces.Directory.Ordinary_File));
   end Create_File;

   -------------------
   -- Create_Server --
   -------------------

   --  A version that took over opens the clients of the entries that the
   --  state had here, and not in Read_State, since opening a client
   --  calls its server.

   procedure Create_Server
     (File_Launch      : Rose.Capabilities.Capability;
      Directory_Launch : Rose.Capabilities.Capability;
      Cap_Set_Service  : Rose.Capabilities.Capability;
      Served           : out Rose.Capabilities.Capability;
      Control          : out Rose.Capabilities.Capability)
   is
   begin
      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      File_Launch_Cap := File_Launch;
      Directory_Launch_Cap := Directory_Launch;
      Cap_Set_Service_Cap := Cap_Set_Service;

      for Item of Entries loop
         Item.Bank.Open (Item.Bank_Ref);
         if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
            Item.File.Open (Item.Served);
         else
            Item.Child.Open (Item.Served);
         end if;
      end loop;

      Rose.Interfaces.Writable_Directory.Server.Attach
        (Context, The_Directory'Access, Directory_Id);
      Rose.Replacement.Attach
        (Context'Access, Write_State'Access, Cap_Set_Service);

      Served := The_Directory.Writable_Directory_Cap;
      Control := Rose.Replacement.Replaceable_Cap;
   end Create_Server;

   ----------------------
   -- Delete_Directory --
   ----------------------

   overriding procedure Delete_Directory
     (Item : in out Directory_Object;
      Name : String)
   is
      pragma Unreferenced (Item);
      Index : constant Natural := Find (Name);
   begin
      if Index = 0 then
         raise Constraint_Error with "delete: no such entry";
      elsif Entries (Index).Kind
        /= Rose.Interfaces.Directory.Directory
      then
         raise Constraint_Error with "delete: not a directory";
      elsif Entries (Index).Child.Directory_Entry_Count > 0 then
         raise Constraint_Error with "delete: the directory has entries";
      end if;

      Remove_Entry (Index);
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   overriding procedure Delete_File
     (Item : in out Directory_Object;
      Name : String)
   is
      pragma Unreferenced (Item);
      Index : constant Natural := Find (Name);
   begin
      if Index = 0 then
         raise Constraint_Error with "delete: no such entry";
      elsif Entries (Index).Kind
        /= Rose.Interfaces.Directory.Ordinary_File
      then
         raise Constraint_Error with "delete: not a file";
      end if;

      Remove_Entry (Index);
   end Delete_File;

   -------------------
   -- Directory_Cap --
   -------------------

   overriding function Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Writable_Directory.Server.Cap_Of
         (Item, Rose.Interfaces.Writable_Directory.Directory_Interface));

   ---------------------------
   -- Directory_Entry_Count --
   ---------------------------

   overriding function Directory_Entry_Count
     (Item : in out Directory_Object)
      return Natural
   is
      pragma Unreferenced (Item);
   begin
      return Entry_Count;
   end Directory_Entry_Count;

   --------------------------
   -- Directory_Entry_Kind --
   --------------------------

   overriding function Directory_Entry_Kind
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Directory.File_Kind
   is
      pragma Unreferenced (Item);
   begin
      if Index > Entry_Count then
         return Rose.Interfaces.Directory.Special_File;
      else
         return Entries (Index).Kind;
      end if;
   end Directory_Entry_Kind;

   --------------------------
   -- Directory_Entry_Name --
   --------------------------

   overriding function Directory_Entry_Name
     (Item  : in out Directory_Object;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Item);
   begin
      if Index > Entry_Count then
         return "";
      else
         declare
            Named : Entry_Record renames Entries (Index).all;
         begin
            return Named.Name (1 .. Named.Name_Last);
         end;
      end if;
   end Directory_Entry_Name;

   --------------------------
   -- Directory_Entry_Size --
   --------------------------

   overriding function Directory_Entry_Size
     (Item  : in out Directory_Object;
      Index : Positive)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Item);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return Entries (Index).File.Size;
      else
         return 0;
      end if;
   end Directory_Entry_Size;

   ----------
   -- Find --
   ----------

   function Find (Name : String) return Natural is
   begin
      for Index in 1 .. Entry_Count loop
         declare
            Item : Entry_Record renames Entries (Index).all;
         begin
            if Item.Name (1 .. Item.Name_Last) = Name then
               return Index;
            end if;
         end;
      end loop;
      return 0;
   end Find;

   ----------------
   -- Find_Entry --
   ----------------

   overriding function Find_Entry
     (Item : in out Directory_Object;
      Name : String)
      return Natural
   is
      pragma Unreferenced (Item);
   begin
      return Find (Name);
   end Find_Entry;

   -------------------
   -- Get_Directory --
   -------------------

   overriding function Get_Directory
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Directory.Directory_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      if not Is_Kind (Index, Rose.Interfaces.Directory.Directory) then
         return Rose.Interfaces.Directory.Client.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;

      declare
         Listed : Entry_Record renames Entries (Index).all;
      begin
         if Listed.Read_Only = Rose.Capabilities.Null_Capability then
            Listed.Read_Only := Listed.Child.Directory_Cap;
         end if;
         return Rose.Interfaces.Directory.Client.From_Cap (Listed.Read_Only);
      end;
   end Get_Directory;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   overriding function Get_Ordinary_File
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.File.File_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      if not Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return Rose.Interfaces.File.Client.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;

      declare
         Listed : Entry_Record renames Entries (Index).all;
      begin
         if Listed.Read_Only = Rose.Capabilities.Null_Capability then
            Listed.Read_Only := Listed.File.File_Cap;
         end if;
         return Rose.Interfaces.File.Client.From_Cap (Listed.Read_Only);
      end;
   end Get_Ordinary_File;

   ----------------------------
   -- Get_Writable_Directory --
   ----------------------------

   overriding function Get_Writable_Directory
     (Item  : in out Directory_Object;
      Index : Positive)
      return Writable_Directory_Class
   is
      pragma Unreferenced (Item);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Directory) then
         return Directory_Clients.From_Cap (Entries (Index).Served);
      else
         return Directory_Clients.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;
   end Get_Writable_Directory;

   -----------------------
   -- Get_Writable_File --
   -----------------------

   overriding function Get_Writable_File
     (Item  : in out Directory_Object;
      Index : Positive)
      return Rose.Interfaces.Writable_File.Writable_File_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return File_Clients.From_Cap (Entries (Index).Served);
      else
         return File_Clients.From_Cap (Rose.Capabilities.Null_Capability);
      end if;
   end Get_Writable_File;

   ------------------
   -- Insert_Entry --
   ------------------

   function Insert_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Boolean
   is
      New_Entry : Entry_Access := new Entry_Record;
   begin
      New_Entry.Name (1 .. Name'Length) := Name;
      New_Entry.Name_Last := Name'Length;
      New_Entry.Kind := Kind;

      begin
         Launch_Entry (New_Entry.all);
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Free (New_Entry);
            return False;
      end;

      Entries.Append (New_Entry);
      return True;
   end Insert_Entry;

   ------------------
   -- Launch_Entry --
   ------------------

   --  A file is the file command, Rose.Core.File (#867), and a directory
   --  the directory command, Rose.Core.Directory (#831). Each runs in a
   --  bank of its own, so that a new version of it, which is launched
   --  against the same bank, is ended with it too (ADR 0032, section 3).
   --  A subdirectory can create files and directories too.

   procedure Launch_Entry (Item : in out Entry_Record) is
      Is_File  : constant Boolean :=
        Item.Kind = Rose.Interfaces.Directory.Ordinary_File;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Own      : Rose.Interfaces.Bank.Client.Bank_Client;
      Launcher : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint : Rose.Capabilities.Capability;
   begin
      Own.Open (Rose.System_Calls.Own_Bank.Cap);
      declare
         Sub : Rose.Interfaces.Bank.Bank_Object'Class :=
           Own.Create_Sub_Bank (0, 0);
      begin
         Item.Bank_Ref := Sub.Bank_Cap;
         Item.Bank.Open (Item.Bank_Ref);
      end;

      begin
         Launcher.Open
           (if Is_File then File_Launch_Cap else Directory_Launch_Cap);
         Endpoint := Launcher.Launch_Command (Item.Bank.Bank_Cap, No_Caps);
         if Endpoint = Rose.Capabilities.Null_Capability then
            raise Rose.Interfaces.Objects.Invocation_Error
              with "cannot launch an entry";
         end if;

         if Is_File then
            Rose.Core.File_Client.Call
              (Endpoint        => Endpoint,
               Create_Endpoint =>
                 Rose.System_Calls.Create_Endpoint_Capability,
               Delete_Cap      => Rose.System_Calls.Delete_Cap_Capability,
               Cap_Set_Service => Cap_Set_Service_Cap,
               Served          => Item.Served,
               Control         => Item.Control);
            Item.File.Open (Item.Served);
         else
            Rose.Core.Directory_Client.Call
              (Endpoint         => Endpoint,
               Create_Endpoint  =>
                 Rose.System_Calls.Create_Endpoint_Capability,
               Delete_Cap       => Rose.System_Calls.Delete_Cap_Capability,
               Cap_Set_Service  => Cap_Set_Service_Cap,
               File_Launch      => File_Launch_Cap,
               Directory_Launch => Directory_Launch_Cap,
               Served           => Item.Served,
               Control          => Item.Control);
            Item.Child.Open (Item.Served);
         end if;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Item.Bank.Destroy;
            Item.Bank.Close;
            raise;
      end;
   end Launch_Entry;

   ----------------
   -- Read_State --
   ----------------

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps)
   is
      Version : Rose.Words.Word_32;
      Count   : Natural;
   begin
      Rose.Words.Word_32'Read (Stream, Version);
      if Version /= Format then
         raise Rose.Replacement.Bad_State with "directory: unknown format";
      end if;

      Rose.Objects.Capability_Identifier'Read (Stream, Directory_Id);
      Natural'Read (Stream, Count);

      for Index in 1 .. Count loop
         declare
            Item   : constant Entry_Access := new Entry_Record;
            Length : Natural;
         begin
            Natural'Read (Stream, Length);
            if Length = 0 or else Length > Max_Name_Length then
               raise Rose.Replacement.Bad_State with "directory: bad name";
            end if;
            for I in 1 .. Length loop
               Character'Read (Stream, Item.Name (I));
            end loop;
            Item.Name_Last := Length;
            Rose.Interfaces.Directory.File_Kind'Read (Stream, Item.Kind);

            Item.Served := Named.Cap (Cap_Name (Index, "Served"));
            Item.Control := Named.Cap (Cap_Name (Index, "Control"));
            Item.Bank_Ref := Named.Cap (Cap_Name (Index, "Bank"));
            Item.Read_Only := Named.Cap (Cap_Name (Index, "Read_Only"));
            Item.Kept := Item.Read_Only /= Rose.Capabilities.Null_Capability;

            if Item.Served = Rose.Capabilities.Null_Capability
              or else Item.Control = Rose.Capabilities.Null_Capability
              or else Item.Bank_Ref = Rose.Capabilities.Null_Capability
            then
               raise Rose.Replacement.Bad_State
                 with "directory: no caps for " & Item.Name (1 .. Length);
            end if;

            Entries.Append (Item);
         end;
      end loop;
   end Read_State;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry (Index : Positive) is
      Removed : Entry_Access := Entries (Index);
      Item    : Entry_Record renames Removed.all;
   begin
      Item.Bank.Destroy;

      --  Closing the clients deletes their caps, including the read-only
      --  interface if the client fetched it, which it didn't if it came
      --  from an old version's state.

      Item.Bank.Close;
      if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
         Item.File.Close;
      else
         Item.Child.Close;
      end if;

      if Item.Kept then
         Rose.System_Calls.Delete_Cap (Item.Read_Only);
      end if;
      if Item.Control /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Delete_Cap (Item.Control);
      end if;

      Entries.Delete (Index);
      Free (Removed);
   end Remove_Entry;

   ------------
   -- Rename --
   ------------

   --  Only the entry's name changes, so the file or directory process is
   --  the same, and the caps that clients hold to it keep working (#425).
   --  A failure raises, which the generated server sends back as an
   --  error.

   overriding procedure Rename
     (Item : in out Directory_Object;
      From : String;
      To   : String)
   is
      pragma Unreferenced (Item);
      Index : constant Natural := Find (From);
   begin
      if Index = 0 then
         raise Constraint_Error with "rename: no such entry";
      elsif To'Length = 0 or else To'Length > Max_Name_Length then
         raise Constraint_Error with "rename: bad name";
      elsif Find (To) /= 0 then
         raise Constraint_Error with "rename: the name is in use";
      end if;

      Entries (Index).Name (1 .. To'Length) := To;
      Entries (Index).Name_Last := To'Length;
   end Rename;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   ----------------------------
   -- Writable_Directory_Cap --
   ----------------------------

   overriding function Writable_Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Writable_Directory.Server.Cap_Of (Item));

   -----------------
   -- Write_State --
   -----------------

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps)
   is
   begin
      Rose.Words.Word_32'Write (Stream, Format);
      Rose.Objects.Capability_Identifier'Write
        (Stream,
         Rose.Interfaces.Writable_Directory.Server.Identifier_Of
           (The_Directory'Access));
      Natural'Write (Stream, Entry_Count);

      for Index in 1 .. Entry_Count loop
         declare
            Item : Entry_Record renames Entries (Index).all;
         begin
            Natural'Write (Stream, Item.Name_Last);
            for I in 1 .. Item.Name_Last loop
               Character'Write (Stream, Item.Name (I));
            end loop;
            Rose.Interfaces.Directory.File_Kind'Write (Stream, Item.Kind);

            Named.Add (Cap_Name (Index, "Served"), Item.Served);
            Named.Add (Cap_Name (Index, "Control"), Item.Control);
            Named.Add (Cap_Name (Index, "Bank"), Item.Bank_Ref);
            if Item.Read_Only /= Rose.Capabilities.Null_Capability then
               Named.Add (Cap_Name (Index, "Read_Only"), Item.Read_Only);
            end if;
         end;
      end loop;
   end Write_State;

end Directory_Server;
