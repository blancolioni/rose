with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Control.Client;
with Rose.Interfaces.File_Version;
with Rose.Interfaces.File_Version.Client;
with Rose.Interfaces.File_Version.Server;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_Directory.Server;
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Client;

package body Directory.Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;

   package Directory_Clients renames
     Rose.Interfaces.Writable_Directory.Client;
   package File_Clients renames Rose.Interfaces.Writable_File.Client;

   subtype Writable_Directory_Class is
     Rose.Interfaces.Writable_Directory.Writable_Directory_Object'Class;
   subtype Writable_File_Class is
     Rose.Interfaces.Writable_File.Writable_File_Object'Class;

   Max_Entries     : constant := 64;
   Max_Name_Length : constant := 64;
   --  User-mode malloc can't grow, so the entries are a fixed table.

   type Entry_Record is
      record
         Used      : Boolean := False;
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Kind      : Rose.Interfaces.Directory.File_Kind;
         File      : File_Clients.Writable_File_Client;
         Control   : Rose.Interfaces.File_Control.Client.File_Control_Client;
         --  A file's File_Control, which only the directory holds (#141).
         Child     : Directory_Clients.Writable_Directory_Client;
         Read_Only : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only interface of the file or directory, made the
         --  first time it is asked for.
         Process   : Rose.Interfaces.Process.Client.Process_Client;
         --  The file or directory's process, which Launch returned. Its
         --  Destroy ends the process when the entry is deleted (#424,
         --  #346).
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

   overriding function New_File_Version
     (Item : in out Directory_Object;
      Name : String)
      return Rose.Interfaces.File_Version.File_Version_Object'Class;

   --  Versions of files (#141, ADR 0012). Each is an object served
   --  through File_Version, which New_File_Version returns to the shell.
   --  The user-mode target attaches objects for good, so there is a
   --  fixed set of them, and one is used again once its version has
   --  been committed or discarded. The shell closes its caps to a
   --  version then.

   Max_Versions : constant := 8;

   type Version_Object is limited new
     Rose.Interfaces.File_Version.Server.File_Version_Interface with
      record
         Used          : Boolean := False;
         Slot          : Natural := 0;
         --  The entry of the file that this is a version of, or 0 for a
         --  new file.
         Gone          : Boolean := False;
         --  The file was deleted while its version was pending.
         Name          : String (1 .. Max_Name_Length);
         Name_Last     : Natural := 0;
         Interface_Cap : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Process_Cap   : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  A new file's name, its File_Control and its process. The
         --  directory launched it, and lists it only when the version
         --  is committed.
         Writable_Cap  : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The version's Writable_File.
      end record;

   overriding function File_Version_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability;

   overriding function Writable
     (Item : in out Version_Object)
      return Writable_File_Class;

   overriding procedure Commit (Item : in out Version_Object);

   overriding procedure Discard (Item : in out Version_Object);

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Storage arrays, and strings in requests, go through the invoke
   --  buffer; without one, Send_Storage_Array clears the cap of the
   --  invocation.

   Context          : Rose.Server.Server_Context;
   The_Directory    : aliased Directory_Object;
   subtype Slot_Index is Positive range 1 .. Max_Entries;

   Entries          : array (Slot_Index) of Entry_Record;
   Order            : array (Slot_Index) of Slot_Index;
   Entry_Count      : Natural := 0;
   --  Entry I, for I up to Entry_Count, is in Entries (Order (I)). An
   --  entry holds clients, which can't be copied, so deleting an entry
   --  moves the slot numbers after it down, and not the entries (#346).
   File_Launch      : Rose.Interfaces.Launch.Client.Launch_Client;
   Directory_Launch : Rose.Interfaces.Launch.Client.Launch_Client;
   Versions         : array (1 .. Max_Versions) of aliased Version_Object;

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability;
   --  Launch a file or directory, add it as Name, and return its
   --  writable interface; or return the existing entry Name if it has
   --  the same kind. Null if Name is another kind, too long, or there
   --  is no room, or the launch fails.

   function Find (Name : String) return Natural;
   --  The index of the entry Name, or 0 if there is none.

   procedure Free (Item : in out Version_Object);
   --  Delete the directory's cap for the version Item, and make Item
   --  free for another.

   procedure Insert_Entry
     (Name          : String;
      Kind          : Rose.Interfaces.Directory.File_Kind;
      Interface_Cap : Rose.Capabilities.Capability;
      Process_Cap   : Rose.Capabilities.Capability);
   --  Add the entry Name for the process Process_Cap, which publishes
   --  Interface_Cap. There must be room.

   function Is_Kind
     (Index : Positive;
      Kind  : Rose.Interfaces.Directory.File_Kind)
      return Boolean
   is (Index <= Entry_Count and then Entries (Order (Index)).Kind = Kind);

   procedure Remove_Entry (Index : Positive);
   --  End the process of entry Index, delete the caps for it, and remove
   --  the entry. Destroy raises Invocation_Error if the process can't be
   --  ended, and then nothing changes.

   function Launch
     (Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Interfaces.Process.Process_Object'Class;
   --  Launch a file or directory process. It raises Invocation_Error if
   --  the launch fails.

   ---------------
   -- Add_Entry --
   ---------------

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability
   is
      Existing      : constant Natural := Find (Name);
      Interface_Cap : Rose.Capabilities.Capability;
      Process_Cap   : Rose.Capabilities.Capability;
      Saved_Name    : String (1 .. Max_Name_Length);
      Saved_Last    : Natural;
   begin
      if Existing > 0 then
         if Entries (Order (Existing)).Kind /= Kind then
            return Rose.Capabilities.Null_Capability;
         elsif Kind = Rose.Interfaces.Directory.Ordinary_File then
            return Entries (Order (Existing)).File.Writable_File_Cap;
         else
            return Entries (Order (Existing)).Child.Writable_Directory_Cap;
         end if;
      end if;

      if Name'Length = 0 or else Name'Length > Max_Name_Length
        or else Entry_Count = Max_Entries
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  Name is in the invoke buffer, which the invocations below can
      --  overwrite.

      Saved_Name (1 .. Name'Length) := Name;
      Saved_Last := Name'Length;

      declare
         Process : Rose.Interfaces.Process.Process_Object'Class :=
           Launch (Kind);
      begin
         Interface_Cap := Process.Published_Interface;
         Process_Cap := Process.Process_Cap;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Rose.Capabilities.Null_Capability;
      end;

      if Interface_Cap = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      Insert_Entry
        (Saved_Name (1 .. Saved_Last), Kind, Interface_Cap, Process_Cap);

      declare
         Added : Entry_Record renames Entries (Order (Entry_Count));
      begin
         if Kind = Rose.Interfaces.Directory.Ordinary_File then
            return Added.File.Writable_File_Cap;
         else
            return Added.Child.Writable_Directory_Cap;
         end if;
      end;
   end Add_Entry;

   ------------
   -- Commit --
   ------------

   overriding procedure Commit (Item : in out Version_Object) is
   begin
      if not Item.Used then
         raise Constraint_Error with "commit: no version";
      elsif Item.Gone then
         raise Constraint_Error with "commit: the file was deleted";
      elsif Item.Slot /= 0 then
         Entries (Item.Slot).Control.Commit_Version;
      else
         declare
            Name : constant String := Item.Name (1 .. Item.Name_Last);
         begin
            if Find (Name) /= 0 then
               raise Constraint_Error with "commit: the name is in use";
            elsif Entry_Count = Max_Entries then
               raise Constraint_Error with "commit: the directory is full";
            end if;

            --  The entry takes over the new file's caps.

            Insert_Entry
              (Name, Rose.Interfaces.Directory.Ordinary_File,
               Item.Interface_Cap, Item.Process_Cap);
         end;
      end if;

      Free (Item);
   end Commit;

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

   procedure Create_Server is
   begin
      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      File_Launch.Open (File_Launch_Cap);
      Directory_Launch.Open (Directory_Launch_Cap);

      Rose.Interfaces.Writable_Directory.Server.Publish
        (Context, The_Directory'Access);

      for Index in Versions'Range loop
         Rose.Interfaces.File_Version.Server.Attach
           (Context, Versions (Index)'Access);
      end loop;
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
      elsif Entries (Order (Index)).Kind
        /= Rose.Interfaces.Directory.Directory
      then
         raise Constraint_Error with "delete: not a directory";
      elsif Entries (Order (Index)).Child.Directory_Entry_Count > 0 then
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
      elsif Entries (Order (Index)).Kind
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
         return Entries (Order (Index)).Kind;
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
            Named : Entry_Record renames Entries (Order (Index));
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
         return Entries (Order (Index)).File.Size;
      else
         return 0;
      end if;
   end Directory_Entry_Size;

   -------------
   -- Discard --
   -------------

   --  A new file's process is ended. A version of a file that has been
   --  deleted has gone with it.

   overriding procedure Discard (Item : in out Version_Object) is
   begin
      if not Item.Used then
         raise Constraint_Error with "discard: no version";
      elsif Item.Gone then
         null;
      elsif Item.Slot /= 0 then
         Entries (Item.Slot).Control.Discard_Version;
      else
         declare
            Process : Rose.Interfaces.Process.Client.Process_Client;
         begin
            Process.Open (Item.Process_Cap);
            Process.Destroy;
            Process.Close;
         end;
         Rose.System_Calls.Delete_Cap (Item.Interface_Cap);
      end if;

      Free (Item);
   end Discard;

   ----------------------
   -- File_Version_Cap --
   ----------------------

   overriding function File_Version_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.File_Version.Server.Cap_Of (Item));

   ----------
   -- Find --
   ----------

   function Find (Name : String) return Natural is
   begin
      for Index in 1 .. Entry_Count loop
         declare
            Item : Entry_Record renames Entries (Order (Index));
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

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Version_Object) is
   begin
      if Item.Writable_Cap /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Delete_Cap (Item.Writable_Cap);
      end if;
      Item.Writable_Cap := Rose.Capabilities.Null_Capability;
      Item.Interface_Cap := Rose.Capabilities.Null_Capability;
      Item.Process_Cap := Rose.Capabilities.Null_Capability;
      Item.Slot := 0;
      Item.Name_Last := 0;
      Item.Gone := False;
      Item.Used := False;
   end Free;

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
         Listed : Entry_Record renames Entries (Order (Index));
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
         Listed : Entry_Record renames Entries (Order (Index));
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
         return Directory_Clients.From_Cap
           (Entries (Order (Index)).Child.Writable_Directory_Cap);
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
         return File_Clients.From_Cap
           (Entries (Order (Index)).File.Writable_File_Cap);
      else
         return File_Clients.From_Cap (Rose.Capabilities.Null_Capability);
      end if;
   end Get_Writable_File;

   ------------------
   -- Insert_Entry --
   ------------------

   --  A file's process publishes its File_Control, which gives the file
   --  itself (#141).

   procedure Insert_Entry
     (Name          : String;
      Kind          : Rose.Interfaces.Directory.File_Kind;
      Interface_Cap : Rose.Capabilities.Capability;
      Process_Cap   : Rose.Capabilities.Capability)
   is
   begin
      --  The first free slot: there is one, since Entry_Count is less
      --  than Max_Entries.

      for Slot in Slot_Index loop
         if not Entries (Slot).Used then
            Entry_Count := Entry_Count + 1;
            Order (Entry_Count) := Slot;
            exit;
         end if;
      end loop;

      declare
         New_Entry : Entry_Record renames Entries (Order (Entry_Count));
      begin
         New_Entry.Used := True;
         New_Entry.Name (1 .. Name'Length) := Name;
         New_Entry.Name_Last := Name'Length;
         New_Entry.Kind := Kind;
         New_Entry.Process.Open (Process_Cap);
         if Kind = Rose.Interfaces.Directory.Ordinary_File then
            New_Entry.Control.Open (Interface_Cap);
            declare
               File : Writable_File_Class := New_Entry.Control.Writable;
            begin
               New_Entry.File.Open (File.Writable_File_Cap);
            end;
         else
            New_Entry.Child.Open (Interface_Cap);
         end if;
      end;
   end Insert_Entry;

   ------------
   -- Launch --
   ------------

   function Launch
     (Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Interfaces.Process.Process_Object'Class
   is
      No_Caps : Rose.Capabilities.Capability_Array (1 .. 0);
   begin
      if Kind = Rose.Interfaces.Directory.Ordinary_File then
         return File_Launch.Launch (No_Caps);
      else
         --  A subdirectory can create files and directories too.
         return Directory_Launch.Launch
           ((File_Launch_Cap, Directory_Launch_Cap));
      end if;
   end Launch;

   ----------------------
   -- New_File_Version --
   ----------------------

   --  A failure raises, which the generated server sends back as an
   --  error.

   overriding function New_File_Version
     (Item : in out Directory_Object;
      Name : String)
      return Rose.Interfaces.File_Version.File_Version_Object'Class
   is
      pragma Unreferenced (Item);
      Index : constant Natural := Find (Name);
      Spare : Natural := 0;
   begin
      for Version in Versions'Range loop
         if not Versions (Version).Used then
            Spare := Version;
            exit;
         end if;
      end loop;

      if Spare = 0 then
         raise Constraint_Error with "version: too many versions";
      elsif Index /= 0
        and then Entries (Order (Index)).Kind
          /= Rose.Interfaces.Directory.Ordinary_File
      then
         raise Constraint_Error with "version: not a file";
      elsif Index = 0
        and then (Name'Length = 0 or else Name'Length > Max_Name_Length)
      then
         raise Constraint_Error with "version: bad name";
      end if;

      declare
         Version : Version_Object renames Versions (Spare);
      begin
         if Index /= 0 then
            declare
               File : Writable_File_Class :=
                 Entries (Order (Index)).Control.New_Version;
            begin
               Version.Writable_Cap := File.Writable_File_Cap;
            end;
            if Version.Writable_Cap = Rose.Capabilities.Null_Capability then
               raise Constraint_Error
                 with "version: the file already has a version";
            end if;
            Version.Slot := Order (Index);
         else

            --  Name is in the invoke buffer, which Launch overwrites
            --  when it sends the environment.

            Version.Name (1 .. Name'Length) := Name;
            Version.Name_Last := Name'Length;

            declare
               Process : Rose.Interfaces.Process.Process_Object'Class :=
                 Launch (Rose.Interfaces.Directory.Ordinary_File);
            begin
               Version.Interface_Cap := Process.Published_Interface;
               Version.Process_Cap := Process.Process_Cap;
            end;

            declare
               Control : Rose.Interfaces.File_Control.Client
                 .File_Control_Client :=
                   Rose.Interfaces.File_Control.Client.From_Cap
                     (Version.Interface_Cap);
               File    : Writable_File_Class := Control.Writable;
            begin
               Version.Writable_Cap := File.Writable_File_Cap;
            end;
            Version.Slot := 0;
         end if;

         Version.Used := True;
         Version.Gone := False;
         return Rose.Interfaces.File_Version.Client.From_Cap
           (Rose.Interfaces.File_Version.Server.Cap_Of (Version));
      end;
   end New_File_Version;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry (Index : Positive) is
      Item : Entry_Record renames Entries (Order (Index));
   begin
      Item.Process.Destroy;

      --  A pending version of the file can no longer be committed.

      for Version of Versions loop
         if Version.Used and then Version.Slot = Order (Index) then
            Version.Gone := True;
         end if;
      end loop;

      --  Closing the clients deletes their caps, including the read-only
      --  interface, which is the one the client fetched.

      Item.Process.Close;
      if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
         Item.File.Close;
         Item.Control.Close;
      else
         Item.Child.Close;
      end if;
      Item.Read_Only := Rose.Capabilities.Null_Capability;
      Item.Name_Last := 0;
      Item.Used := False;

      Order (Index .. Entry_Count - 1) := Order (Index + 1 .. Entry_Count);
      Entry_Count := Entry_Count - 1;
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

      Entries (Order (Index)).Name (1 .. To'Length) := To;
      Entries (Order (Index)).Name_Last := To'Length;
   end Rename;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   --------------
   -- Writable --
   --------------

   overriding function Writable
     (Item : in out Version_Object)
      return Writable_File_Class
   is
   begin
      if not Item.Used then
         raise Constraint_Error with "version: no version";
      end if;
      return File_Clients.From_Cap (Item.Writable_Cap);
   end Writable;

   ----------------------------
   -- Writable_Directory_Cap --
   ----------------------------

   overriding function Writable_Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Writable_Directory.Server.Cap_Of (Item));

end Directory.Server;
