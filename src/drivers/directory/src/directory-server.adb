with Rose.System_Calls.Own_Bank;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with Rose.Core.File_Client;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File;
with Rose.Interfaces.File.Client;
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

   Max_Name_Length : constant := 64;
   --  The longest name, which petal and the commands share.

   type Entry_Record is
      record
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Kind      : Rose.Interfaces.Directory.File_Kind;
         File      : File_Clients.Writable_File_Client;
         Handover  : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  A file's Replaceable interface, which only the directory
         --  holds, for a new version of the file server (#867).
         Bank      : Rose.Interfaces.Bank.Client.Bank_Client;
         --  A file's own bank, a sub-bank of the directory's, which its
         --  process was launched against. Its Destroy ends the process,
         --  and any version that replaced it, when the entry is deleted.
         Child     : Directory_Clients.Writable_Directory_Client;
         Read_Only : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only interface of the file or directory, made the
         --  first time it is asked for.
         Process   : Rose.Interfaces.Process.Client.Process_Client;
         --  A directory's process, which Launch returned. Its Destroy
         --  ends the process when the entry is deleted (#424, #346).
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

   Context          : Rose.Server.Server_Context;
   The_Directory    : aliased Directory_Object;

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
   File_Launch      : Rose.Interfaces.Launch.Client.Launch_Client;
   Directory_Launch : Rose.Interfaces.Launch.Client.Launch_Client;

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability;
   --  Launch a file or directory, add it as Name, and return its
   --  writable interface; or return the existing entry Name if it has
   --  the same kind. Null if Name is another kind, or too long, or the
   --  launch fails.

   function Find (Name : String) return Natural;
   --  The index of the entry Name, or 0 if there is none.

   function Insert_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Boolean;
   --  Launch a file or directory, and add it as the entry Name. False,
   --  with nothing added, if the launch fails.

   procedure Launch_File (Item : in out Entry_Record);
   --  Make a bank for a new file, and launch the file command against it.
   --  Raises Invocation_Error if either fails, and then destroys the bank,
   --  if there is one.

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
         elsif Kind = Rose.Interfaces.Directory.Ordinary_File then
            return Entries (Existing).File.Writable_File_Cap;
         else
            return Entries (Existing).Child.Writable_Directory_Cap;
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

      declare
         Added : Entry_Record renames Entries.Last_Element.all;
      begin
         if Kind = Rose.Interfaces.Directory.Ordinary_File then
            return Added.File.Writable_File_Cap;
         else
            return Added.Child.Writable_Directory_Cap;
         end if;
      end;
   end Add_Entry;

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
         return Directory_Clients.From_Cap
           (Entries (Index).Child.Writable_Directory_Cap);
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
           (Entries (Index).File.Writable_File_Cap);
      else
         return File_Clients.From_Cap (Rose.Capabilities.Null_Capability);
      end if;
   end Get_Writable_File;

   ------------------
   -- Insert_Entry --
   ------------------

   --  A file is the file command, Rose.Core.File, which returns its
   --  Writable_File (#867). A directory's process publishes its
   --  Writable_Directory.

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

      if Kind = Rose.Interfaces.Directory.Ordinary_File then
         begin
            Launch_File (New_Entry.all);
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Free (New_Entry);
               return False;
         end;
      else

         --  A subdirectory can create files and directories too.

         declare
            Process : Rose.Interfaces.Process.Process_Object'Class :=
              Directory_Launch.Launch
                (Rose.System_Calls.Own_Bank.Cap,
                 (File_Launch_Cap, Directory_Launch_Cap,
                  Cap_Set_Service_Cap));
            Interface_Cap : constant Rose.Capabilities.Capability :=
              Process.Published_Interface;
         begin
            New_Entry.Process.Open (Process.Process_Cap);
            if Interface_Cap = Rose.Capabilities.Null_Capability then
               New_Entry.Process.Destroy;
               New_Entry.Process.Close;
               Free (New_Entry);
               return False;
            end if;
            New_Entry.Child.Open (Interface_Cap);
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Free (New_Entry);
               return False;
         end;
      end if;

      Entries.Append (New_Entry);
      return True;
   end Insert_Entry;

   -----------------
   -- Launch_File --
   -----------------

   --  The file runs in a bank of its own, so that a new version of it,
   --  which is launched against the same bank, is ended with it too
   --  (ADR 0032, section 3).

   procedure Launch_File (Item : in out Entry_Record) is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Own      : Rose.Interfaces.Bank.Client.Bank_Client;
      Endpoint : Rose.Capabilities.Capability;
      Served   : Rose.Capabilities.Capability;
   begin
      Own.Open (Rose.System_Calls.Own_Bank.Cap);
      declare
         Sub : Rose.Interfaces.Bank.Bank_Object'Class :=
           Own.Create_Sub_Bank (0, 0);
      begin
         Item.Bank.Open (Sub.Bank_Cap);
      end;

      begin
         Endpoint :=
           File_Launch.Launch_Command (Item.Bank.Bank_Cap, No_Caps);
         if Endpoint = Rose.Capabilities.Null_Capability then
            raise Rose.Interfaces.Objects.Invocation_Error
              with "cannot launch a file";
         end if;

         Rose.Core.File_Client.Call
           (Endpoint        => Endpoint,
            Create_Endpoint => Rose.System_Calls.Create_Endpoint_Capability,
            Delete_Cap      => Rose.System_Calls.Delete_Cap_Capability,
            Cap_Set_Service => Cap_Set_Service_Cap,
            Served          => Served,
            Control         => Item.Handover);
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            Item.Bank.Destroy;
            Item.Bank.Close;
            raise;
      end;

      Item.File.Open (Served);
   end Launch_File;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry (Index : Positive) is
      Removed : Entry_Access := Entries (Index);
      Item    : Entry_Record renames Removed.all;
   begin
      if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
         Item.Bank.Destroy;
      else
         Item.Process.Destroy;
      end if;

      --  Closing the clients deletes their caps, including the read-only
      --  interface, which is the one the client fetched.

      if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
         Item.Bank.Close;
         Item.File.Close;
         if Item.Handover /= Rose.Capabilities.Null_Capability then
            Rose.System_Calls.Delete_Cap (Item.Handover);
         end if;
      else
         Item.Process.Close;
         Item.Child.Close;
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

end Directory.Server;
