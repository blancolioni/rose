with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

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

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Launch sends its environment through the invoke buffer; without
   --  one, Send_Storage_Array clears the cap of the invocation.

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

      --  Name is in the invoke buffer, which Launch overwrites when it
      --  sends the environment.

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
         New_Entry.Name := Saved_Name;
         New_Entry.Name_Last := Saved_Last;
         New_Entry.Kind := Kind;
         New_Entry.Process.Open (Process_Cap);
         if Kind = Rose.Interfaces.Directory.Ordinary_File then
            New_Entry.File.Open (Interface_Cap);
         else
            New_Entry.Child.Open (Interface_Cap);
         end if;
      end;

      return Interface_Cap;
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

   ------------
   -- Launch --
   ------------

   function Launch
     (Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Interfaces.Process.Process_Object'Class
   is
      No_Environment : System.Storage_Elements.Storage_Array (1 .. 0);
      No_Caps        : Rose.Capabilities.Capability_Array (1 .. 0);
   begin
      if Kind = Rose.Interfaces.Directory.Ordinary_File then
         return File_Launch.Launch (No_Caps, No_Environment);
      else
         --  A subdirectory can create files and directories too.
         return Directory_Launch.Launch
           ((File_Launch_Cap, Directory_Launch_Cap), No_Environment);
      end if;
   end Launch;

   ------------------
   -- Remove_Entry --
   ------------------

   procedure Remove_Entry (Index : Positive) is
      Item : Entry_Record renames Entries (Order (Index));
   begin
      Item.Process.Destroy;

      --  Closing the clients deletes their caps, including the read-only
      --  interface, which is the one the client fetched.

      Item.Process.Close;
      if Item.Kind = Rose.Interfaces.Directory.Ordinary_File then
         Item.File.Close;
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

   ----------------------------
   -- Writable_Directory_Cap --
   ----------------------------

   overriding function Writable_Directory_Cap
     (Item : in out Directory_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Writable_Directory.Server.Cap_Of (Item));

end Directory.Server;
