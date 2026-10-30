with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Process;
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
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Kind      : Rose.Interfaces.Directory.File_Kind;
         File      : File_Clients.Writable_File_Client;
         Child     : Directory_Clients.Writable_Directory_Client;
         Read_Only : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only interface of the file or directory, made the
         --  first time it is asked for.
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

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Launch sends its environment through the invoke buffer; without
   --  one, Send_Storage_Array clears the cap of the invocation.

   Context          : Rose.Server.Server_Context;
   The_Directory    : aliased Directory_Object;
   Entries          : array (1 .. Max_Entries) of Entry_Record;
   Entry_Count      : Natural := 0;
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
   is (Index <= Entry_Count and then Entries (Index).Kind = Kind);

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
      Saved_Name    : String (1 .. Max_Name_Length);
      Saved_Last    : Natural;
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
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Rose.Capabilities.Null_Capability;
      end;

      if Interface_Cap = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      Entry_Count := Entry_Count + 1;
      declare
         New_Entry : Entry_Record renames Entries (Entry_Count);
      begin
         New_Entry.Name := Saved_Name;
         New_Entry.Name_Last := Saved_Last;
         New_Entry.Kind := Kind;
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
         return Entries (Index).Name (1 .. Entries (Index).Name_Last);
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
         if Entries (Index).Name (1 .. Entries (Index).Name_Last) = Name then
            return Index;
         end if;
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

      if Entries (Index).Read_Only = Rose.Capabilities.Null_Capability then
         Entries (Index).Read_Only := Entries (Index).Child.Directory_Cap;
      end if;
      return Rose.Interfaces.Directory.Client.From_Cap
        (Entries (Index).Read_Only);
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

      if Entries (Index).Read_Only = Rose.Capabilities.Null_Capability then
         Entries (Index).Read_Only := Entries (Index).File.File_Cap;
      end if;
      return Rose.Interfaces.File.Client.From_Cap (Entries (Index).Read_Only);
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
