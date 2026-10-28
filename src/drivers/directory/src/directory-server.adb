with System.Storage_Elements;

with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Directory;
with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Writable_Directory.Client.Conversions;
with Rose.Interfaces.Writable_Directory.Server;
with Rose.Interfaces.Writable_File.Client.Conversions;

package body Directory.Server is

   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;

   use Rose.Interfaces.Writable_Directory.Client;
   use Rose.Interfaces.Writable_File.Client;

   Max_Entries     : constant := 64;
   Max_Name_Length : constant := 64;
   --  User-mode malloc can't grow, so the entries are a fixed table.

   type Entry_Record is
      record
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Kind      : Rose.Interfaces.Directory.File_Kind;
         File      : Writable_File_Client;
         Child     : Writable_Directory_Client;
         Read_Only : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only interface of the file or directory, made the
         --  first time it is asked for.
      end record;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Launch sends its environment through the invoke buffer; without
   --  one, Send_Storage_Array clears the cap of the invocation.

   Context          : Rose.Server.Server_Context;
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

   function Create_Directory
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Rose.Capabilities.Capability;

   function Create_File
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Rose.Capabilities.Capability;

   function Directory_Entry_Count
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;

   function Directory_Entry_Kind
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Interfaces.Directory.File_Kind;

   procedure Directory_Entry_Name
     (Id     : Rose.Objects.Capability_Identifier;
      Index  : Positive;
      Result : out String;
      Last   : out Natural);

   function Directory_Entry_Size
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return System.Storage_Elements.Storage_Count;

   function Find_Entry
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Natural;

   function Get_Directory
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability;

   function Get_Ordinary_File
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability;

   function Get_Writable_Directory
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability;

   function Get_Writable_File
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability;

   function Is_Kind
     (Index : Positive;
      Kind  : Rose.Interfaces.Directory.File_Kind)
      return Boolean
   is (Index <= Entry_Count and then Entries (Index).Kind = Kind);

   ---------------
   -- Add_Entry --
   ---------------

   function Add_Entry
     (Name : String;
      Kind : Rose.Interfaces.Directory.File_Kind)
      return Rose.Capabilities.Capability
   is
      No_Environment : System.Storage_Elements.Storage_Array (1 .. 0);
      No_Caps        : Rose.Capabilities.Capability_Array (1 .. 0);
      Existing       : constant Natural := Find_Entry (0, Name);
      Process        : Rose.Interfaces.Process.Client.Process_Client;
      Interface_Cap  : Rose.Capabilities.Capability;
      Saved_Name     : String (1 .. Max_Name_Length);
      Saved_Last     : Natural;
   begin
      if Existing > 0 then
         if Entries (Existing).Kind /= Kind then
            return Rose.Capabilities.Null_Capability;
         elsif Kind = Rose.Interfaces.Directory.Ordinary_File then
            return Get_Interface_Cap (Entries (Existing).File);
         else
            return Get_Interface_Cap (Entries (Existing).Child);
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

      if Kind = Rose.Interfaces.Directory.Ordinary_File then
         Process :=
           Rose.Interfaces.Launch.Client.Launch
             (File_Launch, No_Caps, No_Environment);
      else
         --  A subdirectory can create files and directories too.
         Process :=
           Rose.Interfaces.Launch.Client.Launch
             (Directory_Launch,
              (File_Launch_Cap, Directory_Launch_Cap),
              No_Environment);
      end if;

      if Rose.Interfaces.Launch.Client.Has_Error then
         return Rose.Capabilities.Null_Capability;
      end if;

      Interface_Cap :=
        Rose.Interfaces.Process.Client.Published_Interface (Process);

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
            Open (New_Entry.File, Interface_Cap);
         else
            Open (New_Entry.Child, Interface_Cap);
         end if;
      end;

      return Interface_Cap;
   end Add_Entry;

   ----------------------
   -- Create_Directory --
   ----------------------

   function Create_Directory
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Add_Entry (Name, Rose.Interfaces.Directory.Directory);
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Add_Entry (Name, Rose.Interfaces.Directory.Ordinary_File);
   end Create_File;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Rose.Interfaces.Launch.Client.Open (File_Launch, File_Launch_Cap);
      Rose.Interfaces.Launch.Client.Open
        (Directory_Launch, Directory_Launch_Cap);

      Rose.Interfaces.Writable_Directory.Server.Publish_Interface
        (Server_Context         => Context,
         Directory_Entry_Count  => Directory_Entry_Count'Access,
         Directory_Entry_Name   => Directory_Entry_Name'Access,
         Directory_Entry_Kind   => Directory_Entry_Kind'Access,
         Directory_Entry_Size   => Directory_Entry_Size'Access,
         Find_Entry             => Find_Entry'Access,
         Get_Ordinary_File      => Get_Ordinary_File'Access,
         Get_Directory          => Get_Directory'Access,
         Get_Writable_File      => Get_Writable_File'Access,
         Get_Writable_Directory => Get_Writable_Directory'Access,
         Create_File            => Create_File'Access,
         Create_Directory       => Create_Directory'Access);
   end Create_Server;

   ---------------------------
   -- Directory_Entry_Count --
   ---------------------------

   function Directory_Entry_Count
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
      pragma Unreferenced (Id);
   begin
      return Entry_Count;
   end Directory_Entry_Count;

   --------------------------
   -- Directory_Entry_Kind --
   --------------------------

   function Directory_Entry_Kind
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Interfaces.Directory.File_Kind
   is
      pragma Unreferenced (Id);
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

   procedure Directory_Entry_Name
     (Id     : Rose.Objects.Capability_Identifier;
      Index  : Positive;
      Result : out String;
      Last   : out Natural)
   is
      pragma Unreferenced (Id);
   begin
      if Index > Entry_Count then
         Last := Result'First - 1;
         return;
      end if;

      declare
         Item   : Entry_Record renames Entries (Index);
         Length : constant Natural :=
           Natural'Min (Item.Name_Last, Result'Length);
      begin
         Last := Result'First + Length - 1;
         Result (Result'First .. Last) := Item.Name (1 .. Length);
      end;
   end Directory_Entry_Name;

   --------------------------
   -- Directory_Entry_Size --
   --------------------------

   function Directory_Entry_Size
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Id);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return Size (Entries (Index).File);
      else
         return 0;
      end if;
   end Directory_Entry_Size;

   ----------------
   -- Find_Entry --
   ----------------

   function Find_Entry
     (Id   : Rose.Objects.Capability_Identifier;
      Name : String)
      return Natural
   is
      pragma Unreferenced (Id);
   begin
      for Index in 1 .. Entry_Count loop
         if Entries (Index).Name (1 .. Entries (Index).Name_Last) = Name then
            return Index;
         end if;
      end loop;
      return 0;
   end Find_Entry;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if not Is_Kind (Index, Rose.Interfaces.Directory.Directory) then
         return Rose.Capabilities.Null_Capability;
      end if;

      if Entries (Index).Read_Only = Rose.Capabilities.Null_Capability then
         Entries (Index).Read_Only :=
           Rose.Interfaces.Directory.Client.Get_Interface_Cap
             (Rose.Interfaces.Writable_Directory.Client.Conversions
              .To_Directory (Entries (Index).Child));
      end if;
      return Entries (Index).Read_Only;
   end Get_Directory;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   function Get_Ordinary_File
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if not Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return Rose.Capabilities.Null_Capability;
      end if;

      if Entries (Index).Read_Only = Rose.Capabilities.Null_Capability then
         Entries (Index).Read_Only :=
           Rose.Interfaces.File.Client.Get_Interface_Cap
             (Rose.Interfaces.Writable_File.Client.Conversions.To_File
                (Entries (Index).File));
      end if;
      return Entries (Index).Read_Only;
   end Get_Ordinary_File;

   ----------------------------
   -- Get_Writable_Directory --
   ----------------------------

   function Get_Writable_Directory
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Directory) then
         return Get_Interface_Cap (Entries (Index).Child);
      else
         return Rose.Capabilities.Null_Capability;
      end if;
   end Get_Writable_Directory;

   -----------------------
   -- Get_Writable_File --
   -----------------------

   function Get_Writable_File
     (Id    : Rose.Objects.Capability_Identifier;
      Index : Positive)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      if Is_Kind (Index, Rose.Interfaces.Directory.Ordinary_File) then
         return Get_Interface_Cap (Entries (Index).File);
      else
         return Rose.Capabilities.Null_Capability;
      end if;
   end Get_Writable_File;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Directory.Server;
