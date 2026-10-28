with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;

with Rose.System_Calls.Client;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Directory.Server;
with Rose.Interfaces.File;
with Rose.Interfaces.File.Server;
with Rose.Interfaces.File_System.Server;
with Rose.Interfaces.Seekable_File_Reader;
with Rose.Interfaces.Seekable_File_Reader.Server;
with Rose.System_Calls.Server;

with Rose.Server;

with Rose.Console_IO;

with IsoFS.Directories;

package body IsoFS.Server is

   Context : Rose.Server.Server_Context;
   Device  : Rose.Interfaces.Block_Device.Client.Block_Device_Client;

   function Root_Directory
     (Id : Rose.Objects.Capability_Identifier)
     return Rose.Capabilities.Capability;

   function Directory_Entry_Count
     (Id : Rose.Objects.Capability_Identifier)
      return Natural;

   procedure Directory_Entry_Name
     (Id     : in     Rose.Objects.Capability_Identifier;
      Index  : in     Positive;
      Result :    out String;
      Last   :    out Natural);

   function Directory_Entry_Kind
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Interfaces.Directory.File_Kind;

   function Directory_Entry_Size
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return System.Storage_Elements.Storage_Count;

   function Find_Entry
     (Id   : in     Rose.Objects.Capability_Identifier;
      Name : in     String)
      return Natural;

   function Get_Ordinary_File
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability;

   function Get_Directory
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability;

   function Check_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return IsoFS.Directories.Directory_Type;

   --  A file's identifier is its number in IsoFS.Directories, and a
   --  reader's is the one that IsoFS.Directories gives it (ADR 0009).

   function File_Size
     (Id : Rose.Objects.Capability_Identifier)
      return System.Storage_Elements.Storage_Count;

   function Open_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Open_Seekable_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Open_Reader_Through
     (File         : Rose.Objects.Capability_Identifier;
      Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;
   --  A new reader of File, reached through the interface Interface_Id,
   --  or a null cap if there is none.

   procedure Reader_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   procedure Reader_Release (Id : Rose.Objects.Capability_Identifier);

   procedure Reader_Seek
     (Id                : Rose.Objects.Capability_Identifier;
      Offset_From_Start : Rose.Words.Word_64);

   ---------------------
   -- Check_Directory --
   ---------------------

   function Check_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return IsoFS.Directories.Directory_Type
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type :=
                    Get_Identified_Directory (Id);
   begin
      if Directory = No_Directory then
         Rose.Console_IO.Put
           ("bad directory id: ");
         Rose.Console_IO.Put (Natural (Id));
         Rose.Console_IO.New_Line;
      end if;
      return Directory;
   end Check_Directory;

   ---------------------------
   -- Directory_Entry_Count --
   ---------------------------

   function Directory_Entry_Count
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory = No_Directory then
         return 0;
      else
         return Get_Entry_Count (Directory);
      end if;
   end Directory_Entry_Count;

   --------------------------
   -- Directory_Entry_Kind --
   --------------------------

   function Directory_Entry_Kind
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Interfaces.Directory.File_Kind
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory = No_Directory then
         return Rose.Interfaces.Directory.Special_File;
      else
         return Get_Entry_Kind (Directory, Index);
      end if;
   end Directory_Entry_Kind;

   --------------------------
   -- Directory_Entry_Name --
   --------------------------

   procedure Directory_Entry_Name
     (Id     : in     Rose.Objects.Capability_Identifier;
      Index  : in     Positive;
      Result :    out String;
      Last   :    out Natural)
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory /= No_Directory then
         Get_Entry_Name (Directory, Index, Result, Last);
      end if;
   end Directory_Entry_Name;

   --------------------------
   -- Directory_Entry_Size --
   --------------------------

   function Directory_Entry_Size
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return System.Storage_Elements.Storage_Count
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory = No_Directory then
         return 0;
      else
         return Get_Entry_Size (Directory, Index);
      end if;
   end Directory_Entry_Size;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Id : Rose.Objects.Capability_Identifier)
      return System.Storage_Elements.Storage_Count
   is
      use type Rose.Objects.Capability_Identifier;
   begin
      if Id = 0 then
         return 0;
      end if;
      return IsoFS.Directories.File_Size (Positive (Id));
   end File_Size;

   ----------------
   -- Find_Entry --
   ----------------

   function Find_Entry
     (Id   : in     Rose.Objects.Capability_Identifier;
      Name : in     String)
      return Natural
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory = No_Directory then
         return 0;
      else
         return Get_Index_By_Name (Directory, Name);
      end if;
   end Find_Entry;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
      Child     : constant Directory_Type :=
                    (if Directory = No_Directory then No_Directory
                     else Get_Child_Directory (Directory, Index));
   begin
      if Directory = No_Directory
        or else Child = No_Directory
      then
         return 0;
      else
         return Get_Directory_Interface (Child);
      end if;
   end Get_Directory;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   function Get_Ordinary_File
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
      File      : constant Natural :=
                    (if Directory = No_Directory then 0
                     else Get_File (Directory, Index));
   begin
      if File = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;
      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Rose.Interfaces.File.File_Interface,
         Identifier  => Rose.Objects.Capability_Identifier (File));
   end Get_Ordinary_File;

   -----------------
   -- Open_Reader --
   -----------------

   function Open_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is (Open_Reader_Through
         (Id, Rose.Interfaces.Seekable_File_Reader.File_Reader_Interface));

   -------------------------
   -- Open_Reader_Through --
   -------------------------

   function Open_Reader_Through
     (File         : Rose.Objects.Capability_Identifier;
      Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
      use type Rose.Objects.Capability_Identifier;
      Reader : constant Rose.Objects.Capability_Identifier :=
                 (if File = 0 then 0
                  else IsoFS.Directories.Open_Reader (Positive (File)));
   begin
      if Reader = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;
      return Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap  => Create_Endpoint_Cap,
         Endpoint_Id => Interface_Id,
         Identifier  => Reader);
   end Open_Reader_Through;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   function Open_Seekable_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is (Open_Reader_Through
         (Id,
          Rose.Interfaces.Seekable_File_Reader
          .Seekable_File_Reader_Interface));

   -----------------
   -- Reader_Read --
   -----------------

   procedure Reader_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
   begin
      IsoFS.Directories.Read (Id, Buffer, Last);
   end Reader_Read;

   --------------------
   -- Reader_Release --
   --------------------

   procedure Reader_Release (Id : Rose.Objects.Capability_Identifier) is
   begin
      IsoFS.Directories.Release_Reader (Id);
   end Reader_Release;

   -----------------
   -- Reader_Seek --
   -----------------

   procedure Reader_Seek
     (Id                : Rose.Objects.Capability_Identifier;
      Offset_From_Start : Rose.Words.Word_64)
   is
   begin
      IsoFS.Directories.Seek_Reader (Id, Offset_From_Start);
   end Reader_Seek;

   --------------------
   -- Root_Directory --
   --------------------

   function Root_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      Root : constant IsoFS.Directories.Directory_Type :=
               IsoFS.Directories.Get_Root_Directory
                 (Device);
   begin
      return IsoFS.Directories.Get_Directory_Interface (Root);
   end Root_Directory;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is

      Next_Cap_Index : Positive := 1;

      procedure Next (Cap : out Rose.Capabilities.Capability);

      ----------
      -- Next --
      ----------

      procedure Next (Cap : out Rose.Capabilities.Capability) is
      begin
         Cap :=
           Rose.System_Calls.Client.Get_Capability
             (Get_Cap_From_Set, (1 => Rose.Words.Word (Next_Cap_Index)));
         Next_Cap_Index := Next_Cap_Index + 1;
      end Next;

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);

      Next (Console_Cap);
      Next (Device_Cap);
      Next (Delete_Cap);

      Rose.System_Calls.Use_Capabilities (Delete_Cap => Delete_Cap);

      Rose.Console_IO.Open (Console_Cap);
      Rose.Console_IO.Put_Line ("isofs: starting server");

      Rose.Interfaces.Block_Device.Client.Open
        (Device, Device_Cap);

      Rose.Interfaces.File_System.Server.Create_Server
        (Server_Context => Context,
         Root_Directory => Root_Directory'Access);

      Rose.Interfaces.Directory.Server.Attach_Interface
        (Server_Context        => Context,
         Directory_Entry_Count => Directory_Entry_Count'Access,
         Directory_Entry_Name  => Directory_Entry_Name'Access,
         Directory_Entry_Kind  => Directory_Entry_Kind'Access,
         Directory_Entry_Size  => Directory_Entry_Size'Access,
         Find_Entry            => Find_Entry'Access,
         Get_Ordinary_File     => Get_Ordinary_File'Access,
         Get_Directory         => Get_Directory'Access,
         Instanced             => True);

      Rose.Interfaces.File.Server.Attach_Interface
        (Server_Context       => Context,
         Size                 => File_Size'Access,
         Open_Reader          => Open_Reader'Access,
         Open_Seekable_Reader => Open_Seekable_Reader'Access,
         Instanced            => True);

      Rose.Interfaces.Seekable_File_Reader.Server.Attach_Interface
        (Server_Context => Context,
         Read           => Reader_Read'Access,
         Release        => Reader_Release'Access,
         Seek           => Reader_Seek'Access,
         Instanced      => True);

      Rose.Console_IO.Put_Line ("isofs: starting server");

      Rose.Server.Start_Server (Context);

   end Start_Server;

end IsoFS.Server;
