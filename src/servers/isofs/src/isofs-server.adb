with System.Storage_Elements;

with Rose.Objects;
with Rose.System_Calls.Client;

with Rose.Interfaces.Block_Device.Client;
with Rose.Interfaces.Directory.Server;
with Rose.Interfaces.File_System.Server;
with Rose.Interfaces.Stream_Reader.Server;

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

   function Read_File
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability;

   function Check_Directory
     (Id : Rose.Objects.Capability_Identifier)
      return IsoFS.Directories.Directory_Type;

   procedure Stream_Reader_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

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
      pragma Unreferenced (Id, Index);
   begin
      return 0;
   end Get_Ordinary_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Id    : in     Rose.Objects.Capability_Identifier;
      Index : in     Positive)
      return Rose.Capabilities.Capability
   is
      use IsoFS.Directories;
      Directory : constant Directory_Type := Check_Directory (Id);
   begin
      if Directory = No_Directory then
         return 0;
      else
         return IsoFS.Directories.Read_File (Directory, Index);
      end if;
   end Read_File;

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
      procedure Next (Cap : out Rose.Capabilities.Capability);

      ----------
      -- Next --
      ----------

      procedure Next (Cap : out Rose.Capabilities.Capability) is
      begin
         Cap := Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      end Next;

   begin
      Next (Console_Cap);
      Next (Device_Cap);

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
         Read_File             => Read_File'Access,
         Create_Directory      => null,
         Create_File           => null,
         Instanced             => True);

      Rose.Interfaces.Stream_Reader.Server.Attach_Interface
        (Server_Context => Context,
         Read           => Stream_Reader_Read'Access,
         Instanced      => True);

      Rose.Console_IO.Put_Line ("isofs: starting server");

      Rose.Server.Start_Server (Context);

   end Start_Server;

   ------------------------
   -- Stream_Reader_Read --
   ------------------------

   procedure Stream_Reader_Read
     (Id     : in     Rose.Objects.Capability_Identifier;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count)
   is
   begin
      IsoFS.Directories.Read (Positive (Id), Buffer, Last);
   end Stream_Reader_Read;

end IsoFS.Server;
