with System.Storage_Elements;

with Rose.Objects;
with Rose.Words;
with Rose.Interfaces.Directory;

with Rose.Interfaces.Block_Device.Client;

package IsoFS.Directories is

   ISO_Sector_Size         : constant := 2048;
   ISO_First_Volume_Sector : constant := 16;

   Descriptor_Type_Offset  : constant := 0;
   Boot_Record_Descriptor  : constant := 0;
   Primary_Volume_Descriptor : constant := 1;

   type Directory_Type is private;

   No_Directory : constant Directory_Type;

   function Get_Root_Directory
     (Device : Rose.Interfaces.Block_Device.Client.Block_Device_Client)
      return Directory_Type;

   function Get_Child_Directory
     (Parent     : Directory_Type;
      Child_Name : String)
      return Directory_Type;

   function Get_Child_Directory
     (Parent : Directory_Type;
      Index  : Positive)
      return Directory_Type;

   function Get_Entry_Count
     (Directory : Directory_Type)
      return Natural;

   function Get_Index_By_Name
     (Directory : Directory_Type;
      Name      : String)
      return Natural;

   function Get_Entry_Kind
     (Directory : Directory_Type;
      Index     : Positive)
      return Rose.Interfaces.Directory.File_Kind;

   function Get_Entry_Size
     (Directory : Directory_Type;
      Index     : Positive)
      return System.Storage_Elements.Storage_Count;

   function Get_File
     (Directory : Directory_Type;
      Index     : Positive)
      return Natural;
   --  The file at Index in Directory, as a number for its File
   --  interface, or 0 if there is none, or the table of files is full.

   function File_Size
     (File : Positive)
      return System.Storage_Elements.Storage_Count;

   function Open_Reader
     (File : Positive)
      return Rose.Objects.Capability_Identifier;
   --  A new reader of File, at its start, or 0 if the table of readers is
   --  full (ADR 0009).

   procedure Read
     (Reader : Rose.Objects.Capability_Identifier;
      Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count);

   procedure Seek_Reader
     (Reader : Rose.Objects.Capability_Identifier;
      Offset : Rose.Words.Word_64);
   --  The position is capped at the end of the file.

   procedure Release_Reader
     (Reader : Rose.Objects.Capability_Identifier);

   procedure Get_Entry_Name
     (Directory : Directory_Type;
      Index     : Positive;
      Name      : out String;
      Last      : out Natural);

   function Get_Identified_Directory
     (Identifier : Rose.Objects.Capability_Identifier)
      return Directory_Type;

   function Get_Directory_Interface
     (Directory             : Directory_Type)
      return Rose.Capabilities.Capability;

private

   type Directory_Type is new Natural;
   No_Directory : constant Directory_Type := 0;
   Root_Directory : constant Directory_Type := 1;

end IsoFS.Directories;
