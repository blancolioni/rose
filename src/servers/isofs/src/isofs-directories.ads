with System.Storage_Elements;

with Rose.Objects;
with Rose.Invocation;
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

   function Read_File
     (Directory : Directory_Type;
      Index     : Positive)
      return Rose.Capabilities.Capability;

   procedure Read
     (File   : Positive;
      Buffer : out System.Storage_Elements.Storage_Array;
      Count  : out System.Storage_Elements.Storage_Count);

   procedure Get_Entry_Name
     (Directory : Directory_Type;
      Index     : Positive;
      Name      : out String;
      Last      : out Natural);

   function Get_Identified_Directory
     (Identifier : Rose.Objects.Capability_Identifier)
      return Directory_Type;

   procedure Send_Directory_Caps
     (Directory             : Directory_Type;
      Params                : in out Rose.Invocation.Invocation_Record);

private

   type Directory_Type is new Natural;
   No_Directory : constant Directory_Type := 0;
   Root_Directory : constant Directory_Type := 1;

end IsoFS.Directories;
