with Rose.Devices.Block.Client;
with Rose.Objects;
with Rose.Invocation;
with Rose.Interfaces.Directory;

package IsoFS.Directories is

   ISO_Sector_Size         : constant := 2048;
   ISO_First_Volume_Sector : constant := 16;

   Descriptor_Type_Offset  : constant := 0;
   Boot_Record_Descriptor  : constant := 0;
   Primary_Volume_Descriptor : constant := 1;

   type Directory_Type is private;

   No_Directory : constant Directory_Type;

   function Get_Root_Directory
     (Device : Rose.Devices.Block.Client.Block_Device_Type)
      return Directory_Type;

   function Get_Child_Directory
     (Parent     : Directory_Type;
      Child_Name : String)
      return Directory_Type;

   function Get_Entry_Count
     (Directory : Directory_Type)
      return Natural;

   function Get_Entry_Kind
     (Directory : Directory_Type;
      Index     : Positive)
      return Rose.Interfaces.Directory.File_Kind;

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
