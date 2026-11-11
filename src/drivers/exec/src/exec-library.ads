with Rose.Invocation;
with Rose.Objects;

with Rose.Interfaces.Reader;

package Exec.Library is

   --  The installed programs, each numbered from 1, in the order they
   --  were installed. Exec.Server serves a Launch interface for each.

   procedure Set_Region (Region : Rose.Capabilities.Capability);
   --  Keep the images in Region, the cap of a Region interface.

   function Install
     (ELF_Image  : in out Rose.Interfaces.Reader.Reader_Object'Class;
      Name       : String;
      Caps       : Rose.Capabilities.Capability_Array)
     return Rose.Objects.Capability_Identifier;
   --  Install the program in ELF_Image, and return its number, or 0 if
   --  there is no room for it.

   function Update
     (ELF_Image  : in out Rose.Interfaces.Reader.Reader_Object'Class;
      Name       : String;
      Caps       : Rose.Capabilities.Capability_Array;
      Changed    : out Boolean)
     return Rose.Objects.Capability_Identifier;
   --  Install ELF_Image as the new version of the program installed as
   --  Name (#829). If its image is the same, and neither has install
   --  caps, return the installed version's number, with Changed False.
   --  Otherwise install it, retire the installed version, and return the
   --  new number with Changed True. 0 if there is no room for it.

   function Is_Retired
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean;
   --  True if Update replaced program Id, or there is no program Id.
   --  A retired program can't be launched, but its pages are kept.

   function Image_Region
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;
   --  A region of the pages of program Id's ELF image. It is made the
   --  first time, and kept: the store has room for a limited number of
   --  regions.

   procedure Get_Program_Name
     (Id   : Rose.Objects.Capability_Identifier;
      Name : out String;
      Last : out Natural);
   --  The name program Id was installed with, or nothing if there is
   --  none.

   procedure Send_Install_Caps
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record);

   procedure Get_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural);
   --  The contents of the .rose_command section of program Id (ADR 0002),
   --  or nothing if it has none.

end Exec.Library;
