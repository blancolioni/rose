with Rose.Objects;

with Rose.Interfaces.Streams.Reader;

package Exec.Library is

   --  The installed programs, each numbered from 1, in the order they
   --  were installed. Exec.Server serves a Launch interface for each.

   procedure Set_Region (Region : Rose.Capabilities.Capability);
   --  Keep the images in Region, the cap of a Region interface.

   function Install
     (ELF_Image  : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name       : String;
      Caps       : Rose.Capabilities.Capability_Array)
     return Rose.Objects.Capability_Identifier;
   --  Install the program in ELF_Image, and return its number, or 0 if
   --  there is no room for it.

   function Update
     (ELF_Image  : in out Rose.Interfaces.Streams.Reader.Reader_Object'Class;
      Name       : String;
      Caps       : Rose.Capabilities.Capability_Array;
      Changed    : out Boolean)
     return Rose.Objects.Capability_Identifier;
   --  Install ELF_Image as the new version of the program installed as
   --  Name (#829). If its image is the same, and neither has install
   --  caps, return the installed version's number, with Changed False.
   --  Otherwise install it, retire the installed version, and return the
   --  new number with Changed True. 0 if there is no room for it.

   function Newest
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Objects.Capability_Identifier;
   --  The newest version of program Id: Id itself, or, if Update retired
   --  it, the version that replaced it, and so on (#902). A retired
   --  version's pages are kept, and its launch caps launch the newest
   --  version. 0 if there is no program Id.

   function Image_Region
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;
   --  The Region_Access of a region of the pages of program Id's ELF
   --  image, which elf maps, and can't release (#403). The region is
   --  made the first time, and kept: the store has room for a limited
   --  number of regions.

   Max_Name_Length : constant := 16;
   --  A program's name is kept to this many characters.

   procedure Get_Program_Name
     (Id   : Rose.Objects.Capability_Identifier;
      Name : out String;
      Last : out Natural);
   --  The name program Id was installed with, or nothing if there is
   --  none.

   function Install_Caps
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability_Array;
   --  The caps that program Id was installed with, which every process
   --  of it starts with.

   procedure Get_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural);
   --  The contents of the .rose_command section of program Id (ADR 0002),
   --  or nothing if it has none.

end Exec.Library;
