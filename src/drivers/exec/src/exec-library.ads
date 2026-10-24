with Rose.Invocation;
with Rose.Objects;

with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Stream_Reader.Client;

package Exec.Library is

   procedure Set_Region
     (Client : Rose.Interfaces.Region.Client.Region_Client);

   function Install
     (ELF_Image  : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Name       : String;
      Caps       : Rose.Capabilities.Capability_Array)
     return Rose.Capabilities.Capability;
   --  Install the program in ELF_Image, and return its Launch interface:
   --  a cap whose identifier is the program's, and which Send_Interface
   --  answers.

   function Image_Region
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Interfaces.Region.Client.Region_Client;
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

   procedure Send_Interface
     (Id     : Rose.Objects.Capability_Identifier;
      Params : in out Rose.Invocation.Invocation_Record);
   --  Send the caps of the Launch interface of program Id, in the order of
   --  their index constants in Rose.Interfaces.Launch. Each has the
   --  program's identifier.

   procedure Get_Metadata
     (Id     : Rose.Objects.Capability_Identifier;
      Result : out String;
      Last   : out Natural);
   --  The contents of the .rose_command section of program Id (ADR 0002),
   --  or nothing if it has none.

end Exec.Library;
