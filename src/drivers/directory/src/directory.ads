with Rose.Capabilities;

package Directory is

   pragma Pure (Directory);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   Delete_Cap_Cap       : constant Rose.Capabilities.Capability := 3;
   --  For deleting the caps of a deleted entry (#346).
   File_Launch_Cap      : constant Rose.Capabilities.Capability := 4;
   Directory_Launch_Cap : constant Rose.Capabilities.Capability := 5;
   --  The Launch interfaces of the file and directory servers, given
   --  at launch, so that a directory can create only files and
   --  directories.

end Directory;
