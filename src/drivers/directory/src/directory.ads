with Rose.Capabilities;

package Directory is

   pragma Pure (Directory);

   Create_Endpoint_Cap  : constant Rose.Capabilities.Capability := 2;
   File_Launch_Cap      : constant Rose.Capabilities.Capability := 3;
   Directory_Launch_Cap : constant Rose.Capabilities.Capability := 4;
   --  The Launch interfaces of the file and directory servers, given
   --  at launch, so that a directory can create only files and
   --  directories.

end Directory;
