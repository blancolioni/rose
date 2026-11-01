package Tests.Directories is

   function Directory_Test return Boolean;
   --  Launch a directory server, create a file and a subdirectory in
   --  it, and look them up through the writable and read-only
   --  interfaces.

   function Version_Test return Boolean;
   --  Launch a directory server, and make versions of new and existing
   --  files in it, which are committed or discarded (#141).

end Tests.Directories;
