package Tests.Directories is

   function Directory_Test return Boolean;
   --  Launch a directory server, create a file and a subdirectory in
   --  it, and look them up through the writable and read-only
   --  interfaces.

   function Version_Test return Boolean;
   --  Launch a directory server, and make versions of a file in it,
   --  through the file (ADR 0026): what the directory's readers see
   --  changes only on a commit, and a version of a deleted file fails.

end Tests.Directories;
