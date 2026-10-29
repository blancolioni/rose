package Tests.Files is

   function File_Test return Boolean;
   --  Launch a file server, write to it, and read it back through both
   --  the writable client and a read-only one.

end Tests.Files;
