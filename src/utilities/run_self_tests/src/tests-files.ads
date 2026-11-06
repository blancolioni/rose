package Tests.Files is

   function File_Test return Boolean;
   --  Launch a file server, write to it, and read it back through both
   --  the writable client and a read-only one.

   function Untouched_Buffer_Test return Boolean;
   --  Write to a file server from buffers whose pages the test has never
   --  touched, so that mem has not mapped them: one lent read-only, one
   --  lent writable. The kernel has the pages mapped before it shares
   --  them, so the server reads zeroes from them, and not whatever page
   --  an unmapped entry named (#713).

end Tests.Files;
