package Bootcaps.Server is

   procedure Create_Server;

   procedure Start_Server;
   --  Serve cap sets until init releases the server, once boot is done,
   --  and then exit.

end Bootcaps.Server;
