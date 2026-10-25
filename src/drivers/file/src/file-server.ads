package File.Server is

   --  One process serves one file through Writable_File (ADR 0003). The
   --  contents are held in the process's memory, so they persist with
   --  it.

   procedure Create_Server;

   procedure Start_Server;

end File.Server;
