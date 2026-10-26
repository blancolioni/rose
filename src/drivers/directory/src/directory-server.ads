package Directory.Server is

   --  One process serves one directory through Writable_Directory
   --  (ADR 0003). Each entry is a file or directory process, launched
   --  when the entry is created; the directory holds its writable
   --  interface, and hands out read-only ones.

   procedure Create_Server;

   procedure Start_Server;

end Directory.Server;
