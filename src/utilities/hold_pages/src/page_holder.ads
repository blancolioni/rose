package Page_Holder is

   --  Hold 48 MiB of pages across restores, and check them each time the
   --  process is restored (#975, ADR 0028).

   procedure Create_Server;
   --  Fill the pages, read them back, and say so on standard output.
   --  Serve Restorable, so that the kernel says when the process is
   --  restored.

   procedure Start_Server;
   --  Serve. It does not return.

end Page_Holder;
