package Transient_Holder is

   --  Hold a marker in pages that no checkpoint saves (#524).

   procedure Create_Server;
   --  Write the marker through the process's writable pages, say so on
   --  standard output, and serve Restorable, which says if the process
   --  is ever restored, as it must not be.

   procedure Start_Server;
   --  Serve. It does not return.

end Transient_Holder;
