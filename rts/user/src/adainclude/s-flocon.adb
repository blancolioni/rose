package body System.Float_Control is

   --  Not GNAT's body, which calls __gnat_init_float to reset the FPU
   --  (#670). Rose's kernel starts each process with a clean FPU, with
   --  64-bit precision and the exceptions masked, and nothing in a Rose
   --  process changes that behind the program's back, so there is nothing
   --  to reset; resetting would undo any setting the program made itself.

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      null;
   end Reset;

end System.Float_Control;
