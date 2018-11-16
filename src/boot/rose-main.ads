package Rose.Main is

   procedure Kernel_Main;

   pragma Export (C, Kernel_Main, "kernel_main");

end Rose.Main;
