package Ada.Numerics.Aux_Linker_Options is
   pragma Pure;

   --  Not GNAT's package, which links with -lm (#496). Rose has no libm:
   --  the math functions that the other Aux packages import are in the
   --  run-time system's own support library, rose-libm.c, so no extra
   --  linker options are needed.

end Ada.Numerics.Aux_Linker_Options;
