package Tests.Numerics is

   function Numerics_Test return Boolean;
   --  Ada.Numerics from the run-time system (#496): Sqrt, Sin, Cos, Tan,
   --  Log, Exp, Arctan, Tanh and ** for Float, Long_Float and
   --  Long_Long_Float against known values, the complex Sqrt and Exp,
   --  and generators given the same seed giving the same sequence. It
   --  runs before Tests.Clock.Clock_Test opens the clock, so it also
   --  checks that Reset with no seed, without a clock, gives the
   --  sequence of a generator that was never reset.

   function Random_Seed_Test return Boolean;
   --  Reset with no seed, once the clock is open, seeds from the clock,
   --  so that the sequence is not the one of a generator that was never
   --  reset. It runs after Tests.Clock.Clock_Test.

end Tests.Numerics;
