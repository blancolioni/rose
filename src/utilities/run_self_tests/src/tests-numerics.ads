package Tests.Numerics is

   function Numerics_Test return Boolean;
   --  Ada.Numerics from the run-time system (#496): Sqrt, Sin, Cos, Tan,
   --  Log, Exp, Arctan, Tanh and ** for Float, Long_Float and
   --  Long_Long_Float against known values, the complex Sqrt and Exp,
   --  and generators given the same seed giving the same sequence. It
   --  runs before Tests.Clock.Clock_Test opens the clock, so it also
   --  checks that Reset with no seed, without a clock, gives the
   --  sequence of a generator that was never reset.

   function Arrays_Test return Boolean;
   --  Ada.Numerics.Generic_Real_Arrays and Generic_Complex_Arrays (#664):
   --  a matrix product, Solve, Inverse, Determinant, and the eigenvalues
   --  and eigenvectors of a symmetric matrix, for Long_Float; a
   --  determinant for Long_Long_Float; and a complex Solve and inner
   --  product.

   function Random_Seed_Test return Boolean;
   --  Reset with no seed, once the clock is open, seeds from the clock,
   --  so that the sequence is not the one of a generator that was never
   --  reset. It runs after Tests.Clock.Clock_Test.

end Tests.Numerics;
