with Ada.Numerics.Complex_Arrays;
with Ada.Numerics.Complex_Elementary_Functions;
with Ada.Numerics.Complex_Types;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Numerics.Long_Long_Elementary_Functions;
with Ada.Numerics.Long_Long_Real_Arrays;
with Ada.Numerics.Long_Real_Arrays;
with Ada.Text_IO;

package body Tests.Numerics is

   Passed : Boolean := True;

   procedure Fail (Name : String);
   --  Write Name as a failed check, and fail the test.

   generic
      Type_Name : String;
      type Real is digits <>;
      with package Functions is
        new Ada.Numerics.Generic_Elementary_Functions (Real);
   procedure Check_Functions;
   --  Check the elementary functions of Real against known values, to
   --  within a few units in the last place.

   procedure Check_Complex;
   --  Check the complex Sqrt and Exp.

   procedure Check_Random;
   --  Check that generators given the same seed give the same sequence,
   --  and that Reset with no seed, without a clock, gives the sequence of
   --  a generator that was never reset.

   -----------------
   -- Arrays_Test --
   -----------------

   function Arrays_Test return Boolean is
      use Ada.Numerics.Long_Real_Arrays;

      Tolerance : constant := 1.0E-12;

      function Near (Got, Want : Real_Vector) return Boolean
      is (abs (Got - Want) <= Tolerance);

      function Near (Got, Want : Real_Matrix) return Boolean
      is (for all I in Got'Range (1) =>
            (for all J in Got'Range (2) =>
               abs (Got (I, J) - Want (I, J)) <= Tolerance));
      --  Got and Want have the same bounds: every matrix here is a
      --  Matrix_2, or a result whose bounds come from one. A positional
      --  aggregate of Real_Matrix itself would start at Integer'First.

      subtype Vector_2 is Real_Vector (1 .. 2);
      subtype Matrix_2 is Real_Matrix (1 .. 2, 1 .. 2);

      A : constant Matrix_2 := ((2.0, 1.0), (1.0, 3.0));
      B : constant Matrix_2 := ((1.0, 2.0), (3.0, 4.0));
      S : constant Matrix_2 := ((2.0, 1.0), (1.0, 2.0));

      Product       : constant Matrix_2 := ((5.0, 8.0), (10.0, 14.0));
      Solution      : constant Vector_2 := (0.8, 1.4);
      Inverse_A     : constant Matrix_2 := ((0.6, -0.2), (-0.2, 0.4));
      Eigenvalues_S : constant Vector_2 := (3.0, 1.0);
   begin
      Ada.Text_IO.Put ("Ada.Numerics arrays test");
      Passed := True;

      if not Near (A * B, Product) then
         Fail ("matrix product");
      end if;
      if not Near (Solve (A, Vector_2'(3.0, 5.0)), Solution) then
         Fail ("Solve");
      end if;
      if not Near (Inverse (A), Inverse_A) then
         Fail ("Inverse");
      end if;
      if abs (Determinant (A) - 5.0) > Tolerance then
         Fail ("Determinant");
      end if;

      --  The eigenvalues come in non-increasing order (RM G.3.1), and
      --  each column of the vectors is the eigenvector of its value.

      if not Near (Eigenvalues (S), Eigenvalues_S) then
         Fail ("Eigenvalues");
      end if;

      declare
         Values  : Vector_2;
         Vectors : Matrix_2;
      begin
         Eigensystem (S, Values, Vectors);
         if not Near (S * Vectors,
                      Vectors * Matrix_2'((Values (1), 0.0),
                                          (0.0, Values (2))))
         then
            Fail ("Eigensystem");
         end if;
      end;

      declare
         package Long_Long renames Ada.Numerics.Long_Long_Real_Arrays;
         M : constant Long_Long.Real_Matrix := ((4.0, 1.0), (2.0, 3.0));
      begin
         if abs (Long_Long.Determinant (M) - 10.0)
           > 8.0 * Long_Long_Float'Model_Epsilon
         then
            Fail ("Long_Long_Float Determinant");
         end if;
      end;

      declare
         use Ada.Numerics.Complex_Types;
         use Ada.Numerics.Complex_Arrays;

         Float_Tolerance : constant := 1.0E-5;

         M : constant Complex_Matrix :=
               (((0.0, 1.0), (0.0, 0.0)),
                ((0.0, 0.0), (2.0, 0.0)));
         Y : constant Complex_Vector := ((1.0, 0.0), (4.0, 0.0));
         X : constant Complex_Vector := ((0.0, -1.0), (2.0, 0.0));
         U : constant Complex_Vector := ((1.0, 1.0), (2.0, 0.0));
         V : constant Complex_Vector := ((1.0, -1.0), (3.0, 0.0));
         Eight : constant Complex := (8.0, 0.0);
      begin
         if abs (Solve (M, Y) - X) > Float_Tolerance then
            Fail ("complex Solve");
         end if;
         if abs (U * V - Eight) > Float_Tolerance then
            Fail ("complex inner product");
         end if;
      end;

      return Passed;
   end Arrays_Test;

   -------------------
   -- Check_Complex --
   -------------------

   procedure Check_Complex is
      use Ada.Numerics.Complex_Types;
      use Ada.Numerics.Complex_Elementary_Functions;

      function Near (Got, Want : Complex) return Boolean
      is (abs (Got - Want) <= 8.0 * Float'Model_Epsilon);

      Root  : constant Complex :=
                Sqrt (Compose_From_Cartesian (-1.0, 0.0));
      Euler : constant Complex :=
                Exp (Compose_From_Cartesian (0.0, Ada.Numerics.Pi));
   begin
      if not Near (Root, Compose_From_Cartesian (0.0, 1.0)) then
         Fail ("complex Sqrt (-1.0)");
      end if;
      if not Near (Euler, Compose_From_Cartesian (-1.0, 0.0)) then
         Fail ("complex Exp (Pi * i)");
      end if;
   end Check_Complex;

   ---------------------
   -- Check_Functions --
   ---------------------

   procedure Check_Functions is
      use Functions;

      Tolerance : constant Real := 8.0 * Real'Model_Epsilon;

      Sqrt_2 : constant := 1.41421_35623_73095_04880_16887_24209_698;

      procedure Near
        (Name      : String;
         Got, Want : Real);
      --  Check that Got is within Tolerance of Want, relative to Want
      --  when it is more than 1.

      procedure Not_Raised
        (Name  : String;
         Value : Real);
      --  Fail: Value is the result of a call that should have raised.

      ----------
      -- Near --
      ----------

      procedure Near
        (Name      : String;
         Got, Want : Real)
      is
      begin
         if abs (Got - Want) > Tolerance * Real'Max (1.0, abs Want) then
            Fail (Type_Name & " " & Name);
         end if;
      end Near;

      ----------------
      -- Not_Raised --
      ----------------

      procedure Not_Raised
        (Name  : String;
         Value : Real)
      is
         pragma Unreferenced (Value);
      begin
         Fail (Type_Name & " " & Name);
      end Not_Raised;

   begin
      Near ("Sqrt (2.0)", Sqrt (2.0), Sqrt_2);
      Near ("Sin (Pi / 6.0)", Sin (Ada.Numerics.Pi / 6.0), 0.5);
      Near ("Cos (Pi / 3.0)", Cos (Ada.Numerics.Pi / 3.0), 0.5);
      Near ("Tan (Pi / 4.0)", Tan (Ada.Numerics.Pi / 4.0), 1.0);
      Near ("Sin (1.0E6)", Sin (1.0E6),
            -0.34999_35021_71292_95213);
      Near ("Log (10.0)", Log (10.0),
            2.30258_50929_94045_68401_79914_54684_364);
      Near ("Exp (1.0)", Exp (1.0), Ada.Numerics.e);
      Near ("Exp (-2.5)", Exp (-2.5),
            0.08208_49986_23898_79516_95286_74467_160);
      Near ("Arctan (1.0)", Arctan (1.0), Ada.Numerics.Pi / 4.0);
      Near ("Tanh (0.5)", Tanh (0.5),
            0.46211_71572_60009_75850_23184_83643_67);
      Near ("2.0 ** 0.5", 2.0 ** 0.5, Sqrt_2);
      Near ("1.5 ** 3", 1.5 ** 3, 3.375);
      Near ("10.0 ** (-2)", 10.0 ** (-2), 0.01);

      begin
         Not_Raised ("Sqrt (-1.0) did not raise Argument_Error",
                     Sqrt (-1.0));
      exception
         when Ada.Numerics.Argument_Error =>
            null;
      end;

      begin
         Not_Raised ("Log (0.0) did not raise Constraint_Error",
                     Log (0.0));
      exception
         when Constraint_Error =>
            null;
      end;
   end Check_Functions;

   ------------------
   -- Check_Random --
   ------------------

   procedure Check_Random is
      use Ada.Numerics.Float_Random;

      subtype Die is Integer range 1 .. 6;
      package Dice is new Ada.Numerics.Discrete_Random (Die);

      Count : constant := 100;

      First, Second : Generator;
      Never_Reset   : Generator;
      Same          : Boolean := True;
      In_Range      : Boolean := True;
      Throw_1       : Dice.Generator;
      Throw_2       : Dice.Generator;
      Same_Throws   : Boolean := True;
   begin
      Reset (First, 496);
      Reset (Second, 496);
      for I in 1 .. Count loop
         declare
            X : constant Uniformly_Distributed := Random (First);
         begin
            Same := Same and then X = Random (Second);
            In_Range := In_Range and then X < 1.0;
         end;
      end loop;
      if not Same then
         Fail ("Float_Random with the same seed");
      end if;
      if not In_Range then
         Fail ("Float_Random out of range");
      end if;

      Dice.Reset (Throw_1, 664);
      Dice.Reset (Throw_2, 664);
      for I in 1 .. Count loop
         Same_Throws :=
           Same_Throws and then Dice.Random (Throw_1) = Dice.Random (Throw_2);
      end loop;
      if not Same_Throws then
         Fail ("Discrete_Random with the same seed");
      end if;

      Reset (First);
      Same := True;
      for I in 1 .. Count loop
         Same := Same and then Random (First) = Random (Never_Reset);
      end loop;
      if not Same then
         Fail ("Reset with no seed and no clock");
      end if;
   end Check_Random;

   ----------
   -- Fail --
   ----------

   procedure Fail (Name : String) is
   begin
      Ada.Text_IO.Put (": " & Name);
      Passed := False;
   end Fail;

   -------------------
   -- Numerics_Test --
   -------------------

   function Numerics_Test return Boolean is

      procedure Check_Float is
        new Check_Functions
          ("Float", Float, Ada.Numerics.Elementary_Functions);

      procedure Check_Long_Float is
        new Check_Functions
          ("Long_Float", Long_Float,
           Ada.Numerics.Long_Elementary_Functions);

      procedure Check_Long_Long_Float is
        new Check_Functions
          ("Long_Long_Float", Long_Long_Float,
           Ada.Numerics.Long_Long_Elementary_Functions);

   begin
      Ada.Text_IO.Put ("Ada.Numerics test");
      Passed := True;
      Check_Float;
      Check_Long_Float;
      Check_Long_Long_Float;
      Check_Complex;
      Check_Random;
      return Passed;
   end Numerics_Test;

   ----------------------
   -- Random_Seed_Test --
   ----------------------

   function Random_Seed_Test return Boolean is
      use Ada.Numerics.Float_Random;

      From_Clock  : Generator;
      Never_Reset : Generator;
      Same        : Boolean := True;
   begin
      Ada.Text_IO.Put ("Random seed test");
      Passed := True;
      Reset (From_Clock);
      for I in 1 .. 10 loop
         Same := Same and then Random (From_Clock) = Random (Never_Reset);
      end loop;
      if Same then
         Fail ("Reset with no seed did not use the clock");
      end if;
      return Passed;
   end Random_Seed_Test;

end Tests.Numerics;
