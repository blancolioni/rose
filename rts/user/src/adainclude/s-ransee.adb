with Rose.Clock_Source;

package body System.Random_Seed is

   --  Not GNAT's body, which seeds from Ada.Calendar.Clock (#496). A
   --  Rose program can read the clock only if it has a cap for it, from
   --  its Clock_Service role, so this seeds from the clock when it has
   --  been opened, and otherwise from GNAT's default seed, which a
   --  generator uses when it is never reset. Without the clock, Reset
   --  with no initiator gives the same sequence on every run.

   --  These generators, Ada.Numerics.Float_Random and Discrete_Random,
   --  give deterministic pseudo-random sequences, and their seeds are easy
   --  to guess. They are not a source of secrets: that is the kernel's
   --  random source (ADR 0016, #476).

   Default_Seed : constant := 5489;
   --  Seed0 in System.Random_Numbers

   --------------
   -- Get_Seed --
   --------------

   function Get_Seed return Interfaces.Unsigned_64 is
   begin
      if Rose.Clock_Source.Has_Source then
         return Interfaces.Unsigned_64 (Rose.Clock_Source.Realtime);
      else
         return Default_Seed;
      end if;
   end Get_Seed;

end System.Random_Seed;
