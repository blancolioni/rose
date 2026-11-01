--  Rose's System.OS_Primitives: what Ada.Calendar needs.
--
--  GCC's is a binding to the POSIX clock and to nanosleep. Rose has
--  neither. Clock reads the real time from the source that Rose.Clock
--  registers (Rose.Clock_Source), and a delay is not available: a program
--  can't block until a time yet (#497).

package System.OS_Primitives is
   pragma Preelaborate;

   Max_Sensible_Delay : constant Duration :=
                          Duration'Min (183 * 24 * 60 * 60.0,
                                        Duration'Last);

   Max_System_Delay : constant Duration := Max_Sensible_Delay;

   procedure Initialize;
   --  Does nothing. Ada.Calendar calls it when it is elaborated.

   function Clock return Duration;
   --  The real time, as seconds since 1970-01-01 00:00:00 UTC. It raises
   --  Program_Error if no program has opened the clock (Rose.Clock.Open).

   Relative          : constant := 0;
   Absolute_Calendar : constant := 1;
   Absolute_RT       : constant := 2;

   procedure Timed_Delay (Time : Duration; Mode : Integer);
   --  Raises Program_Error: a delay is not supported yet.

end System.OS_Primitives;
