with Rose.Capabilities;

package Init.Clock_Checks is

   function Run (Clock : Rose.Capabilities.Capability) return Boolean;
   --  Read the clock, through Clock, an interface cap for Clock (ADR
   --  0020), twice, two seconds apart. Report what it says on the
   --  console, and return True if both readings advanced by about two
   --  seconds, and the real time is in this century.

end Init.Clock_Checks;
