with Rose.Capabilities;

with Rose.Interfaces.Screens.Surface;

package Trellis_Server is

   --  The display server, trellis (ADR 0022, #583).

   procedure Create_Server
     (Root   : in out Rose.Interfaces.Screens.Surface.Surface_Object'Class;
      Served : out Rose.Capabilities.Capability);
   --  Take Root as the surface to draw on: map its buffer, draw the
   --  background on it, and ask it for a signal when it has events.
   --  Served is the Display.

   procedure Start_Server;

end Trellis_Server;
