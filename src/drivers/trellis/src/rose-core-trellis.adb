with Rose.Capabilities;
with Rose.System_Calls;

with Rose.Interfaces.Objects;

with Trellis_Server;

procedure Rose.Core.Trellis
  (Create_Signal : in out Rose.Interfaces.Caps.Cap.Cap_Object'Class;
   Root          : in out Rose.Interfaces.Screens.Surface.Surface_Object'Class;
   Served        : out Rose.Capabilities.Capability)
is
   use type Rose.Capabilities.Capability;
begin

   --  The root signals trellis when it has events. Without a cap to make
   --  the signal cap with, trellis would never hear of them, so it
   --  refuses to start.

   if Create_Signal.Cap_Cap = Rose.Capabilities.Null_Capability then
      raise Rose.Interfaces.Objects.Invocation_Error
        with "trellis: no Create_Signal cap";
   end if;

   Rose.System_Calls.Use_Capabilities
     (Create_Signal => Create_Signal.Cap_Cap);
   Trellis_Server.Create_Server (Root, Served);
end Rose.Core.Trellis;
