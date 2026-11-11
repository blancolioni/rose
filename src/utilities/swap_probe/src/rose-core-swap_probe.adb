with Swap_Probe_Server;

procedure Rose.Core.Swap_Probe
  (Number : Natural;
   Target : in out Rose.Interfaces.Named.Named_Object'Class;
   Probe  : out Rose.Capabilities.Capability)
is
begin

   --  The launch call completes, and then the generated main runs
   --  Swap_Probe_Server.Start_Server, which asks Target, if there is one,
   --  and then serves. So the call to Target is made after init's launch
   --  call has returned, and init can hold it.

   Swap_Probe_Server.Create_Server
     (Number => Number,
      Target => Target.Named_Cap);
   Probe := Swap_Probe_Server.Interface_Cap;
end Rose.Core.Swap_Probe;
