with Rose.Capabilities;

package Swap_Probe_Server is

   --  A probe for init's test of swapping two processes (ADR 0032, #862).
   --  It serves Named, whose Name is the probe's answer: its number in
   --  decimal, or what its target answered.

   procedure Create_Server
     (Number : Natural;
      Target : Rose.Capabilities.Capability);
   --  Answer Number, or, if Target is not the null cap, what Target's
   --  Name answers when Start_Server asks it.

   function Interface_Cap return Rose.Capabilities.Capability;
   --  The probe's Named interface.

   procedure Start_Server;
   --  Ask the target, if there is one, and then serve. It does not
   --  return.

end Swap_Probe_Server;
