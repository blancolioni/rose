with Rose.Capabilities;

package Page_Probe_Server is

   --  A probe for init's test of dropping clean pages (ADR 0028, section
   --  5; #973). The filler serves Writer: its Write fills its own memory
   --  past what is free, with a checkpoint after each chunk, reads it all
   --  back, and checks the buffer it was given, which stays mapped in its
   --  shared slots while it runs. The caller serves Named: its Name sends
   --  its buffer to the filler's Write, and answers the result.

   procedure Create_Server
     (Pages      : Natural;
      Checkpoint : Rose.Capabilities.Capability;
      Target     : Rose.Capabilities.Capability);
   --  Pages and Checkpoint, init's Take_Checkpoint, are for the filler;
   --  Target, the filler's Writer, is for the caller, which fills its
   --  buffer now.

   function Writer_Cap return Rose.Capabilities.Capability;
   --  The probe's Writer interface.

   function Named_Cap return Rose.Capabilities.Capability;
   --  The probe's Named interface.

   procedure Start_Server;
   --  Serve. It does not return.

end Page_Probe_Server;
