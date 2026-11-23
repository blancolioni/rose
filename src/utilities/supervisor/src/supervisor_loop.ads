with Rose.Capabilities;

package Supervisor_Loop is

   --  The console supervisor (#512, #526): login, then the user's
   --  session, then login again, for as long as the system runs.

   procedure Create_Server
     (Console  : Rose.Capabilities.Capability;
      Commands : Rose.Capabilities.Capability;
      Timer    : Rose.Capabilities.Capability;
      Clock    : Rose.Capabilities.Capability;
      Rights   : Rose.Capabilities.Capability;
      System   : Rose.Capabilities.Capability;
      Auth     : Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability);
   --  Keep the caps that login and the sessions are given: the console's
   --  Terminal, the command registry, the timer, the clock, the rights
   --  broker, the system directory and auth. Serve Restorable and
   --  Supervisor, whose interface cap is Control.

   procedure Start_Server
     with No_Return;
   --  Wait for Restored or Start, and then run login and sessions at the
   --  console. It does not return.

end Supervisor_Loop;
