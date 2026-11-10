with Rose.Capabilities;

package Rose.System_Calls.Own_Bank is

   --  The process's own bank (ADR 0015, #750): a sub-bank of the bank it
   --  was launched against, which is destroyed when the process ends, and
   --  with it the processes launched against it. It is what a process
   --  launches against by default (Launch.Launch, Launch.Launch_Command).

   function Cap return Rose.Capabilities.Capability;
   --  The cap for the process's own bank, which mem gives through the
   --  process's Process interface (Rose.Capabilities.Standard), or the
   --  null cap if it has none. It is asked for once, and kept. Only for
   --  a process that exec launched: a boot module's caps are laid out
   --  otherwise.

end Rose.System_Calls.Own_Bank;
