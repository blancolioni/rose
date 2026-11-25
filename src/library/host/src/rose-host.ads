package Rose.Host is

   --  Commands built for the host's own Ada run-time, to test them
   --  without booting Rose (#1101). idle --target host generates a
   --  command's main, which binds the command line with
   --  Rose.Command_Line.Host and passes each interface argument and role
   --  member as an object of its host backing: a type in a child of this
   --  package that implements the interface's object type over the host's
   --  files, streams and clock. interfaces/host-backings.txt lists them.
   --
   --  A backing fails a call by raising
   --  Rose.Interfaces.Objects.Invocation_Error, as a client does when its
   --  server fails the call. Its Open, which binds an argument, raises
   --  Rose.Command_Line.Command_Failed with petal's message when the
   --  argument names nothing it can open, since petal would fail the
   --  command. Nothing here is a cap, so every cap function gives the
   --  null cap.

end Rose.Host;
