with Rose.Capabilities;

package Home_Holder is

   --  Hold a user server, its home directory and one of its sessions
   --  across the server's replacement and a restore, and check them the
   --  first time the process is restored (#831, ADR 0032).

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      User_Control    : out Rose.Capabilities.Capability);
   --  Through Commands, the command registry's Commands interface,
   --  launch a home directory, create a file in it, and launch a user
   --  server that holds it, with Cap_Set_Service for its role, against a
   --  bank of its own, which is also the user's. Start a session.
   --  User_Control is the user server's Replaceable interface, or the
   --  null cap if something didn't start. Serve Restorable, so that the
   --  kernel says when the process is restored.

   procedure Start_Server;
   --  Serve. It does not return.

end Home_Holder;
