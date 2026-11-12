with Rose.Capabilities;

package File_Holder is

   --  Hold a file and two of its streams across its replacement and a
   --  restore, and check them the first time the process is restored
   --  (#867, ADR 0032).

   procedure Create_Server
     (Commands        : Rose.Capabilities.Capability;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability);
   --  Launch a file through Commands, the command registry's Commands
   --  interface, with Cap_Set_Service for its role, write hello to it,
   --  open a writer and a reader, and read two bytes. Control is the
   --  file's Replaceable interface, or the null cap if the file didn't
   --  start. Serve Restorable, so that the kernel says when the process is
   --  restored.

   procedure Start_Server;
   --  Serve. It does not return.

end File_Holder;
