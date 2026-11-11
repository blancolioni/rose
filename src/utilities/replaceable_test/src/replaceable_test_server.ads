with Ada.Streams;

with Rose.Capabilities;
with Rose.Replacement;

package Replaceable_Test_Server is

   --  The server of Rose.Core.Replaceable_Test (#865).

   procedure Write_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps);
   --  Its state, for a new version.

   procedure Read_State
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps);
   --  The state of the version that this one takes over from, which the
   --  main reads before it calls the command.

   procedure Create_Server
     (Name            : String;
      Cap_Set_Service : Rose.Capabilities.Capability;
      Served          : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability);
   --  Attach the Named and Replaceable interfaces, and return their caps.
   --  Name and Cap_Set_Service are used only by a version that took over
   --  from none.

   procedure Start_Server;
   --  Serve, and never return.

end Replaceable_Test_Server;
