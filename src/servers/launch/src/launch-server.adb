with Rose.Objects;

with Rose.Console_IO;

with Rose.Server;
with Rose.Interfaces.Launch.Server;

package body Launch.Server is

   procedure On_Launch
     (Id    : in     Rose.Objects.Capability_Identifier;
      Image : in     Rose.Capabilities.Capability;
      Cap_1 : in     Rose.Capabilities.Capability;
      Cap_2 : in     Rose.Capabilities.Capability;
      Cap_3 : in     Rose.Capabilities.Capability;
      Cap_4 : in     Rose.Capabilities.Capability;
      Cap_5 : in     Rose.Capabilities.Capability;
      Cap_6 : in     Rose.Capabilities.Capability;
      Cap_7 : in     Rose.Capabilities.Capability);

   Context : Rose.Server.Server_Context;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Console_IO.Put_Line ("launch: creating server");
      Rose.Interfaces.Launch.Server.Create_Server
        (Context, On_Launch'Access);
   end Create_Server;

   ---------------
   -- On_Launch --
   ---------------

   procedure On_Launch
     (Id    : in     Rose.Objects.Capability_Identifier;
      Image : in     Rose.Capabilities.Capability;
      Cap_1 : in     Rose.Capabilities.Capability;
      Cap_2 : in     Rose.Capabilities.Capability;
      Cap_3 : in     Rose.Capabilities.Capability;
      Cap_4 : in     Rose.Capabilities.Capability;
      Cap_5 : in     Rose.Capabilities.Capability;
      Cap_6 : in     Rose.Capabilities.Capability;
      Cap_7 : in     Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id, Image, Cap_1, Cap_2, Cap_3,
                           Cap_4, Cap_5, Cap_6, Cap_7);
   begin
      Rose.Console_IO.Put_Line ("launching");
   end On_Launch;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Launch.Server;
