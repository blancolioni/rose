with Rose.Capabilities;
with Rose.Server;

with Rose.Interfaces.Named.Server;

package body Probe_Server is

   --  The object and the context are the package's, at library level, as
   --  the server keeps access values to them.

   type Probe_Object is
     limited new Rose.Interfaces.Named.Server.Named_Interface
   with null record;

   overriding function Named_Cap
     (Item : in out Probe_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Named.Server.Cap_Of (Item));

   overriding function Name (Item : in out Probe_Object) return String
   is ("probe");

   Context : Rose.Server.Server_Context;
   Probe   : aliased Probe_Object;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Interfaces.Named.Server.Publish (Context, Probe'Access);
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Probe_Server;
