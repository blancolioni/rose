with Rose.Capabilities;
with Rose.Interfaces.Clock_Admin.Client;

package Rights_Server is

   --  The rights broker (ADR 0021): it holds the caps that grant
   --  authority, and gives one out only if its policy allows the user who
   --  asks. The policy and the grants are in the process's memory, so
   --  they persist with it.

   procedure Create_Server
     (Clock_Admin : Rose.Interfaces.Clock_Admin.Client.Clock_Admin_Client);
   --  Serve Rights and Rights_Admin. Clock_Admin is the clock's admin
   --  interface, which the role Clock_Admin_Service grants.

   function Rights_Cap return Rose.Capabilities.Capability;
   function Admin_Cap return Rose.Capabilities.Capability;

   procedure Start_Server;

end Rights_Server;
