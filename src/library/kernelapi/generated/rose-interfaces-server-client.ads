with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Server.Client is

   type Server_Client is private;

   procedure Open_Cap_Set
     (Client              :    out Server_Client;
      Published_Interface : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Server_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Published_Interface (Item : Server_Client)
      return Rose.Capabilities.Capability;

   function Get_Interface_Cap (Item : Server_Client)
      return Rose.Capabilities.Capability;

   function Get_Published_Interface_Cap (Item : Server_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Server_Client is
      record
         Is_Open             : Boolean := False;
         Interface_Cap       : Rose.Capabilities.Capability := 0;
         Published_Interface : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Server.Client;
