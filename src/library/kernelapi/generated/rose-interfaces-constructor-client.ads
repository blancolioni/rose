with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Constructor.Client is

   type Constructor_Client is private;

   procedure Open_Cap_Set
     (Client :    out Constructor_Client;
      Create : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Constructor_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Create (Item : Constructor_Client)
      return Rose.Capabilities.Capability;

   function Get_Interface_Cap (Item : Constructor_Client)
      return Rose.Capabilities.Capability;

   function Get_Create_Cap (Item : Constructor_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Constructor_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Create        : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Constructor.Client;
