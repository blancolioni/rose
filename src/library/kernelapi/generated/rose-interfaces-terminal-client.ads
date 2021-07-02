with System.Storage_Elements;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Terminal.Client is

   type Terminal_Client is private;

   procedure Open_Cap_Set
     (Client :    out Terminal_Client;
      Read   : in     Rose.Capabilities.Capability;
      Write  : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Terminal_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Read
     (Item   : in     Terminal_Client;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   procedure Write
     (Item   : Terminal_Client;
      Buffer : System.Storage_Elements.Storage_Array);

   function Get_Interface_Cap (Item : Terminal_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Terminal_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Read          : Rose.Capabilities.Capability := 0;
         Write         : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Terminal.Client;
