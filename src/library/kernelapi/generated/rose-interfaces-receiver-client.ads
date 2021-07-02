with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Receiver.Client is

   type Receiver_Client is private;

   procedure Open_Cap_Set
     (Client   :    out Receiver_Client;
      Send_Cap : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Receiver_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Send_Cap
     (Item : Receiver_Client;
      Cap  : Rose.Capabilities.Capability);

   function Get_Interface_Cap (Item : Receiver_Client)
      return Rose.Capabilities.Capability;

   function Get_Send_Cap_Cap (Item : Receiver_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Receiver_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Send_Cap      : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Receiver.Client;
