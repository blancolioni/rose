with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Timeout.Client is

   type Timeout_Client is private;

   procedure Open_Cap_Set
     (Client     :    out Timeout_Client;
      On_Timeout : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Timeout_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure On_Timeout (Item : Timeout_Client);

   function Get_Interface_Cap (Item : Timeout_Client)
      return Rose.Capabilities.Capability;

   function Get_On_Timeout_Cap (Item : Timeout_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Timeout_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         On_Timeout    : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Timeout.Client;
