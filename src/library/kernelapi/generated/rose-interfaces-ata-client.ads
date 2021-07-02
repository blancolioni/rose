with Rose.Interfaces.Block_Device.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Ata.Client is

   type Ata_Client is private;

   procedure Open_Cap_Set
     (Client     :    out Ata_Client;
      Get_Device : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Ata_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Get_Device (Item : Ata_Client)
      return Rose.Interfaces.Block_Device.Client.Block_Device_Client;

   function Get_Interface_Cap (Item : Ata_Client)
      return Rose.Capabilities.Capability;

   function Get_Get_Device_Cap (Item : Ata_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Ata_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Get_Device    : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Ata.Client;
