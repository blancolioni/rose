with Rose.Words;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Block_Device.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Storage.Client is

   type Storage_Client is private;

   procedure Open_Cap_Set
     (Client            :    out Storage_Client;
      Reserve_Storage   : in     Rose.Capabilities.Capability;
      Add_Backing_Store : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Storage_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Reserve_Storage
     (Item : Storage_Client;
      Size : Rose.Words.Word_64)
   return Rose.Interfaces.Region.Client.Region_Client;

   procedure Add_Backing_Store
     (Item  : Storage_Client;
      Store : Rose.Interfaces.Block_Device.Client.Block_Device_Client);

   function Get_Interface_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability;

   function Get_Reserve_Storage_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability;

   function Get_Add_Backing_Store_Cap (Item : Storage_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Storage_Client is
      record
         Is_Open           : Boolean := False;
         Interface_Cap     : Rose.Capabilities.Capability := 0;
         Reserve_Storage   : Rose.Capabilities.Capability := 0;
         Add_Backing_Store : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Storage.Client;
