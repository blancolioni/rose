with Rose.Capabilities;
with System.Storage_Elements;
with Rose.Interfaces.Process.Client;
with Rose.Invocation;

package Rose.Interfaces.Launch.Client is

   type Launch_Client is private;

   procedure Open_Cap_Set
     (Client :    out Launch_Client;
      Launch : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Launch_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Launch
     (Item        : Launch_Client;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : System.Storage_Elements.Storage_Array)
   return Rose.Interfaces.Process.Client.Process_Client;

   function Get_Interface_Cap (Item : Launch_Client)
      return Rose.Capabilities.Capability;

   function Get_Launch_Cap (Item : Launch_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Launch_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Launch        : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Launch.Client;
