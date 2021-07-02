with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage.Client;
with Rose.Capabilities;
with Rose.Interfaces.Process.Client;
with Rose.Invocation;

package Rose.Interfaces.Executable.Client is

   type Executable_Client is private;

   procedure Open_Cap_Set
     (Client :    out Executable_Client;
      Launch : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Executable_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Launch
     (Item  : Executable_Client;
      Image : Rose.Interfaces.Region.Client.Region_Client;
      Store : Rose.Interfaces.Storage.Client.Storage_Client;
      Caps  : Rose.Capabilities.Capability_Array)
   return Rose.Interfaces.Process.Client.Process_Client;

   function Get_Interface_Cap (Item : Executable_Client)
      return Rose.Capabilities.Capability;

   function Get_Launch_Cap (Item : Executable_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Executable_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Launch        : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Executable.Client;
