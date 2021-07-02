with Rose.Capabilities;
with System.Storage_Elements;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Stream_Reader.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Invocation;

package Rose.Interfaces.Exec.Client is

   type Exec_Client is private;

   procedure Open_Cap_Set
     (Client  :    out Exec_Client;
      Launch  : in     Rose.Capabilities.Capability;
      Install : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Exec_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Launch
     (Item        : Exec_Client;
      Caps        : Rose.Capabilities.Capability_Array;
      Environment : System.Storage_Elements.Storage_Array)
   return Rose.Interfaces.Process.Client.Process_Client;

   function Install
     (Item         : Exec_Client;
      Executable   :
        Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
      Install_Caps : Rose.Capabilities.Capability_Array)
   return Rose.Interfaces.Launch.Client.Launch_Client;

   function Get_Interface_Cap (Item : Exec_Client)
      return Rose.Capabilities.Capability;

   function Get_Install_Cap (Item : Exec_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Exec_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Launch        : Rose.Capabilities.Capability := 0;
         Install       : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Exec.Client;
