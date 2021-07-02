with Rose.Interfaces.Directory.Client;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.File_System.Client is

   type File_System_Client is private;

   procedure Open_Cap_Set
     (Client         :    out File_System_Client;
      Root_Directory : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out File_System_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   function Root_Directory (Item : File_System_Client)
      return Rose.Interfaces.Directory.Client.Directory_Client;

   function Get_Interface_Cap (Item : File_System_Client)
      return Rose.Capabilities.Capability;

   function Get_Root_Directory_Cap (Item : File_System_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type File_System_Client is
      record
         Is_Open        : Boolean := False;
         Interface_Cap  : Rose.Capabilities.Capability := 0;
         Root_Directory : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.File_System.Client;
