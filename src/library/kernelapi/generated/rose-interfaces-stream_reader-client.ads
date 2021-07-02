with System.Storage_Elements;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Stream_Reader.Client is

   type Stream_Reader_Client is private;

   procedure Open_Cap_Set
     (Client :    out Stream_Reader_Client;
      Read   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Stream_Reader_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Read
     (Item   : in     Stream_Reader_Client;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   function Get_Interface_Cap (Item : Stream_Reader_Client)
      return Rose.Capabilities.Capability;

   function Get_Read_Cap (Item : Stream_Reader_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Stream_Reader_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Read          : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Stream_Reader.Client;
