with System.Storage_Elements;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Stream_Writer.Client is

   type Stream_Writer_Client is private;

   procedure Open_Cap_Set
     (Client :    out Stream_Writer_Client;
      Write  : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Stream_Writer_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Write
     (Item   : Stream_Writer_Client;
      Buffer : System.Storage_Elements.Storage_Array);

   function Get_Interface_Cap (Item : Stream_Writer_Client)
      return Rose.Capabilities.Capability;

   function Get_Write_Cap (Item : Stream_Writer_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Stream_Writer_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Write         : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Stream_Writer.Client;
