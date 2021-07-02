with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Seekable.Client is

   type Seekable_Client is private;

   procedure Open_Cap_Set
     (Client :    out Seekable_Client;
      Seek   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Seekable_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Seek
     (Item              : Seekable_Client;
      Offset_From_Start : Rose.Words.Word_64);

   function Get_Interface_Cap (Item : Seekable_Client)
      return Rose.Capabilities.Capability;

   function Get_Seek_Cap (Item : Seekable_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Seekable_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Seek          : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Seekable.Client;
