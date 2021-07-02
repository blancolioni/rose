with System.Storage_Elements;
with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.File.Client is

   type File_Client is private;

   procedure Open_Cap_Set
     (Client :    out File_Client;
      Read   : in     Rose.Capabilities.Capability;
      Write  : in     Rose.Capabilities.Capability;
      Seek   : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out File_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Read
     (Item   : in     File_Client;
      Buffer :    out System.Storage_Elements.Storage_Array;
      Last   :    out System.Storage_Elements.Storage_Count);

   procedure Write
     (Item   : File_Client;
      Buffer : System.Storage_Elements.Storage_Array);

   procedure Seek
     (Item              : File_Client;
      Offset_From_Start : Rose.Words.Word_64);

   function Get_Interface_Cap (Item : File_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type File_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Read          : Rose.Capabilities.Capability := 0;
         Write         : Rose.Capabilities.Capability := 0;
         Seek          : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.File.Client;
