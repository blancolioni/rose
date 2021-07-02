with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Keyboard_Handler.Client is

   type Keyboard_Handler_Client is private;

   procedure Open_Cap_Set
     (Client     :    out Keyboard_Handler_Client;
      Handle_Key : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Keyboard_Handler_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Handle_Key
     (Item  : Keyboard_Handler_Client;
      Code  : Rose.Words.Word;
      State : Rose.Words.Word);

   function Get_Interface_Cap (Item : Keyboard_Handler_Client)
      return Rose.Capabilities.Capability;

   function Get_Handle_Key_Cap (Item : Keyboard_Handler_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Keyboard_Handler_Client is
      record
         Is_Open       : Boolean := False;
         Interface_Cap : Rose.Capabilities.Capability := 0;
         Handle_Key    : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Keyboard_Handler.Client;
