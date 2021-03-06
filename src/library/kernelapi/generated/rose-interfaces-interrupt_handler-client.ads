with Rose.Words;
with Rose.Capabilities;
with Rose.Invocation;

package Rose.Interfaces.Interrupt_Handler.Client is

   type Interrupt_Handler_Client is private;

   procedure Open_Cap_Set
     (Client           :    out Interrupt_Handler_Client;
      Handle_Interrupt : in     Rose.Capabilities.Capability);

   procedure Open
     (Client        :    out Interrupt_Handler_Client;
      Interface_Cap : in     Rose.Capabilities.Capability);

   procedure Handle_Interrupt
     (Item : Interrupt_Handler_Client;
      Code : Rose.Words.Word);

   function Get_Interface_Cap (Item : Interrupt_Handler_Client)
      return Rose.Capabilities.Capability;

   function Get_Handle_Interrupt_Cap (Item : Interrupt_Handler_Client)
      return Rose.Capabilities.Capability;
   function Has_Error return Boolean;
   function Get_Last_Error return Rose.Invocation.Invocation_Error;

private

   type Interrupt_Handler_Client is
      record
         Is_Open          : Boolean := False;
         Interface_Cap    : Rose.Capabilities.Capability := 0;
         Handle_Interrupt : Rose.Capabilities.Capability := 0;
      end record;

end Rose.Interfaces.Interrupt_Handler.Client;
