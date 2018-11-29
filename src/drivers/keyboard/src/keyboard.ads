with Rose.Capabilities;

package Keyboard is

   pragma Pure (Keyboard);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Register_IRQ_Cap    : constant Rose.Capabilities.Capability := 2;

end Keyboard;
