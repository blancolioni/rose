with Rose.Capabilities;

package Keyboard is

   pragma Pure (Keyboard);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Register_IRQ_Cap    : constant Rose.Capabilities.Capability := 2;
   Read_Key_Cap        : constant Rose.Capabilities.Capability := 3;
   Read_Status_Cap     : constant Rose.Capabilities.Capability := 4;
   Command_Cap         : constant Rose.Capabilities.Capability := 5;
   Data_Cap            : constant Rose.Capabilities.Capability := 6;

end Keyboard;
