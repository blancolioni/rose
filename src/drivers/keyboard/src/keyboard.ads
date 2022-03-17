with Rose.Capabilities;

package Keyboard is

   pragma Pure (Keyboard);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Register_IRQ_Cap    : constant Rose.Capabilities.Capability := 3;
   Command_Cap         : constant Rose.Capabilities.Capability := 4;
   Data_Cap            : constant Rose.Capabilities.Capability := 5;
   Read_Key_Cap        : constant Rose.Capabilities.Capability := 6;
   Read_Status_Cap     : constant Rose.Capabilities.Capability := 7;
   Console_Cap         : constant Rose.Capabilities.Capability := 8;

end Keyboard;
