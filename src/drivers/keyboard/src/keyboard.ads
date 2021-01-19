with Rose.Capabilities;

package Keyboard is

   pragma Pure (Keyboard);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 4;
   Register_IRQ_Cap    : constant Rose.Capabilities.Capability := 5;
   Command_Cap         : constant Rose.Capabilities.Capability := 6;
   Data_Cap            : constant Rose.Capabilities.Capability := 7;
   Read_Key_Cap        : constant Rose.Capabilities.Capability := 8;
   Read_Status_Cap     : constant Rose.Capabilities.Capability := 9;
   Console_Cap         : constant Rose.Capabilities.Capability := 10;

end Keyboard;
