with Rose.Capabilities;

package Timer is

   pragma Pure (Timer);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Console_Cap         : constant Rose.Capabilities.Capability := 2;

   Set_Timeout_Cap       : constant Rose.Capabilities.Capability := 3;
   Get_Current_Ticks_Cap : constant Rose.Capabilities.Capability := 4;

end Timer;
