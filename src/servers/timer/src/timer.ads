with Rose.Capabilities;

package Timer is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : constant Rose.Capabilities.Capability := 2;

   Console_Cap           : Rose.Capabilities.Capability;
   Set_Timeout_Cap       : Rose.Capabilities.Capability;
   Get_Current_Ticks_Cap : Rose.Capabilities.Capability;

end Timer;
