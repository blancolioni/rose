with Rose.Capabilities;

package Clock with Pure is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Ticks_Cap           : constant Rose.Capabilities.Capability := 3;
   RTC_Out_Cap         : constant Rose.Capabilities.Capability := 4;
   RTC_In_Cap          : constant Rose.Capabilities.Capability := 5;
   --  The caps of clock.driver: the kernel's tick count, and the RTC's
   --  index and data ports (out), and data port (in).

end Clock;
