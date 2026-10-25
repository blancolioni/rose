with Rose.Capabilities;

package Rose.Trace is

   --  Trace events (blancolioni/rose#197). An event is a line in the
   --  kernel's log, with the time and the process that sent it, when the
   --  kernel is started with trace-launch; otherwise the kernel ignores it.
   --  A process needs a trace cap, which init makes; without one, Event
   --  does nothing.

   procedure Set_Trace_Cap (Cap : Rose.Capabilities.Capability);

   procedure Event (Text : String);
   --  Log Text. Only its first 64 characters are kept.

   procedure Event
     (Text  : String;
      Value : Natural);
   --  Log Text followed by Value.

   procedure Event
     (Text    : String;
      Value   : Natural;
      Text_2  : String;
      Value_2 : Natural);
   --  Log Text, Value, Text_2 and Value_2.

   function Ticks return Natural;
   --  The kernel's tick count, or zero without a trace cap: a clock for
   --  timing the work a server does (see blancolioni/rose#193).

end Rose.Trace;
