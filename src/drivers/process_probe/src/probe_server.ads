package Probe_Server is

   --  A server for End_Process_Test (#320) to launch with Launch, which
   --  gives its launcher a Process cap, as a command's launch doesn't. It
   --  publishes a Named interface, whose name is "probe", and serves it
   --  until its process ends. Every other userland server is a command
   --  (#831).

   procedure Start_Server;
   --  Publish the Named interface, and serve it. It does not return.

end Probe_Server;
