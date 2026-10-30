package Clock.Server is

   procedure Start_Server;
   --  Publish Clock_Admin, whose ancestor is Clock, read the RTC, and
   --  serve them (ADR 0020).

end Clock.Server;
