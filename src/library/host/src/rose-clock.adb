with Rose.Host.Time;

package body Rose.Clock is

   --  The body of Rose.Clock for a command built for the host (#1101):
   --  the host's clock, through Rose.Host.Time. The cap that Open is given
   --  is the null cap of the host backing of the Clock_Service role, and
   --  is only noted.

   Opened : Boolean := False;

   -------------
   -- Is_Open --
   -------------

   function Is_Open return Boolean is (Opened);

   ---------------
   -- Monotonic --
   ---------------

   function Monotonic return Rose.Words.Word_64
   is (Rose.Host.Time.Monotonic_Now);

   ----------
   -- Open --
   ----------

   procedure Open (Clock : Rose.Capabilities.Capability) is
      pragma Unreferenced (Clock);
   begin
      Opened := True;
   end Open;

   --------------
   -- Realtime --
   --------------

   function Realtime return Rose.Words.Word_64
   is (Rose.Host.Time.Realtime_Now);

   -------------------------
   -- Realtime_Is_Trusted --
   -------------------------

   function Realtime_Is_Trusted return Boolean is (True);

end Rose.Clock;
