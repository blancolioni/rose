with Rose.Interfaces.Clock.Client;
with Rose.Interfaces.Objects;

package body Rose.Clock is

   --  The user-mode IDL target's client (#554): a reading that fails
   --  raises Invocation_Error, which is reported as Program_Error.

   Source : Rose.Interfaces.Clock.Client.Clock_Client;
   Opened : Boolean := False;

   -------------
   -- Is_Open --
   -------------

   function Is_Open return Boolean is
   begin
      return Opened;
   end Is_Open;

   ---------------
   -- Monotonic --
   ---------------

   function Monotonic return Rose.Words.Word_64 is
   begin
      return Source.Monotonic;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         raise Program_Error with "Rose.Clock: the clock can't be reached";
   end Monotonic;

   ----------
   -- Open --
   ----------

   procedure Open (Clock : Rose.Capabilities.Capability) is
   begin
      Source.Open (Clock);
      Opened := True;
   end Open;

   --------------
   -- Realtime --
   --------------

   function Realtime return Rose.Words.Word_64 is
   begin
      return Source.Realtime;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         raise Program_Error with "Rose.Clock: the clock can't be reached";
   end Realtime;

   -------------------------
   -- Realtime_Is_Trusted --
   -------------------------

   function Realtime_Is_Trusted return Boolean is
   begin
      return Source.Realtime_Is_Trusted;
   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         raise Program_Error with "Rose.Clock: the clock can't be reached";
   end Realtime_Is_Trusted;

end Rose.Clock;
