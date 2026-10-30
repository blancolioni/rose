with Rose.Interfaces.Clock.Client;

package body Rose.Clock is

   use type Rose.Words.Word;

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
      Result : constant Rose.Words.Word_64 :=
                 Rose.Interfaces.Clock.Client.Monotonic (Source);
   begin
      if Rose.Interfaces.Clock.Client.Has_Error then
         raise Program_Error with "Rose.Clock: the clock can't be reached";
      end if;
      return Result;
   end Monotonic;

   ----------
   -- Open --
   ----------

   procedure Open (Clock : Rose.Capabilities.Capability) is
   begin
      Rose.Interfaces.Clock.Client.Open (Source, Clock);
      Opened := True;
   end Open;

   --------------
   -- Realtime --
   --------------

   function Realtime return Rose.Words.Word_64 is
      Result : constant Rose.Words.Word_64 :=
                 Rose.Interfaces.Clock.Client.Realtime (Source);
   begin
      if Rose.Interfaces.Clock.Client.Has_Error then
         raise Program_Error with "Rose.Clock: the clock can't be reached";
      end if;
      return Result;
   end Realtime;

   -------------------------
   -- Realtime_Is_Trusted --
   -------------------------

   function Realtime_Is_Trusted return Boolean is
      Result : constant Rose.Words.Word :=
                 Rose.Interfaces.Clock.Client.Realtime_Is_Trusted (Source);
   begin
      if Rose.Interfaces.Clock.Client.Has_Error then
         raise Program_Error with "Rose.Clock: the clock can't be reached";
      end if;
      return Result /= 0;
   end Realtime_Is_Trusted;

end Rose.Clock;
