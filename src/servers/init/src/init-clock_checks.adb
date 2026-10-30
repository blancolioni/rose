with Rose.Words;

with Rose.Interfaces.Clock.Client;

with Init.Boot_Steps;
with Init.Calls;

package body Init.Clock_Checks is

   use Rose.Words;

   NL : constant Character := Character'Val (10);

   Wait_Milliseconds : constant := 2_000;

   Min_Delta : constant Word_64 := 1_900_000_000;
   Max_Delta : constant Word_64 := 3_000_000_000;
   --  The readings, in nanoseconds, have to be this far apart.

   First_Second_2000 : constant Word_64 := 946_684_800_000_000_000;
   First_Second_2100 : constant Word_64 := 4_102_444_800_000_000_000;

   procedure Report (Label : String; First, Second : Word_64);
   --  Write Label and the two readings on the console, as sixteen
   --  hexadecimal digits each. The host reads these as the time.

   ------------
   -- Report --
   ------------

   procedure Report (Label : String; First, Second : Word_64) is
      Digits_16 : constant String := "0123456789abcdef";
      Line      : String (1 .. 64);
      Last      : Natural := 0;

      procedure Append (Text : String);
      procedure Append_Hex (Value : Word_64);

      procedure Append (Text : String) is
      begin
         for Ch of Text loop
            Last := Last + 1;
            Line (Last) := Ch;
         end loop;
      end Append;

      procedure Append_Hex (Value : Word_64) is
         Rest : Word_64 := Value;
         Text : String (1 .. 16);
      begin
         for I in reverse Text'Range loop
            Text (I) := Digits_16 (Natural (Rest mod 16) + 1);
            Rest := Rest / 16;
         end loop;
         Append (Text);
      end Append_Hex;
   begin
      Append (Label);
      Append_Hex (First);
      Append (" ");
      Append_Hex (Second);
      Append ((1 => NL));
      Init.Calls.Send_String (Line (1 .. Last));
   end Report;

   ---------
   -- Run --
   ---------

   function Run (Clock : Rose.Capabilities.Capability) return Boolean is
      Client : Rose.Interfaces.Clock.Client.Clock_Client;
      Ok     : Boolean;
   begin
      Rose.Interfaces.Clock.Client.Open (Client, Clock);

      declare
         Mono_1  : constant Word_64 :=
                     Rose.Interfaces.Clock.Client.Monotonic (Client);
         Real_1  : constant Word_64 :=
                     Rose.Interfaces.Clock.Client.Realtime (Client);
         Trusted : constant Word :=
                     Rose.Interfaces.Clock.Client.Realtime_Is_Trusted
                       (Client);
      begin
         Init.Boot_Steps.Wait (Wait_Milliseconds);

         declare
            Mono_2 : constant Word_64 :=
                       Rose.Interfaces.Clock.Client.Monotonic (Client);
            Real_2 : constant Word_64 :=
                       Rose.Interfaces.Clock.Client.Realtime (Client);
         begin
            Report ("init: clock: monotonic ", Mono_1, Mono_2);
            Report ("init: clock: realtime  ", Real_1, Real_2);
            Report ("init: clock: trusted   ", Word_64 (Trusted), 0);

            Ok := Mono_2 - Mono_1 in Min_Delta .. Max_Delta
              and then Real_2 - Real_1 in Min_Delta .. Max_Delta
              and then Real_1 in First_Second_2000 .. First_Second_2100;
         end;
      end;

      if Ok then
         Init.Calls.Send_String ("init: clock: ok" & NL);
      else
         Init.Calls.Send_String ("init: clock: FAILED" & NL);
      end if;
      return Ok;
   end Run;

end Init.Clock_Checks;
