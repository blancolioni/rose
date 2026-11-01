with Ada.Text_IO;

with Rose.Clock.Calendar;
with Rose.Interfaces.Objects;

procedure Rose.Core.Set_Date
  (Clock_Admin_Service : in out
     Rose.Interfaces.Clock_Admin.Clock_Admin_Object'Class;
   Time                : String)
is
   use Rose.Clock.Calendar;

   First_Year : constant := 2000;
   Last_Year  : constant := 2099;
   --  What the RTC can hold, which Clock_Admin.Set_Realtime checks too.

   procedure Fail (Reason : String);
   --  Report that the time cannot be set, and why, on standard error, and
   --  fail the launch call.

   function Number
     (First : Positive;
      Last  : Positive)
      return Natural;
   --  The decimal number in Time (First .. Last), or Natural'Last if
   --  there is a character in it that isn't a digit.

   ----------
   -- Fail --
   ----------

   procedure Fail (Reason : String) is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "set_date: cannot set the time to '" & Time & "': " & Reason);
      Ada.Text_IO.Flush (Ada.Text_IO.Standard_Error);

      --  An unhandled exception fails the launch call (see
      --  Rose.Command_Line.Launch).

      raise Constraint_Error;
   end Fail;

   ------------
   -- Number --
   ------------

   function Number
     (First : Positive;
      Last  : Positive)
      return Natural
   is
      Result : Natural := 0;
   begin
      for I in First .. Last loop
         if Time (I) not in '0' .. '9' then
            return Natural'Last;
         end if;
         Result := Result * 10 + (Character'Pos (Time (I)) - 48);
      end loop;
      return Result;
   end Number;

begin
   --  YYYY-MM-DDTHH:MM:SSZ

   if Time'Length /= 20
     or else Time'First /= 1
     or else Time (5) /= '-' or else Time (8) /= '-'
     or else Time (11) /= 'T'
     or else Time (14) /= ':' or else Time (17) /= ':'
     or else Time (20) /= 'Z'
   then
      Fail ("the form is 2026-10-30T18:12:03Z");
   end if;

   declare
      Year   : constant Natural := Number (1, 4);
      Month  : constant Natural := Number (6, 7);
      Day    : constant Natural := Number (9, 10);
      Hour   : constant Natural := Number (12, 13);
      Minute : constant Natural := Number (15, 16);
      Second : constant Natural := Number (18, 19);
   begin
      if Year not in First_Year .. Last_Year then
         Fail ("the year has to be from 2000 to 2099");
      elsif Month not in 1 .. 12
        or else Day not in 1 .. 31
        or else Hour > 23
        or else Minute > 59
        or else Second > 59
      then
         Fail ("not a date and time");
      end if;

      declare
         Parsed : constant Date_Time :=
           (Year       => Year,
            Month      => Month,
            Day        => Day,
            Hour       => Hour,
            Minute     => Minute,
            Second     => Second,
            Nanosecond => 0);
      begin
         if not Is_Valid (Parsed) then
            Fail ("there is no such day");
         end if;

         begin
            Clock_Admin_Service.Set_Realtime (To_Nanoseconds (Parsed));
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Fail ("the clock refused it");
         end;
      end;
   end;
end Rose.Core.Set_Date;
