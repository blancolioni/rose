with Ada.Text_IO;

with Rose.Clock.Calendar;

procedure Rose.Core.Date
  (Clock_Service : in out Rose.Interfaces.Clock.Clock_Object'Class;
   ISO_8601      : Boolean := False)
is
   use Rose.Clock.Calendar;

   Day_Names   : constant array (Weekday) of String (1 .. 3) :=
                   ("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");
   Month_Names : constant array (1 .. 12) of String (1 .. 3) :=
                   ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   function Image
     (Value : Natural;
      Width : Positive;
      Pad   : Character := '0')
      return String;
   --  Value in Width characters, padded on the left with Pad.

   -----------
   -- Image --
   -----------

   function Image
     (Value : Natural;
      Width : Positive;
      Pad   : Character := '0')
      return String
   is
      Result : String (1 .. Width) := (others => Pad);
      Rest   : Natural := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Character'Val (Character'Pos ('0') + Rest mod 10);
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
      return Result;
   end Image;

begin
   Rose.Clock.Open (Clock_Service.Clock_Cap);

   declare
      Time : constant Date_Time := To_Date_Time (Rose.Clock.Realtime);
      Hms  : constant String :=
               Image (Time.Hour, 2) & ':' & Image (Time.Minute, 2) & ':'
               & Image (Time.Second, 2);
   begin
      if not Rose.Clock.Realtime_Is_Trusted then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "date: the clock has not been set, so the time may be wrong");
      end if;

      if ISO_8601 then
         Ada.Text_IO.Put_Line
           (Image (Time.Year, 4) & '-' & Image (Time.Month, 2) & '-'
            & Image (Time.Day, 2) & 'T' & Hms & 'Z');
      else
         Ada.Text_IO.Put_Line
           (Day_Names (Day_Of_Week (Time)) & ' ' & Month_Names (Time.Month)
            & ' ' & Image (Time.Day, 2, ' ') & ' ' & Hms & " UTC "
            & Image (Time.Year, 4));
      end if;
   end;
end Rose.Core.Date;
