with Rose.Devices.Port_IO;
with Rose.Words;

package body Clock.RTC is

   use Rose.Words;

   Seconds_Register : constant Word_8 := 16#00#;
   Minutes_Register : constant Word_8 := 16#02#;
   Hours_Register   : constant Word_8 := 16#04#;
   Weekday_Register : constant Word_8 := 16#06#;
   Day_Register     : constant Word_8 := 16#07#;
   Month_Register   : constant Word_8 := 16#08#;
   Year_Register    : constant Word_8 := 16#09#;
   Status_A         : constant Word_8 := 16#0A#;
   Status_B         : constant Word_8 := 16#0B#;
   Status_D         : constant Word_8 := 16#0D#;

   Update_In_Progress : constant Word_8 := 16#80#;
   --  Status A: the registers are changing, and can't be read.

   Set_Bit            : constant Word_8 := 16#80#;
   Hours_24           : constant Word_8 := 16#02#;
   Binary_Mode        : constant Word_8 := 16#04#;
   --  Status B: stop updating, 24 hour clock, and binary rather than BCD.

   Valid_Battery      : constant Word_8 := 16#80#;
   --  Status D: the RTC's RAM and time are valid.

   PM_Bit             : constant Word_8 := 16#80#;
   --  In the hours register in 12 hour mode.

   type Registers is
      record
         Second  : Word_8;
         Minute  : Word_8;
         Hour    : Word_8;
         Day     : Word_8;
         Month   : Word_8;
         Year    : Word_8;
      end record;

   function Read_Register (Index : Word_8) return Word_8;
   procedure Write_Register (Index : Word_8; Value : Word_8);

   procedure Read_Registers (Item : out Registers);
   --  Wait until the RTC isn't updating, and read until two reads agree.

   function From_BCD (Value : Word_8) return Natural;
   function To_BCD (Value : Natural) return Word_8;
   function Is_BCD (Value : Word_8) return Boolean;

   --------------
   -- From_BCD --
   --------------

   function From_BCD (Value : Word_8) return Natural
   is (Natural (Value / 16) * 10 + Natural (Value mod 16));

   ------------
   -- Is_BCD --
   ------------

   function Is_BCD (Value : Word_8) return Boolean
   is (Value / 16 <= 9 and then Value mod 16 <= 9);

   ----------
   -- Read --
   ----------

   procedure Read
     (Time  : out Clock.Calendar.Date_Time;
      Valid : out Boolean)
   is
      Raw    : Registers;
      B      : Word_8;
      Binary : Boolean;
      PM     : Boolean;
   begin
      Read_Registers (Raw);
      B := Read_Register (Status_B);
      Binary := (B and Binary_Mode) /= 0;

      Valid := (Read_Register (Status_D) and Valid_Battery) /= 0;

      --  The hours register has the PM bit in 12 hour mode, where 12 is
      --  the first hour of the day and of the afternoon.

      PM := (B and Hours_24) = 0 and then (Raw.Hour and PM_Bit) /= 0;
      Raw.Hour := Raw.Hour and not PM_Bit;

      if not Binary
        and then not (Is_BCD (Raw.Second) and then Is_BCD (Raw.Minute)
                      and then Is_BCD (Raw.Hour) and then Is_BCD (Raw.Day)
                      and then Is_BCD (Raw.Month) and then Is_BCD (Raw.Year))
      then
         Valid := False;
         Time := (2000, 1, 1, 0, 0, 0);
         return;
      end if;

      declare
         function Field (Value : Word_8) return Natural
         is (if Binary then Natural (Value) else From_BCD (Value));

         Hour : Natural := Field (Raw.Hour);
      begin
         if (B and Hours_24) = 0 then
            Hour := Hour mod 12 + (if PM then 12 else 0);
         end if;

         Time := (Year   => 2000 + Field (Raw.Year),
                  Month  => Field (Raw.Month),
                  Day    => Field (Raw.Day),
                  Hour   => Hour,
                  Minute => Field (Raw.Minute),
                  Second => Field (Raw.Second));
      end;

      if not Clock.Calendar.Is_Valid (Time) then
         Valid := False;
         Time := (2000, 1, 1, 0, 0, 0);
      end if;
   end Read;

   -------------------
   -- Read_Register --
   -------------------

   function Read_Register (Index : Word_8) return Word_8 is
   begin
      Rose.Devices.Port_IO.Port_Out_8 (RTC_Out_Cap, 0, Index);
      return Rose.Devices.Port_IO.Port_In_8 (RTC_In_Cap);
   end Read_Register;

   --------------------
   -- Read_Registers --
   --------------------

   procedure Read_Registers (Item : out Registers) is
      Again : Registers;

      procedure Get (Result : out Registers);

      procedure Get (Result : out Registers) is
      begin
         while (Read_Register (Status_A) and Update_In_Progress) /= 0 loop
            null;
         end loop;
         Result :=
           (Second => Read_Register (Seconds_Register),
            Minute => Read_Register (Minutes_Register),
            Hour   => Read_Register (Hours_Register),
            Day    => Read_Register (Day_Register),
            Month  => Read_Register (Month_Register),
            Year   => Read_Register (Year_Register));
      end Get;
   begin
      Get (Item);
      loop
         Get (Again);
         exit when Again = Item;
         Item := Again;
      end loop;
   end Read_Registers;

   ------------
   -- To_BCD --
   ------------

   function To_BCD (Value : Natural) return Word_8
   is (Word_8 (Value / 10) * 16 + Word_8 (Value mod 10));

   -----------
   -- Write --
   -----------

   procedure Write (Time : Clock.Calendar.Date_Time) is
      B       : constant Word_8 := Read_Register (Status_B);
      Binary  : constant Boolean := (B and Binary_Mode) /= 0;
      Weekday : constant Positive :=
                  Clock.Calendar.Day_Of_Week
                    (Clock.Calendar.To_Seconds (Time));

      function Field (Value : Natural) return Word_8
      is (if Binary then Word_8 (Value) else To_BCD (Value));
   begin

      --  Stop the RTC updating, and use a 24 hour clock, for as long as
      --  it takes to write the registers.

      Write_Register (Status_B, B or Set_Bit or Hours_24);
      Write_Register (Seconds_Register, Field (Time.Second));
      Write_Register (Minutes_Register, Field (Time.Minute));
      Write_Register (Hours_Register, Field (Time.Hour));
      Write_Register (Weekday_Register, Word_8 (Weekday));
      Write_Register (Day_Register, Field (Time.Day));
      Write_Register (Month_Register, Field (Time.Month));
      Write_Register (Year_Register, Field (Time.Year - 2000));
      Write_Register (Status_B, (B or Hours_24) and not Set_Bit);
   end Write;

   --------------------
   -- Write_Register --
   --------------------

   procedure Write_Register (Index : Word_8; Value : Word_8) is
   begin
      Rose.Devices.Port_IO.Port_Out_8 (RTC_Out_Cap, 0, Index);
      Rose.Devices.Port_IO.Port_Out_8 (RTC_Out_Cap, 1, Value);
   end Write_Register;

end Clock.RTC;
