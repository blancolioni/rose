with Rose.Capabilities;
with Rose.Invocation;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Clock_Admin.Server;
with Rose.Interfaces.Restorable.Server;

with Clock.Calendar;
with Clock.RTC;

package body Clock.Server is

   use Rose.Words;

   --  One object serves Clock_Admin, and Clock as its ancestor, and
   --  takes the restore notice (ADR 0004). Its state is the package's,
   --  so that a checkpoint holds it with the process's memory.

   type Clock_Object is
     new Rose.Interfaces.Clock_Admin.Server.Clock_Admin_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding function Clock_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability;

   overriding function Clock_Admin_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability;

   overriding function Monotonic
     (Item : in out Clock_Object)
      return Rose.Words.Word_64;

   overriding function Realtime
     (Item : in out Clock_Object)
      return Rose.Words.Word_64;

   overriding function Realtime_Is_Trusted
     (Item : in out Clock_Object)
      return Boolean;

   overriding procedure Set_Realtime
     (Item        : in out Clock_Object;
      Nanoseconds : Rose.Words.Word_64);

   overriding procedure Restored (Item : in out Clock_Object);

   Context   : Rose.Server.Server_Context;
   The_Clock : aliased Clock_Object;

   Tick_Nanoseconds : constant Word_64 := 10_000_150;
   --  The PIT's divisor is 11932, so a tick is 11932 / 1193181.8 s.
   --  Counting 10 ms would lose 15 parts per million.

   Second           : constant Word_64 := 1_000_000_000;

   Edge_Timeout     : constant Word_64 := 150;
   --  Ticks that the RTC has to change its second in, if it is running.

   --  Monotonic = Base_Mono + (ticks - Base_Ticks) * Tick_Nanoseconds,
   --  and Realtime = Monotonic + Realtime_Offset. Last_Mono and
   --  Last_Real are the latest readings returned, which a restore
   --  carries on from.

   Base_Ticks      : Word_64 := 0;
   Base_Mono       : Word_64 := 0;
   Realtime_Offset : Word_64 := 0;
   Last_Mono       : Word_64 := 0;
   Last_Real       : Word_64 := 0;
   Trusted         : Boolean := False;

   function Ticks return Word_64;
   --  The kernel's tick count since this boot.

   function Raw_Monotonic return Word_64;
   --  Monotonic, without recording it in Last_Mono.

   procedure Find_Second_Edge
     (Real  : out Word_64;
      Mono  : out Word_64;
      Valid : out Boolean);
   --  Wait until the RTC changes its second. Real is the RTC's time at
   --  that moment, and Mono is the monotonic time. If the RTC is not
   --  valid, or isn't running, Valid is False, and Real and Mono are as
   --  good as there is.

   procedure Divide
     (Value     : Word_64;
      Quotient  : out Word_64;
      Remainder : out Word_64);
   --  Divide by a billion. Without a 64-bit division, which the run-time
   --  system doesn't have.

   procedure Initialize;
   --  The first start: there is no earlier time to continue from.

   ---------------------
   -- Clock_Admin_Cap --
   ---------------------

   overriding function Clock_Admin_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Clock_Admin.Server.Cap_Of (Item));

   ---------------
   -- Clock_Cap --
   ---------------

   overriding function Clock_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Clock_Admin.Server.Cap_Of
         (Item, Rose.Interfaces.Clock_Admin.Clock_Interface));

   ------------
   -- Divide --
   ------------

   procedure Divide
     (Value     : Word_64;
      Quotient  : out Word_64;
      Remainder : out Word_64)
   is
      Mask : Word_64 := 2 ** 63;
   begin
      Quotient := 0;
      Remainder := 0;
      while Mask /= 0 loop
         Remainder := Remainder * 2 + (if (Value and Mask) /= 0 then 1 else 0);
         Quotient := Quotient * 2;
         if Remainder >= Second then
            Remainder := Remainder - Second;
            Quotient := Quotient + 1;
         end if;
         Mask := Mask / 2;
      end loop;
   end Divide;

   ----------------------
   -- Find_Second_Edge --
   ----------------------

   procedure Find_Second_Edge
     (Real  : out Word_64;
      Mono  : out Word_64;
      Valid : out Boolean)
   is
      First : Clock.Calendar.Date_Time;
      Time  : Clock.Calendar.Date_Time;
      Now   : Boolean;
      Start : constant Word_64 := Ticks;
      use type Clock.Calendar.Date_Time;
   begin
      Clock.RTC.Read (First, Valid);
      Time := First;

      if Valid then
         loop
            Clock.RTC.Read (Time, Now);
            exit when Time /= First or else not Now
              or else Ticks - Start > Edge_Timeout;
         end loop;
         Valid := Now and then Time /= First;
      end if;

      Mono := Raw_Monotonic;
      Real := Word_64 (Clock.Calendar.To_Seconds (Time)) * Second;
   end Find_Second_Edge;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Real  : Word_64;
      Mono  : Word_64;
      Valid : Boolean;
   begin
      Base_Ticks := Ticks;
      Base_Mono  := 0;
      Last_Mono  := 0;
      Find_Second_Edge (Real, Mono, Valid);
      Realtime_Offset := Real - Mono;
      Last_Real := Real;
      Trusted := Valid;
   end Initialize;

   ---------------
   -- Monotonic --
   ---------------

   overriding function Monotonic
     (Item : in out Clock_Object)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Item);
      Now : constant Word_64 := Raw_Monotonic;
   begin
      if Now > Last_Mono then
         Last_Mono := Now;
      end if;
      return Now;
   end Monotonic;

   -------------------
   -- Raw_Monotonic --
   -------------------

   function Raw_Monotonic return Word_64
   is (Base_Mono + (Ticks - Base_Ticks) * Tick_Nanoseconds);

   --------------
   -- Realtime --
   --------------

   overriding function Realtime
     (Item : in out Clock_Object)
      return Rose.Words.Word_64
   is
      pragma Unreferenced (Item);
      Mono : constant Word_64 := Raw_Monotonic;
      Real : constant Word_64 := Mono + Realtime_Offset;
   begin
      if Mono > Last_Mono then
         Last_Mono := Mono;
      end if;
      if Real > Last_Real then
         Last_Real := Real;
      end if;
      return Real;
   end Realtime;

   -------------------------
   -- Realtime_Is_Trusted --
   -------------------------

   overriding function Realtime_Is_Trusted
     (Item : in out Clock_Object)
      return Boolean
   is
      pragma Unreferenced (Item);
   begin
      return Trusted;
   end Realtime_Is_Trusted;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Clock_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Clock_Object) is
      pragma Unreferenced (Item);
      Real  : Word_64;
      Mono  : Word_64;
      Valid : Boolean;
   begin

      --  The tick count has started again at zero, and the checkpoint's
      --  readings are the latest that anything restored can hold. Carry
      --  on from them, whatever the RTC says (ADR 0020 section 4).

      Base_Ticks := Ticks;
      Base_Mono  := Last_Mono;
      Find_Second_Edge (Real, Mono, Valid);

      --  The RTC being a second or more behind the last reading means
      --  that it is wrong, or that the disk was copied to a machine with
      --  an earlier clock. Either way the clock runs on from the last
      --  reading, and says that it isn't to be trusted.

      Trusted := Valid and then Real + Second >= Last_Real;
      if Real < Last_Real then
         Real := Last_Real;
      end if;
      Realtime_Offset := Real - Mono;
   end Restored;

   ------------------
   -- Set_Realtime --
   ------------------

   overriding procedure Set_Realtime
     (Item        : in out Clock_Object;
      Nanoseconds : Rose.Words.Word_64)
   is
      pragma Unreferenced (Item);
      Seconds : Word_64;
      Rest    : Word_64;
   begin
      Divide (Nanoseconds, Seconds, Rest);

      if Seconds < Word_64 (Clock.Calendar.First_Second)
        or else Seconds >= Word_64 (Clock.Calendar.Last_Second)
      then
         raise Constraint_Error with "the RTC can't hold that time";
      end if;

      --  The RTC has whole seconds, so wait until the clock reaches the
      --  next one, and write that.

      declare
         Now      : constant Word_64 := Raw_Monotonic;
         Wait     : constant Word_64 :=
                      (if Rest = 0 then 0 else Second - Rest);
         Boundary : constant Word_64 := Now + Wait;
         Written  : constant Word_32 :=
                      Word_32 (if Rest = 0 then Seconds else Seconds + 1);
      begin
         while Raw_Monotonic < Boundary loop
            null;
         end loop;

         Clock.RTC.Write (Clock.Calendar.To_Date_Time (Written));
         Realtime_Offset := Nanoseconds - Now;
         Last_Real := Nanoseconds;
         Trusted := True;
      end;
   end Set_Realtime;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Interfaces.Clock_Admin.Server.Publish (Context, The_Clock'Access);
      Rose.Interfaces.Restorable.Server.Attach (Context, The_Clock'Access);

      --  Callers wait for the server while it finds the edge of the RTC's
      --  second, which takes up to a second.

      Initialize;
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------
   -- Ticks --
   -----------

   function Ticks return Word_64 is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Clock.Ticks_Cap);
      Rose.System_Calls.Receive_Words (Params, 2);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         raise Program_Error with "clock: no tick count";
      end if;
      return Word_64 (Params.Data (0))
        + Word_64 (Params.Data (1)) * 2 ** 32;
   end Ticks;

end Clock.Server;
