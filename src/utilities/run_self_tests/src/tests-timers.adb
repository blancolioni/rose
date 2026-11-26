with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Clock;
with Rose.Invocation;
with Rose.Objects;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Caps.Cap;
with Rose.Interfaces.Time.Timeout;
with Rose.Interfaces.Time.Timer.Client;

with Tests.Registry;

package body Tests.Timers is

   use Rose.Words;
   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Capability_Identifier;
   use type Rose.Objects.Endpoint_Id;

   Tick : constant Word_64 := 10_000_000;
   --  Nanoseconds in a tick, near enough. The timer counts in ticks, and
   --  a timer can fire up to a tick before its time in milliseconds.

   Late_Id      : constant Rose.Objects.Capability_Identifier := 1;
   Early_Id     : constant Rose.Objects.Capability_Identifier := 2;
   Destroyed_Id : constant Rose.Objects.Capability_Identifier := 3;
   Due_Id       : constant Rose.Objects.Capability_Identifier := 4;

   Late_Time      : constant Word_64 := 400;
   Early_Time     : constant Word_64 := 150;
   Destroyed_Time : constant Word_64 := 50;
   --  Milliseconds. They are far apart: the timer calls a timeout cap
   --  without waiting, so a timer fires into nothing unless its caller
   --  is already receiving.

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   ----------------
   -- Timer_Test --
   ----------------

   function Timer_Test return Boolean is
      Timer       : Rose.Interfaces.Time.Timer.Client.Timer_Client;
      Receive_Cap : Rose.Capabilities.Capability;
      Start       : Word_64;
      Sent        : array (Late_Id .. Due_Id) of Rose.Capabilities.Capability;

      procedure Wait
        (Id      : out Rose.Objects.Capability_Identifier;
         Elapsed : out Word_64);
      --  Receive the next timeout, and return the identifier of the cap
      --  the timer called, and the nanoseconds since Start.

      function Too_Soon
        (Elapsed      : Word_64;
         Milliseconds : Word_64)
         return Boolean
      is (Elapsed + 2 * Tick < Milliseconds * 1_000_000);
      --  True if a timer of Milliseconds fired at Elapsed, more than the
      --  timer's rounding before its time.

      procedure Delete_Caps;
      --  Delete the caps the test made.

      -----------------
      -- Delete_Caps --
      -----------------

      procedure Delete_Caps is
      begin
         for Cap of Sent loop
            Rose.System_Calls.Delete_Cap (Cap);
         end loop;
         Rose.System_Calls.Delete_Cap (Receive_Cap);
      end Delete_Caps;

      ----------
      -- Wait --
      ----------

      procedure Wait
        (Id      : out Rose.Objects.Capability_Identifier;
         Elapsed : out Word_64)
      is
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
         Elapsed := Rose.Clock.Monotonic - Start;
         if Params.Control.Flags (Rose.Invocation.Error)
           or else Params.Endpoint
             /= Rose.Interfaces.Time.Timeout.On_Timeout_Endpoint
         then
            Id := 0;
         else
            Id := Params.Identifier;
         end if;
      end Wait;

      Id      : Rose.Objects.Capability_Identifier;
      Elapsed : Word_64;

   begin
      Ada.Text_IO.Put ("Timer test");

      if Tests.Registry.Timer = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no timer)");
         return True;
      elsif Tests.Registry.Clock = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no clock)");
         return True;
      end if;

      Rose.Clock.Open (Tests.Registry.Clock);
      Rose.Interfaces.Time.Timer.Client.Open (Timer, Tests.Registry.Timer);

      Receive_Cap :=
        Rose.System_Calls.Server.Create_Receive_Cap
          (Tests.Registry.Create_Endpoint);
      for I in Sent'Range loop
         Sent (I) :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap  => Tests.Registry.Create_Endpoint,
              Endpoint_Id => Rose.Interfaces.Time.Timeout.On_Timeout_Endpoint,
              Identifier  => I);
      end loop;

      Start := Rose.Clock.Monotonic;

      --  The early timer comes before the late one, which the timer told
      --  the kernel about first. The destroyed one comes before both, so
      --  the kernel's timeout for it arrives with no timer due: before
      --  #1014, that fired the early timer about 100 ms early.

      declare
         Late      : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                       Timer.Set_Timer (Word (Late_Time), Sent (Late_Id))
           with Unreferenced;
         Early     : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                       Timer.Set_Timer (Word (Early_Time), Sent (Early_Id))
           with Unreferenced;
         Destroyed : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                       Timer.Set_Timer
                         (Word (Destroyed_Time), Sent (Destroyed_Id));
      begin
         Destroyed.Destroy;
      end;

      Wait (Id, Elapsed);
      if Id /= Early_Id then
         Delete_Caps;
         return Fail ("the early timer didn't fire first");
      elsif Too_Soon (Elapsed, Early_Time) then
         Delete_Caps;
         return Fail ("the early timer fired too soon");
      end if;

      --  A timer of no time is due as it is set. Before #1014, the timer
      --  asked the kernel for a timeout of no ticks, which never comes,
      --  and no timer fired again.

      declare
         Due : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                 Timer.Set_Timer (0, Sent (Due_Id))
           with Unreferenced;
         Set_At : constant Word_64 := Rose.Clock.Monotonic - Start;
      begin
         Wait (Id, Elapsed);
         if Id /= Due_Id then
            Delete_Caps;
            return Fail ("the timer that was due didn't fire next");
         elsif Elapsed - Set_At > 10 * Tick then
            Delete_Caps;
            return Fail ("the timer that was due was late");
         end if;
      end;

      Wait (Id, Elapsed);
      Delete_Caps;

      if Id /= Late_Id then
         return Fail ("the late timer didn't fire last");
      elsif Too_Soon (Elapsed, Late_Time) then
         return Fail ("the late timer fired too soon");
      end if;

      return True;
   end Timer_Test;

end Tests.Timers;
