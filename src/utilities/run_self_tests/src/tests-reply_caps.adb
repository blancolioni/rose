with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;

package body Tests.Reply_Caps is

   use type Rose.Capabilities.Capability;

   function Ask
     (Interface_Cap : Rose.Capabilities.Capability;
      Count         : Natural;
      Receive       : Natural;
      Received      : out Natural)
      return Rose.Capabilities.Capability;
   --  Ask the interface whose cap is Interface_Cap for its first Count
   --  caps, receiving Receive of them. Received is the number of caps
   --  that the reply says it brought. The result is the first of them,
   --  or the null cap if none came.

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   ---------
   -- Ask --
   ---------

   function Ask
     (Interface_Cap : Rose.Capabilities.Capability;
      Count         : Natural;
      Receive       : Natural;
      Received      : out Natural)
      return Rose.Capabilities.Capability
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Received := 0;
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Send_Word (Params, Natural (0));
      Rose.System_Calls.Send_Word (Params, Natural (0));
      Rose.System_Calls.Send_Word (Params, Count);
      Rose.System_Calls.Receive_Caps (Params, Receive);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return Rose.Capabilities.Null_Capability;
      end if;

      Received := Rose.Invocation.Sent_Cap_Count (Params);
      return (if Received > 0 then Params.Caps (0)
              else Rose.Capabilities.Null_Capability);
   end Ask;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": " & Message);
      return False;
   end Fail;

   ---------------------
   -- Reply_Caps_Test --
   ---------------------

   function Reply_Caps_Test return Boolean is
      use Rose.Capabilities;

      Bank     : constant Capability := Rose.System_Calls.Own_Bank.Cap;
      Fillers  : Capability_Array (1 .. 64) := (others => Null_Capability);
      Count    : Natural := 0;
      Run      : Natural := 0;
      Received : Natural;
      Base     : Capability;
      Asked    : Capability;
      Next     : Capability;
      Last     : Capability;

      function Finish (Result : Boolean) return Boolean;
      --  Delete the caps that the test made, and return Result.

      ------------
      -- Finish --
      ------------

      function Finish (Result : Boolean) return Boolean is
      begin
         for Cap of Fillers (1 .. Count) loop
            if Cap /= Null_Capability then
               Rose.System_Calls.Delete_Cap (Cap);
            end if;
         end loop;
         return Result;
      end Finish;

   begin
      Ada.Text_IO.Put ("Reply caps test");

      if Bank = Null_Capability then
         return Fail ("no bank of its own");
      end if;

      --  The test frees cap numbers, which it can't do without the
      --  Delete_Cap cap, as when petal runs it without that role.

      if Rose.System_Calls.Delete_Cap_Capability = Null_Capability then
         Ada.Text_IO.Put (" (skipped: no Delete_Cap cap)");
         return True;
      end if;

      --  Take the lowest free cap numbers until five in a row are
      --  consecutive. Then free those five: every number below the
      --  first of them is taken, and the five are free.

      while Run < 5 loop
         if Count = Fillers'Last then
            return Finish (Fail ("no run of five free cap numbers"));
         end if;

         Count := Count + 1;
         Fillers (Count) := Ask (Bank, 1, 1, Received);
         if Received /= 1 then
            return Finish (Fail ("the bank sent no cap"));
         end if;

         Run := (if Count > 1 and then Run > 0
                 and then Fillers (Count) = Fillers (Count - 1) + 1
                 then Run + 1 else 1);
      end loop;

      Base := Fillers (Count - 4);
      for Cap of Fillers (Count - 4 .. Count) loop
         Rose.System_Calls.Delete_Cap (Cap);
         Cap := Null_Capability;
      end loop;

      --  Asking for four caps but receiving one brings one cap, and
      --  leaves the next number free.

      Asked := Ask (Bank, 4, 1, Received);
      if Received /= 1 then
         Rose.System_Calls.Delete_Cap (Asked);
         return Finish
           (Fail ("received" & Received'Image & " caps, asked for 1"));
      end if;

      Next := Ask (Bank, 1, 1, Received);

      --  Receiving no caps brings none, and leaves the next number free.

      if Ask (Bank, 4, 0, Received) /= Null_Capability
        or else Received /= 0
      then
         return Finish (Fail ("received caps without asking for any"));
      end if;

      Last := Ask (Bank, 1, 1, Received);

      Rose.System_Calls.Delete_Cap (Asked);
      Rose.System_Calls.Delete_Cap (Next);
      Rose.System_Calls.Delete_Cap (Last);

      if Asked /= Base then
         return Finish (Fail ("the cap asked for is not the lowest free"));
      elsif Next /= Base + 1 then
         return Finish (Fail ("caps that were not asked for took numbers"));
      elsif Last /= Base + 2 then
         return Finish (Fail ("caps that were not received took numbers"));
      end if;

      return Finish (True);
   end Reply_Caps_Test;

end Tests.Reply_Caps;
