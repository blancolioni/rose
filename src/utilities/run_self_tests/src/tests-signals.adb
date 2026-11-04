with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Server;
with Rose.System_Calls;

package body Tests.Signals is

   use type Rose.Capabilities.Capability;

   Signalled_Bit : constant := 9;
   Other_Bit     : constant := 10;

   Signalled_Count : Natural := 0;
   Other_Count     : Natural := 0;

   procedure On_Signalled_Bit;
   procedure On_Other_Bit;

   ------------------
   -- On_Other_Bit --
   ------------------

   procedure On_Other_Bit is
   begin
      Other_Count := Other_Count + 1;
   end On_Other_Bit;

   ----------------------
   -- On_Signalled_Bit --
   ----------------------

   procedure On_Signalled_Bit is
   begin
      Signalled_Count := Signalled_Count + 1;
   end On_Signalled_Bit;

   -----------------
   -- Signal_Test --
   -----------------

   function Signal_Test return Boolean is
      Context    : Rose.Server.Server_Context;
      Signal_Cap : Rose.Capabilities.Capability;
      Passed     : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         if not Ok then
            Ada.Text_IO.Put (": " & Name);
            Passed := False;
         end if;
      end Check;

   begin
      Ada.Text_IO.Put ("Signal test");

      if Rose.System_Calls.Create_Signal_Capability
        = Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put (" (skipped: no Create_Signal cap)");
         return True;
      end if;

      Check ("a cap for a named signal's bit is refused",
             Rose.System_Calls.Create_Signal_Cap (3)
             = Rose.Capabilities.Null_Capability);

      Signal_Cap := Rose.System_Calls.Create_Signal_Cap (Signalled_Bit);
      Check ("no signal cap", Signal_Cap /= Rose.Capabilities.Null_Capability);
      if not Passed then
         return False;
      end if;

      Rose.Server.Set_Signal_Handler
        (Context, Signalled_Bit, On_Signalled_Bit'Access);
      Rose.Server.Set_Signal_Handler
        (Context, Other_Bit, On_Other_Bit'Access);

      --  Nothing is receiving yet, so each invocation sets the bit and
      --  goes on: the second finds it set already.

      for I in 1 .. 2 loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Signal_Cap);
            Rose.System_Calls.Invoke_Capability (Params);
            Check ("invoking the signal cap failed",
                   not Params.Control.Flags (Rose.Invocation.Error));
         end;
      end loop;

      --  The next receive is the signal.

      Rose.Server.Receive_Message (Context);
      Check ("the signalled bit's handler was called"
             & Natural'Image (Signalled_Count) & " times",
             Signalled_Count = 1);
      Check ("another bit's handler was called", Other_Count = 0);

      Rose.System_Calls.Delete_Cap (Signal_Cap);
      return Passed;
   end Signal_Test;

end Tests.Signals;
