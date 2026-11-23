with Ada.Streams;

with Rose.Invocation;
with Rose.Server;
with Rose.System_Calls.Own_Bank;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Caps.Cap;
with Rose.Interfaces.Commands.Lookup.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Launch;
with Rose.Interfaces.Processes.Restorable.Server;
with Rose.Interfaces.Screens.Supervisor.Server;
with Rose.Interfaces.Screens.Terminal.Client;
with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Time.Timeout;
with Rose.Interfaces.Time.Timer.Client;
with Rose.Interfaces.Users.User.Client;

package body Supervisor_Loop is

   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word;

   LF : constant Character := Character'Val (10);

   Completed : constant Rose.Words.Word := 0;
   --  The first word of a command's reply when it has completed.

   Retry_Delay : constant Rose.Words.Word := 1_000;
   --  Milliseconds to wait before launching login again, after a launch
   --  or a call that failed, as the call to a login does after a
   --  restore.

   Refused_Delay : constant Rose.Words.Word := 3_000;
   --  Milliseconds that a refused login's message stays on the screen.

   Context : Rose.Server.Server_Context;

   Started : Boolean := False;
   --  Restored or Start has come: login runs from now on.

   Console  : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
   Commands : Rose.Interfaces.Commands.Lookup.Client.Lookup_Client;
   Timer    : Rose.Interfaces.Time.Timer.Client.Timer_Client;

   Timer_Interface_Cap  : Rose.Capabilities.Capability := 0;
   Clock_Interface_Cap  : Rose.Capabilities.Capability := 0;
   Rights_Interface_Cap : Rose.Capabilities.Capability := 0;
   System_Interface_Cap : Rose.Capabilities.Capability := 0;
   Auth_Interface_Cap   : Rose.Capabilities.Capability := 0;
   --  Passed on to login and the sessions.

   Timeout_Receive_Cap : Rose.Capabilities.Capability := 0;
   Timeout_Send_Cap    : Rose.Capabilities.Capability := 0;
   --  The endpoint that the timer calls when a wait is over.

   type Restorable_Object is
     limited new Rose.Interfaces.Processes.Restorable.Server
       .Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Restorable_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Processes.Restorable.Server.Cap_Of (Context, Item));

   overriding procedure Restored (Item : in out Restorable_Object);

   type Supervisor_Object is
     limited new Rose.Interfaces.Screens.Supervisor.Server
       .Supervisor_Interface
   with null record;

   overriding function Supervisor_Cap
     (Item : in out Supervisor_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Screens.Supervisor.Server.Cap_Of (Context, Item));

   overriding procedure Start (Item : in out Supervisor_Object);

   The_Restorable : aliased Restorable_Object;
   The_Supervisor : aliased Supervisor_Object;

   procedure Put (Text : String);
   --  Write Text to the console.

   function Run_Login return Rose.Capabilities.Capability;
   --  Launch login, and wait for it to give a user. The user's User
   --  interface, or null if the launch failed, or the call did, as it
   --  does when a restore has ended the transient login.

   procedure Run_Session (User_Cap : Rose.Capabilities.Capability);
   --  Run the session of the user whose User interface is User_Cap,
   --  and return when it ends.

   procedure Wait (Milliseconds : Rose.Words.Word);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Console  : Rose.Capabilities.Capability;
      Commands : Rose.Capabilities.Capability;
      Timer    : Rose.Capabilities.Capability;
      Clock    : Rose.Capabilities.Capability;
      Rights   : Rose.Capabilities.Capability;
      System   : Rose.Capabilities.Capability;
      Auth     : Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability)
   is
   begin
      Rose.Interfaces.Screens.Terminal.Client.Open
        (Supervisor_Loop.Console, Console);
      Rose.Interfaces.Commands.Lookup.Client.Open
        (Supervisor_Loop.Commands, Commands);
      Rose.Interfaces.Time.Timer.Client.Open (Supervisor_Loop.Timer, Timer);
      Timer_Interface_Cap := Timer;
      Clock_Interface_Cap := Clock;
      Rights_Interface_Cap := Rights;
      System_Interface_Cap := System;
      Auth_Interface_Cap := Auth;

      Rose.Interfaces.Processes.Restorable.Server.Attach
        (Context, The_Restorable'Access);
      Rose.Interfaces.Screens.Supervisor.Server.Attach
        (Context, The_Supervisor'Access);
      Control :=
        Rose.Interfaces.Screens.Supervisor.Server.Cap_Of
          (Context, The_Supervisor);
   end Create_Server;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
      use Ada.Streams;
      Buffer : Stream_Element_Array (1 .. Text'Length);
      for Buffer'Address use Text'Address;
      pragma Import (Ada, Buffer);
   begin
      Console.Write (Buffer);
   end Put;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Restorable_Object) is
      pragma Unreferenced (Item);
   begin
      Started := True;
   end Restored;

   ---------------
   -- Run_Login --
   ---------------

   function Run_Login return Rose.Capabilities.Capability is
      Launcher : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                   Commands.Find ("login");
      Endpoint : constant Rose.Capabilities.Capability :=
                   Launcher.Launch_Command (Rose.System_Calls.Own_Bank.Cap);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  Login's roles' caps and its Auth argument, as
      --  src/utilities/login/rose-core-login.idl declares them. It is
      --  launched against this process's own bank, so it ends with the
      --  supervisor. The call completes with the User interface of the
      --  user who logged in.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Caps
        (Params,
         (Console.Terminal_Cap,
          Rose.System_Calls.Create_Endpoint_Capability,
          Timer_Interface_Cap,
          Auth_Interface_Cap));
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      Rose.System_Calls.Delete_Cap (Endpoint);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Params.Caps (0);

   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end Run_Login;

   -----------------
   -- Run_Session --
   -----------------

   procedure Run_Session (User_Cap : Rose.Capabilities.Capability) is
      User : Rose.Interfaces.Users.User.Client.User_Client;
   begin
      Rose.Interfaces.Users.User.Client.Open (User, User_Cap);

      declare

         --  The program the user starts with (#525). A user server that
         --  took over from a version without one has none, and the login
         --  is refused.

         First : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                   User.First_Process;

         --  The session's bank (ADR 0014, section 2; #450), a sub-bank of
         --  the user's. The session is launched against it, and so its
         --  jobs are in it too.

         Session  : Rose.Interfaces.Storage.Bank.Bank_Object'Class :=
                      User.New_Session;
         Endpoint : constant Rose.Capabilities.Capability :=
                      First.Launch_Command (Session.Bank_Cap);
         Params   : aliased Rose.Invocation.Invocation_Record;
      begin
         if Endpoint = Rose.Capabilities.Null_Capability then
            Session.Destroy;
            Put ("Cannot start the session" & LF);
            Wait (Refused_Delay);
            Rose.System_Calls.Delete_Cap (User_Cap);
            return;
         end if;

         --  Petal's roles' caps and its User argument, in the order of
         --  its command declaration (src/utilities/petal/
         --  rose-core-petal.idl): petal is every user's first program for
         --  now, and another will need its own. A standard stream is the
         --  cap of its Read or Write, which the run-time system invokes
         --  directly (Rose.Standard_Streams). Petal's Delete_Cap role is
         --  retired (#1022, #1023), so it is given the null cap. The call
         --  returns when petal logs out (#458), and waits through a
         --  restore (#519).

         Rose.System_Calls.Initialize_Send (Params, Endpoint);
         Rose.System_Calls.Send_Caps
           (Params,
            (Console.Get_Read_Cap,
             Console.Get_Write_Cap,
             Console.Get_Write_Cap,
             Rose.System_Calls.Create_Endpoint_Capability,
             Rose.Capabilities.Null_Capability,
             Rose.System_Calls.Rescind_Cap_Capability,
             Commands.Lookup_Cap,
             Console.Terminal_Cap,
             Clock_Interface_Cap,
             Rights_Interface_Cap,
             System_Interface_Cap,
             User_Cap));
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);

         Rose.System_Calls.Delete_Cap (Endpoint);

         --  Ending the session destroys its bank: whatever the session
         --  left running ends, and what it reserved is released.

         Session.Destroy;
      end;

      Rose.System_Calls.Delete_Cap (User_Cap);

      --  Whatever the session left, login starts with line editing and
      --  echo on, and plain text (#734).

      Console.Enable_Line_Editing;
      Console.Enable_Echo;
      Console.Set_Attributes (False, False);

   exception
      when Rose.Interfaces.Objects.Invocation_Error =>
         Rose.System_Calls.Delete_Cap (User_Cap);
         Console.Enable_Line_Editing;
         Console.Enable_Echo;
         Put ("Login refused: the user has no program to start" & LF);
         Wait (Refused_Delay);
   end Run_Session;

   -----------
   -- Start --
   -----------

   overriding procedure Start (Item : in out Supervisor_Object) is
      pragma Unreferenced (Item);
   begin
      Started := True;
   end Start;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin

      --  Nothing is launched before the restart at the end of an
      --  install, which would read the console too (#456, #512). The
      --  restored supervisor is told Restored; one that is never
      --  restored is told Start.

      while not Started loop
         Rose.Server.Receive_Message (Context);
      end loop;

      --  From now on, the supervisor is always in a call: to login, or
      --  to the session. It never receives again.

      loop
         declare
            User_Cap : constant Rose.Capabilities.Capability := Run_Login;
         begin
            if User_Cap = Rose.Capabilities.Null_Capability then
               Wait (Retry_Delay);
            else
               Run_Session (User_Cap);
            end if;
         end;
      end loop;
   end Start_Server;

   ----------
   -- Wait --
   ----------

   procedure Wait (Milliseconds : Rose.Words.Word) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Timeout_Receive_Cap = Rose.Capabilities.Null_Capability then
         Timeout_Receive_Cap :=
           Rose.System_Calls.Server.Create_Receive_Cap
             (Rose.System_Calls.Create_Endpoint_Capability);
         Timeout_Send_Cap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Rose.System_Calls.Create_Endpoint_Capability,
              Rose.Interfaces.Time.Timeout.On_Timeout_Endpoint);
      end if;

      --  The timer calls the endpoint when the time is up, without
      --  waiting, so the supervisor is already receiving on it by then.

      declare
         Handle : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                    Timer.Set_Timer (Milliseconds, Timeout_Send_Cap)
           with Unreferenced;
      begin
         Rose.System_Calls.Initialize_Receive (Params, Timeout_Receive_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Wait;

end Supervisor_Loop;
