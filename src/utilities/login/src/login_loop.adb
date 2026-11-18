with Ada.Exceptions;

with Ada.Streams;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Build;
with Rose.Version;
with Rose.Words;

with Rose.Interfaces.Users.Auth.Client;
with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Caps.Cap;
with Rose.Interfaces.Commands.Lookup.Client;
with Rose.Interfaces.Processes.Launch;
with Rose.Interfaces.Screens.Terminal.Client;
with Rose.Interfaces.Time.Timeout;
with Rose.Interfaces.Time.Timer.Client;
with Rose.Interfaces.Users.User;

package body Login_Loop is

   use Ada.Streams;
   use type Rose.Capabilities.Capability;

   LF  : constant Character := Character'Val (10);

   Failure_Delay : constant Rose.Words.Word := 3_000;
   --  Milliseconds to wait after a wrong name or password.

   Console  : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
   Commands : Rose.Interfaces.Commands.Lookup.Client.Lookup_Client;
   Timer    : Rose.Interfaces.Time.Timer.Client.Timer_Client;
   Auth     : Rose.Interfaces.Users.Auth.Client.Auth_Client;

   Clock_Interface_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   --  The clock's Clock interface, for petal.

   Rights_Interface_Cap : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;
   --  The rights broker's Rights interface, for petal.

   Pending      : Stream_Element_Array (1 .. 4096);
   Pending_Last : Stream_Element_Offset := 0;
   --  What the console has given that Get_Line hasn't returned yet.

   Timeout_Receive_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Timeout_Send_Cap    : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   --  The endpoint that the timer calls when a wait is over.

   procedure Get_Line
     (Line : out String;
      Last : out Natural);
   --  The next line typed at the console, without its line break. An end
   --  of file gives an empty line.

   procedure Put (Text : String);
   --  Write Text to the console.

   procedure Run_Petal
     (User : in out Rose.Interfaces.Users.User.User_Object'Class);
   --  Run petal for User, and return when it logs out.

   procedure Wait (Milliseconds : Rose.Words.Word);

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (Line : out String;
      Last : out Natural)
   is
   begin
      Last := Line'First - 1;

      loop
         for I in 1 .. Pending_Last loop
            if Pending (I) = Character'Pos (LF) then
               for J in 1 .. I - 1 loop
                  if Last < Line'Last then
                     Last := Last + 1;
                     Line (Last) := Character'Val (Pending (J));
                  end if;
               end loop;
               Pending (1 .. Pending_Last - I) :=
                 Pending (I + 1 .. Pending_Last);
               Pending_Last := Pending_Last - I;
               return;
            end if;
         end loop;

         declare
            Buffer : Stream_Element_Array (1 .. Pending'Last - Pending_Last);
            Count  : Stream_Element_Offset;
         begin
            if Buffer'Length = 0 then
               Pending_Last := 0;
               return;
            end if;

            Console.Read (Buffer, Count);
            if Count < Buffer'First then
               return;
            end if;

            Pending (Pending_Last + 1 .. Pending_Last + Count) :=
              Buffer (1 .. Count);
            Pending_Last := Pending_Last + Count;
         end;
      end loop;
   end Get_Line;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
      Buffer : Stream_Element_Array (1 .. Text'Length);
      for Buffer'Address use Text'Address;
      pragma Import (Ada, Buffer);
   begin
      Console.Write (Buffer);
   end Put;

   ---------
   -- Run --
   ---------

   procedure Run is
      Banner   : constant String :=
                   "Rose " & Rose.Version.Version_String
                   & " (" & Rose.Version.Version_Name & ")"
                   & Rose.Build.Build_Suffix;
      Name     : String (1 .. 64);
      Password : String (1 .. 256);
      Name_Last, Password_Last : Natural;
      Clear    : Boolean := True;
   begin
      loop
         begin

            --  The screen is cleared at the start and after a session,
            --  but not after a failed login, so that the message stays.

            if Clear then
               Console.Clear;
               Put (Banner & LF);
               Clear := False;
            end if;

            Put ("login: ");
            Get_Line (Name, Name_Last);

            if Name_Last >= Name'First then
               Put ("password: ");
               Console.Disable_Echo;
               Get_Line (Password, Password_Last);
               Console.Enable_Echo;
               Put ((1 => LF));

               declare
                  User : Rose.Interfaces.Users.User.User_Object'Class :=
                           Auth.Login
                             (Name (Name'First .. Name_Last),
                              Password (Password'First .. Password_Last));
               begin
                  Password := (others => ' ');
                  if User.User_Cap /= Rose.Capabilities.Null_Capability
                  then
                     Run_Petal (User);

                     --  Whatever the session left, the next one starts
                     --  with line editing and echo on, and plain text
                     --  (#734).

                     Console.Enable_Line_Editing;
                     Console.Enable_Echo;
                     Console.Set_Attributes (False, False);
                     Clear := True;
                  else
                     Wait (Failure_Delay);
                     Put ("Login failed" & LF);
                  end if;
               end;
            end if;

         exception
            when E : others =>

               --  Login must keep running, or the console has nothing to
               --  read it.

               Console.Enable_Line_Editing;
               Console.Enable_Echo;
               Put (LF & "login: " & Ada.Exceptions.Exception_Message (E)
                    & LF);
               Wait (Failure_Delay);
         end;
      end loop;
   end Run;

   ---------------
   -- Run_Petal --
   ---------------

   procedure Run_Petal
     (User : in out Rose.Interfaces.Users.User.User_Object'Class)
   is

      --  The session's bank (ADR 0014, section 2; #450), a sub-bank of
      --  the user's. Petal is launched against it, and so its jobs are
      --  in it too.

      Session  : Rose.Interfaces.Storage.Bank.Bank_Object'Class := User
        .New_Session;
      Launcher : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                   Commands.Find ("petal");
      Endpoint : constant Rose.Capabilities.Capability :=
                   Launcher.Launch_Command (Session.Bank_Cap);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         Session.Destroy;
         Put ("login: cannot run petal" & LF);
         return;
      end if;

      --  Petal's roles' caps and its User argument, in the order of its
      --  command declaration (src/utilities/petal/rose-core-petal.idl).
      --  A standard stream is the cap of its Read or Write, which the
      --  run-time system invokes directly (Rose.Standard_Streams). The
      --  call returns when petal logs out (#458).

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Caps
        (Params,
         (Console.Get_Read_Cap,
          Console.Get_Write_Cap,
          Console.Get_Write_Cap,
          Rose.System_Calls.Create_Endpoint_Capability,
          Rose.System_Calls.Delete_Cap_Capability,
          Rose.System_Calls.Rescind_Cap_Capability,
          Commands.Lookup_Cap,
          Console.Terminal_Cap,
          Clock_Interface_Cap,
          Rights_Interface_Cap,
          User.User_Cap));
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      Rose.System_Calls.Delete_Cap (Endpoint);

      --  Ending the session destroys its bank: whatever the session
      --  left running ends, and what it reserved is released.

      Session.Destroy;
   end Run_Petal;

   --------------
   -- Set_Caps --
   --------------

   procedure Set_Caps
     (Console  : Rose.Capabilities.Capability;
      Commands : Rose.Capabilities.Capability;
      Timer    : Rose.Capabilities.Capability;
      Clock    : Rose.Capabilities.Capability;
      Rights   : Rose.Capabilities.Capability;
      Auth     : Rose.Capabilities.Capability)
   is
   begin
      Clock_Interface_Cap := Clock;
      Rights_Interface_Cap := Rights;
      Rose.Interfaces.Screens.Terminal.Client.Open (Login_Loop
        .Console, Console);
      Rose.Interfaces.Commands.Lookup.Client.Open (Login_Loop
        .Commands, Commands);
      Rose.Interfaces.Time.Timer.Client.Open (Login_Loop.Timer, Timer);
      Rose.Interfaces.Users.Auth.Client.Open (Login_Loop.Auth, Auth);
   end Set_Caps;

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
      --  waiting, so login is already receiving on it by then.

      declare
         Handle : Rose.Interfaces.Caps.Cap.Cap_Object'Class :=
                    Timer.Set_Timer (Milliseconds, Timeout_Send_Cap)
           with Unreferenced;
      begin
         Rose.System_Calls.Initialize_Receive (Params, Timeout_Receive_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Wait;

end Login_Loop;
