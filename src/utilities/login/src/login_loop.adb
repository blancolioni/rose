with Ada.Exceptions;

with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Version;
with Rose.Words;

with Rose.Interfaces.Auth.Client;
with Rose.Interfaces.Cap;
with Rose.Interfaces.Commands.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Terminal.Client;
with Rose.Interfaces.Timeout;
with Rose.Interfaces.Timer.Client;
with Rose.Interfaces.User;

package body Login_Loop is

   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;

   LF  : constant Character := Character'Val (10);
   ESC : constant Character := Character'Val (27);

   Failure_Delay : constant Rose.Words.Word := 3_000;
   --  Milliseconds to wait after a wrong name or password.

   Console  : Rose.Interfaces.Terminal.Client.Terminal_Client;
   Commands : Rose.Interfaces.Commands.Client.Commands_Client;
   Timer    : Rose.Interfaces.Timer.Client.Timer_Client;
   Auth     : Rose.Interfaces.Auth.Client.Auth_Client;

   Pending      : Storage_Array (1 .. 4096);
   Pending_Last : Storage_Count := 0;
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
     (User : in out Rose.Interfaces.User.User_Object'Class);
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
            Buffer : Storage_Array (1 .. Pending'Last - Pending_Last);
            Count  : Storage_Count;
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
      Buffer : Storage_Array (1 .. Text'Length);
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
                   & " (" & Rose.Version.Version_Name & ")";
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
               Put (ESC & "[2J" & ESC & "[H" & Banner & LF);
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
                  User : Rose.Interfaces.User.User_Object'Class :=
                           Auth.Login
                             (Name (Name'First .. Name_Last),
                              Password (Password'First .. Password_Last));
               begin
                  Password := (others => ' ');
                  if User.User_Cap /= Rose.Capabilities.Null_Capability
                  then
                     Run_Petal (User);
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
     (User : in out Rose.Interfaces.User.User_Object'Class)
   is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
                   Commands.Find ("petal");
      Endpoint : constant Rose.Capabilities.Capability :=
                   Launcher.Launch_Command (No_Caps);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         Put ("login: cannot run petal" & LF);
         return;
      end if;

      --  Petal's roles' caps and its User argument, in the order of its
      --  command declaration (src/utilities/petal/rose-core-petal.idl).
      --  A standard stream is the cap of its Read or Write, which the
      --  run-time system invokes directly (Rose.Standard_Streams). The
      --  call returns when petal logs out (#458).

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Console.Get_Read_Cap);
      Rose.System_Calls.Send_Cap (Params, Console.Get_Write_Cap);
      Rose.System_Calls.Send_Cap (Params, Console.Get_Write_Cap);
      Rose.System_Calls.Send_Cap
        (Params, Rose.System_Calls.Create_Endpoint_Capability);
      Rose.System_Calls.Send_Cap
        (Params, Rose.System_Calls.Delete_Cap_Capability);
      Rose.System_Calls.Send_Cap
        (Params, Rose.System_Calls.Rescind_Cap_Capability);
      Rose.System_Calls.Send_Cap (Params, Commands.Commands_Cap);
      Rose.System_Calls.Send_Cap (Params, Console.Terminal_Cap);
      Rose.System_Calls.Send_Cap (Params, User.User_Cap);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      Rose.System_Calls.Delete_Cap (Endpoint);
   end Run_Petal;

   --------------
   -- Set_Caps --
   --------------

   procedure Set_Caps
     (Console  : Rose.Capabilities.Capability;
      Commands : Rose.Capabilities.Capability;
      Timer    : Rose.Capabilities.Capability;
      Auth     : Rose.Capabilities.Capability)
   is
   begin
      Rose.Interfaces.Terminal.Client.Open (Login_Loop.Console, Console);
      Rose.Interfaces.Commands.Client.Open (Login_Loop.Commands, Commands);
      Rose.Interfaces.Timer.Client.Open (Login_Loop.Timer, Timer);
      Rose.Interfaces.Auth.Client.Open (Login_Loop.Auth, Auth);
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
              Rose.Interfaces.Timeout.On_Timeout_Endpoint);
      end if;

      --  The timer calls the endpoint when the time is up, without
      --  waiting, so login is already receiving on it by then.

      declare
         Handle : Rose.Interfaces.Cap.Cap_Object'Class :=
                    Timer.Set_Timer (Milliseconds, Timeout_Send_Cap)
           with Unreferenced;
      begin
         Rose.System_Calls.Initialize_Receive (Params, Timeout_Receive_Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end;
   end Wait;

end Login_Loop;
