with Ada.Exceptions;

with Ada.Streams;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Build;
with Rose.Version;
with Rose.Words;

with Rose.Interfaces.Users.Auth.Client;
with Rose.Interfaces.Caps.Cap;
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
   Timer    : Rose.Interfaces.Time.Timer.Client.Timer_Client;
   Auth     : Rose.Interfaces.Users.Auth.Client.Auth_Client;

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

   function Run return Rose.Capabilities.Capability is
      Banner   : constant String :=
                   "Rose " & Rose.Version.Version_String
                   & " (" & Rose.Version.Version_Name & ")"
                   & Rose.Build.Build_Suffix;
      Name     : String (1 .. 64);
      Password : String (1 .. 256);
      Name_Last, Password_Last : Natural;
   begin

      --  The screen is cleared at the start, but not after a failed
      --  login, so that the message stays.

      Console.Clear;
      Put (Banner & LF);

      loop
         begin
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
                     return User.User_Cap;
                  end if;

                  Wait (Failure_Delay);
                  Put ("Login failed" & LF);
               end;
            end if;

         exception
            when E : others =>

               --  Keep asking: an error that a login can raise, such as
               --  a name that is too long, is the person's to correct.

               Password := (others => ' ');
               Console.Enable_Line_Editing;
               Console.Enable_Echo;
               Put (LF & "login: " & Ada.Exceptions.Exception_Message (E)
                    & LF);
               Wait (Failure_Delay);
         end;
      end loop;
   end Run;

   --------------
   -- Set_Caps --
   --------------

   procedure Set_Caps
     (Console : Rose.Capabilities.Capability;
      Timer   : Rose.Capabilities.Capability;
      Auth    : Rose.Capabilities.Capability)
   is
   begin
      Rose.Interfaces.Screens.Terminal.Client.Open (Login_Loop
        .Console, Console);
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
