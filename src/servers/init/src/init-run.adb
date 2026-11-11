with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Init_Capabilities;
with Rose.Limits;
with Rose.Words;

with Rose.Interfaces.Exec;
with Rose.Interfaces.Map;
with Rose.Interfaces.Process;

with Rose.Invocation;
with Rose.System_Calls;

with Init.Boot;
with Init.Boot_Steps;
with Init.Calls;
with Init.Grants;
with Init.Installer;
with Init.Self_Tests;
with Init.Users;

package body Init.Run is

   NL : constant Character := Character'Val (10);

   Is_Driver_Flag     : constant := 32;
   --  Set in the flags of an install message for a program from the
   --  drivers directory (Restore.Installer.Is_Driver).

   Requires_Framebuffer_Flag : constant := 128;
   Requires_No_Framebuffer_Flag : constant := 512;
   --  Set for a program that is installed only if the boot loader gave a
   --  framebuffer: the screen driver (Restore.Installer
   --  .Requires_Framebuffer, #582). Requires_No_Framebuffer_Flag is set
   --  for one that is installed only if there isn't, vga-console (#653).

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;

   --------------
   -- Run_Init --
   --------------

   procedure Run_Init (Install : Boolean) is
      use Rose.Capabilities, Rose.Words;
      package Init_Caps renames Rose.Init_Capabilities;

      --  The caps the kernel gives init

      Reserve_Cap          : constant Capability :=
                               Init_Caps.Reserve_Memory_Cap;
      Create_Endpoint_Cap  : constant Capability :=
                               Init_Caps.Create_Endpoint_Cap;
      Delete_Cap           : constant Capability := Init_Caps.Delete_Cap;
      Rescind_Cap          : constant Capability := Init_Caps.Rescind_Cap;
      Derive_Cap           : constant Capability := Init_Caps.Derive_Cap;
      Exit_Cap             : constant Capability := Init_Caps.Exit_Cap;
      Restart_Cap          : constant Capability := Init_Caps.Restart_Cap;
      Trace_Cap            : constant Capability := Init_Caps.Trace_Cap;
      --  For trace events (Rose.Trace), given to the processes that
      --  launch commands.

      --  The boot servers' caps, which Init.Boot sets when it launches
      --  them (boot.yaml)

      Console_Interface_Cap  : Capability
                                 renames Init.Boot.Console_Interface_Cap;
      Console_Stream_Cap     : Capability
                                 renames Init.Boot.Console_Stream_Cap;
      Console_Redirect_Cap   : Capability
                                 renames Init.Boot.Console_Redirect_Cap;
      Storage_Cap            : Capability renames Init.Boot.Storage_Cap;
      System_Bank_Cap        : Capability := Null_Capability;
      Reserve_Bank_Cap       : Capability := Null_Capability;
      --  The system bank's Bank interface and its Reserve_Storage
      --  endpoint (ADR 0014, #450): exec and the command registry are
      --  launched against it, exec launches everything else against it,
      --  unless given a bank, and the installer reserves from it.
      Launch_Elf_Cap         : Capability renames Init.Boot.Launch_Elf_Cap;
      Launch_Command_Elf_Cap : Capability
                                 renames Init.Boot.Launch_Command_Elf_Cap;
      Install_Receiver       : Capability
                                 renames Init.Boot.Install_Receiver_Cap;

      Keyboard_Interface_Cap : Rose.Capabilities.Capability :=
                                 Null_Capability
                                   with Unreferenced;
      Console_Input_Stream_Cap : Rose.Capabilities.Capability :=
                                   Null_Capability;
      Console_Terminal_Cap     : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  The console as a Terminal, for the console role (#454).

      Clock_Cap                : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  The clock's Clock interface, from the installer (ADR 0020).
      Clock_Admin_Cap          : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  The clock's Clock_Admin interface, which init gives to the rights
      --  broker and then deletes (ADR 0021).
      Screen_Cap               : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  The screen driver's surface, from the installer, if there is a
      --  framebuffer (#582).
      Cap_Sets_Cap             : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  The userland cap set server's Constructor interface, from the
      --  installer (#825), for the self tests' Cap_Set_Service role.
      Create_Cap_Set_Cap       : Rose.Capabilities.Capability :=
                                   Null_Capability;
      --  Its Create cap, for the set that init leaves for the restore
      --  test.

      Restored           : Boolean := False;
      --  Set if the restore server restored a checkpoint, in which case
      --  the programs it would have installed are running already.
      Updated            : Boolean := False;
      --  Set if, as well, the update boot has written the install
      --  media's boot set to hd0 (#828), so that init restarts into it.
      Restore_Reply_Cap    : Rose.Capabilities.Capability := 0;
      --  The reply cap of the restore server's last message.

      Find_Cap    : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;

      Commands_Cap : Rose.Capabilities.Capability :=
                       Rose.Capabilities.Null_Capability;
      --  The command registry's Commands interface (#460).

      First_User     : String (1 .. Init.Users.Max_Name_Length);
      First_Last     : Natural := 0;
      First_Password : String (1 .. Init.Users.Max_Password_Length);
      Password_Last  : Natural := 0;
      --  The first user's name and password, which the person installing
      --  types (#456).
      Auth_Cap       : Rose.Capabilities.Capability;
      Auth_Admin_Cap : Rose.Capabilities.Capability;
      User_Cap       : Rose.Capabilities.Capability;
      Home_Cap   : Rose.Capabilities.Capability;
      --  The first user, and its home directory, created on first boot.
      Rights_Cap       : Rose.Capabilities.Capability := Null_Capability;
      Rights_Admin_Cap : Rose.Capabilities.Capability := Null_Capability;
      --  The rights broker's Rights and Rights_Admin interfaces
      --  (ADR 0021).

      function Get_Public_Interface_From_Process
        (Process_Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability;

      procedure Wait (Milliseconds : Natural)
                      renames Init.Boot_Steps.Wait;

      function Take_Checkpoint return Boolean;
      --  Take the new system's first checkpoint, through the checkpoint
      --  server (#413), trying again if the kernel can't take its
      --  snapshot.  False, and a message, if there is none.

      ---------------------------------------
      -- Get_Public_Interface_From_Process --
      ---------------------------------------

      function Get_Public_Interface_From_Process
        (Process_Cap : Rose.Capabilities.Capability)
         return Rose.Capabilities.Capability
      is
         Caps          : Init.Calls.Array_Of_Capabilities
           (1 .. Rose.Interfaces.Process.Subprogram_Count);
         Public_Cap    : Rose.Capabilities.Capability;
         Interface_Cap : Rose.Capabilities.Capability := Null_Capability;
      begin
         --  get process interface caps
         Init.Calls.Get_Interface (Process_Cap, Caps);

         Public_Cap :=
           Caps (Rose.Interfaces.Process.Published_Interface_Index + 1);

         if Public_Cap /= Null_Capability then
            for I in 1 .. 4 loop
               Init.Calls.Get_Interface (Public_Cap, Caps);
               Interface_Cap := Caps (1);
               exit when Interface_Cap /= Null_Capability;
               Wait (500);
            end loop;
         end if;

         return Interface_Cap;
      end Get_Public_Interface_From_Process;

      ---------------------
      -- Take_Checkpoint --
      ---------------------

      function Take_Checkpoint return Boolean is
      begin
         for Attempt in 1 .. 5 loop
            declare
               Params : aliased Rose.Invocation.Invocation_Record;
            begin
               Rose.System_Calls.Initialize_Send
                 (Params, Init.Boot.Checkpointer_Cap);
               Rose.System_Calls.Receive_Words (Params, 1);
               Rose.System_Calls.Invoke_Capability (Params);
               if not Params.Control.Flags (Rose.Invocation.Error)
                 and then Params.Control.Flags (Rose.Invocation.Send_Words)
                 and then Params.Data (0) = 1
               then
                  return True;
               end if;
            end;
            Wait (1000);
         end loop;

         Init.Calls.Send_String
           (Console_Stream_Cap, "init: no checkpoint; not restarting" & NL);
         return False;
      end Take_Checkpoint;

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap      => Delete_Cap,
         Rescind_Cap     => Rescind_Cap);

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Init.Calls.Send (Reserve_Cap, (16#0000_1000#, 16#0009_F000#));

      --  The boot servers, up to the restore server, which restores a
      --  checkpoint, or on the install media, installs a new system.

      Init.Boot.Start (Install);
      Init.Boot.Launch_Boot_Servers;

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
         Reply  : aliased Rose.Invocation.Invocation_Record;
         Install_Cap : Rose.Capabilities.Capability :=
                         Rose.Capabilities.Null_Capability;
         Add_Cap     : Rose.Capabilities.Capability :=
                         Rose.Capabilities.Null_Capability;
         Do_Exec     : Boolean := True;
         Do_Command  : Boolean := True;
         Set_Output_Cap : Rose.Capabilities.Capability :=
                            Rose.Capabilities.Null_Capability;

         function Installs (Name : String) return Boolean;
         --  True if Params asks to install the program called Name.

         --------------
         -- Installs --
         --------------

         function Installs (Name : String) return Boolean is
            use System.Storage_Elements;
            Buffer : Storage_Array (1 .. Params.Buffer_Length);
            pragma Import (Ada, Buffer);
            for Buffer'Address use Params.Buffer_Address;
         begin
            if not Params.Control.Flags (Rose.Invocation.Send_Buffer)
              or else Buffer'Length /= Name'Length
            then
               return False;
            end if;

            for I in Name'Range loop
               if Character'Val
                 (Buffer (Storage_Offset (I - Name'First + 1)))
                 /= Name (I)
               then
                  return False;
               end if;
            end loop;

            return True;
         end Installs;

      begin
         loop
            Rose.System_Calls.Initialize_Receive (Params, Install_Receiver);
            Rose.System_Calls.Receive_Caps (Params, 3);
            Rose.System_Calls.Receive_Words (Params, 3);
            Rose.System_Calls.Receive_Buffer (Params, 4096);
            Rose.System_Calls.Invoke_Capability (Params);
            --  The restore server's last message has no caps.  Its first
            --  data word is 1 if it restored a checkpoint, and 2 if it
            --  did, and updated hd0's boot set too.  The restore init
            --  installs nothing, so that is the only message it takes.

            if not Install
              or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
            then
               Restored := Params.Control.Flags (Rose.Invocation.Send_Words)
                 and then Params.Data (0) in 1 .. 2;
               Updated := Restored and then Params.Data (0) = 2;
               Restore_Reply_Cap := Params.Reply_Cap;
               exit;
            end if;

            Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

            if Do_Exec then
               Do_Exec := False;

               --  The system bank is made only now, when a new system is
               --  installed. On the update boot the install media restores
               --  a checkpoint instead, and a bank made before the restore
               --  would take the number of the checkpoint's own system
               --  bank (#873).

               Init.Grants.System_Bank
                 (Storage_Cap, System_Bank_Cap, Reserve_Bank_Cap);

               declare
                  Exec_Cap : constant Rose.Capabilities.Capability :=
                               Init.Installer.Install_Exec_Library
                                 (Bank_Cap        => System_Bank_Cap,
                                  Reserve_Cap     => Reserve_Bank_Cap,
                                  Launch_Cap      => Launch_Elf_Cap,
                                  Launch_Command_Cap =>
                                    Launch_Command_Elf_Cap,
                                  Trace_Cap       => Trace_Cap,
                                  Cap_Stream      => Params.Caps (0),
                                  Standard_Output => Console_Interface_Cap,
                                  Binary_Stream   => Params.Caps (1),
                                  Binary_Length   => Params.Data (0));
                  Caps     : Init.Calls.Array_Of_Capabilities
                    (1 .. Rose.Interfaces.Exec.Subprogram_Count);
                  Public_Cap : constant Rose.Capabilities.Capability :=
                                 Get_Public_Interface_From_Process (Exec_Cap);
               begin
                  Install_Cap := Null_Capability;

                  if Public_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "unable to find exec interface" & NL);
                  else
                     Init.Calls.Get_Interface (Public_Cap, Caps);
                     Install_Cap :=
                       Caps (Rose.Interfaces.Exec.Install_Index + 1);
                     Set_Output_Cap :=
                       Caps
                         (Rose.Interfaces.Exec.Set_Standard_Output_Index + 1);
                  end if;
               end;

            elsif Do_Command and then Installs ("command") then
               Do_Command := False;

               declare
                  Command_Cap : constant Rose.Capabilities.Capability :=
                                  Init.Installer.Install_Command_Library
                                    (Bank_Cap        => System_Bank_Cap,
                                     Reserve_Cap     => Reserve_Bank_Cap,
                                     Launch_Cap      => Launch_Elf_Cap,
                                     Cap_Stream      => Params.Caps (0),
                                     Standard_Output => Console_Interface_Cap,
                                     Binary_Stream   => Params.Caps (1),
                                     Binary_Length   => Params.Data (0));
                  Caps     : Init.Calls.Array_Of_Capabilities
                    (1 .. Rose.Interfaces.Map.Subprogram_Count);
                  Public_Cap : constant Rose.Capabilities.Capability :=
                                  Get_Public_Interface_From_Process
                                    (Command_Cap);
                  No_Words   : Init.Calls.Array_Of_Words (1 .. 0);
               begin
                  Add_Cap := Null_Capability;
                  for I in 1 .. 4 loop
                     Init.Calls.Get_Interface (Public_Cap, Caps);
                     Find_Cap := Caps (Rose.Interfaces.Map.Find_Index + 1);
                     Add_Cap := Caps (Rose.Interfaces.Map.Add_Index + 1);
                     exit when Add_Cap /= Null_Capability;
                     Wait (500);
                  end loop;

                  if Add_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "unable to find command interface" & NL);
                  end if;
                  if Find_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "find cap was null" & NL);
                  end if;

                  --  The registry's Commands interface, which finds
                  --  commands but cannot change the registry, for
                  --  petal and the self tests (#460).

                  declare
                     Get_Commands : constant Rose.Capabilities.Capability :=
                       Caps (Rose.Interfaces.Map.Get_Commands_Index + 1);
                  begin
                     if Get_Commands /= Null_Capability then
                        Commands_Cap :=
                          Init.Calls.Call (Get_Commands, No_Words);
                     end if;
                  end;
                  if Commands_Cap = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "commands cap was null" & NL);
                  end if;
               end;
            elsif Params.Data (1) = 99 then
               Keyboard_Interface_Cap := Params.Caps (0);
               Console_Input_Stream_Cap := Params.Caps (1);
               Console_Terminal_Cap := Params.Caps (2);
               if Console_Input_Stream_Cap = Null_Capability then
                  Init.Calls.Send_String
                    (Console_Stream_Cap,
                     "did not receive input stream cap" & NL);
               else
                  Init.Calls.Send_String
                    (Console_Stream_Cap, "received input stream cap" & NL);
               end if;
            elsif Params.Data (1) = 96 then

               --  The screen driver is running, and cap 0 is its surface.

               Screen_Cap := Params.Caps (0);
            elsif Params.Data (1) = 97 then

               --  The clock is running. Its Clock_Admin interface is cap
               --  0, and its Clock interface is cap 1.

               Clock_Admin_Cap := Params.Caps (0);
               Clock_Cap := Params.Caps (1);
            elsif Params.Data (1) = 95 then

               --  The userland cap set server is running, and cap 0 is
               --  its Constructor interface (#825).

               declare
                  Caps : Init.Calls.Array_Of_Capabilities (1 .. 1);
               begin
                  Cap_Sets_Cap := Params.Caps (0);
                  Init.Calls.Get_Interface (Cap_Sets_Cap, Caps);
                  Create_Cap_Set_Cap := Caps (1);
               end;
            elsif Params.Data (1) = 98 then

               --  The persistent console is running.  The boot console
               --  sends what it is given on to it, and it is the
               --  standard output of everything installed from now on.

               Console_Interface_Cap := Params.Caps (0);

               declare
                  Caps : Init.Calls.Array_Of_Capabilities (1 .. 1);

                  procedure Send_Console (To : Rose.Capabilities.Capability);

                  ------------------
                  -- Send_Console --
                  ------------------

                  procedure Send_Console
                    (To : Rose.Capabilities.Capability)
                  is
                     Call_Params : aliased Rose.Invocation.Invocation_Record;
                  begin
                     Rose.System_Calls.Initialize_Send (Call_Params, To);
                     Rose.System_Calls.Send_Cap
                       (Call_Params, Console_Interface_Cap);
                     Rose.System_Calls.Invoke_Capability (Call_Params);
                  end Send_Console;

               begin
                  Init.Calls.Get_Interface (Console_Interface_Cap, Caps);
                  Console_Stream_Cap := Caps (1);
                  Init.Calls.Set_Console (Console_Stream_Cap);
                  Send_Console (Console_Redirect_Cap);
                  Send_Console (Set_Output_Cap);
               end;
            elsif (Params.Data (2) / Requires_Framebuffer_Flag) mod 2 = 1
              and then not Init.Grants.Has_Framebuffer
            then

               --  The screen driver, and there is no framebuffer. It isn't
               --  installed, and restore gets no caps (#582).

               null;
            elsif (Params.Data (2) / Requires_No_Framebuffer_Flag) mod 2 = 1
              and then Init.Grants.Has_Framebuffer
            then

               --  The console on the VGA page, and the console is on the
               --  screen's surface instead (#653).

               null;
            else

               declare
                  Extra_Caps : constant
                    Init.Calls.Array_Of_Capabilities (1 .. 1)
                    := (1 => Params.Caps (2));
                  Last_Cap   : constant Natural
                    := (if Params.Caps (2)
                        = Null_Capability
                        then 0 else 1);
                  Name       : String (1 .. 64);
                  Name_Last  : Natural := 0;
                  Launch     : Capability;
                  Buffer     : System.Storage_Elements.Storage_Array
                    (1 .. 4096);
                  pragma Import (Ada, Buffer);
                  for Buffer'Address use Params.Buffer_Address;
               begin

                  for I in 1 .. Params.Buffer_Length loop
                     Name_Last := Name_Last + 1;
                     Name (Name_Last) :=
                       Character'Val (Buffer (I));
                  end loop;

                  Launch :=
                    Init.Installer.Install_Executable
                      (Install_Cap   => Install_Cap,
                       Cap_Stream    => Params.Caps (0),
                       Binary_Stream => Params.Caps (1),
                       Binary_Length => Params.Data (0),
                       Name          => Name (1 .. Name_Last),
                       Is_Driver     =>
                         (Params.Data (2) / Is_Driver_Flag) mod 2 = 1,
                       Extra_Caps    => Extra_Caps (1 .. Last_Cap));

                  Wait (1000);

                  --  Launch is the program's Launch interface. Restore
                  --  launches drivers itself, through the interface's
                  --  Launch cap, and saves commands with the interface.
                  --  It is null if exec could not install the program,
                  --  and then restore gets an error.

                  if Launch = Null_Capability then
                     Init.Calls.Send_String
                       (Console_Stream_Cap,
                        "init: cannot install " & Name (1 .. Name_Last)
                        & NL);
                     Rose.System_Calls.Send_Error
                       (Reply, Rose.Invocation.Request_Error);
                  elsif Params.Data (1) = 0 then
                     declare
                        Launch_Caps : Init.Calls.Array_Of_Capabilities
                          (1 .. 3);
                     begin
                        Init.Calls.Get_Interface (Launch, Launch_Caps);
                        Rose.System_Calls.Send_Cap (Reply, Launch_Caps (1));
                     end;
                  elsif Params.Data (1) = 1 then
                     Rose.System_Calls.Send_Cap (Reply, Add_Cap);
                     Rose.System_Calls.Send_Cap (Reply, Launch);
                  else
                     Init.Calls.Send_String
                       (Console_Stream_Cap, "init: bad action");
                  end if;
               end;
            end if;

            Rose.System_Calls.Invoke_Capability (Reply);
         end loop;
      end;

      --  The restore init doesn't install.  If there was no checkpoint to
      --  restore, the system stays as it is, with this message on the
      --  screen (#373).

      if not Install and then not Restored then
         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: no system on this disk; boot the install media" & NL);
         Init.Boot_Steps.End_Boot_Cap_Sets;
         Init.Calls.Send (Exit_Cap);
         return;
      end if;

      Init.Boot.Launch_Checkpoint;

      --  After a restore, the programs that would be launched below are
      --  running already, as they were at the checkpoint.

      if Restored then

         --  The checkpoint server was the last boot module (#826).

         Init.Boot_Steps.End_Boot_Cap_Sets;

         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: restored from a checkpoint; exiting" & NL);

         --  The restored console redraws the screen as it was at the
         --  checkpoint, so the boot console stops writing to it.

         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Console_Redirect_Cap);
            Rose.System_Calls.Invoke_Capability (Params);
         end;

         --  The restore server lets the restored processes run when
         --  this reply arrives, now that the boot modules are started.

         declare
            Reply : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Reply (Reply, Restore_Reply_Cap);
            Rose.System_Calls.Invoke_Capability (Reply);
         end;

         --  After an update, restart from hd0 once a checkpoint is in
         --  the log, as an install does, so that the machine boots the
         --  new set from hd0 (#828).

         if Updated then
            Init.Calls.Send_String
              (Console_Stream_Cap, "update complete; restarting from hd0"
               & NL);
            if Take_Checkpoint then
               Wait (3000);
               Init.Calls.Send (Restart_Cap);
            end if;
         end if;

         Init.Calls.Send (Exit_Cap);
         return;
      end if;

      Init.Calls.Send_String
        (Console_Stream_Cap,
         "completed initial install" & NL);

      if Find_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to find 'find'" & NL);
      end if;

      if Cap_Sets_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: no cap set server" & NL);
      end if;

      if Clock_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: no clock" & NL);
      end if;

      --  The self tests run as a command, and init waits for them to
      --  finish before it goes on (#460).

      Init.Calls.Send_String
        (Console_Stream_Cap, "running self tests" & NL);
      if not Init.Self_Tests.Derive_Test
        (Find_Cap        => Find_Cap,
         Standard_Output => Console_Stream_Cap,
         Derive_Cap      => Derive_Cap,
         Rescind_Cap     => Rescind_Cap,
         Delete_Cap      => Delete_Cap)
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: derive test failed" & NL);
      end if;

      --  A persistent holder of derived caps, which checks them after a
      --  restore (#691).

      if not Init.Self_Tests.Start_Derived_Holder
        (Find_Cap        => Find_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         Standard_Output => Console_Stream_Cap,
         Commands        => Commands_Cap,
         Derive_Cap      => Derive_Cap,
         Rescind_Cap     => Rescind_Cap,
         Delete_Cap      => Delete_Cap)
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: hold_derived did not start" & NL);
      end if;

      if not Init.Self_Tests.Run
        (Find_Cap        => Find_Cap,
         Standard_Output => Console_Stream_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         Commands        => Commands_Cap,
         Clock           => Clock_Cap,
         Screen          => Screen_Cap,
         Create_Signal   => Rose.Init_Capabilities.Create_Signal_Cap,
         Cap_Sets        => Cap_Sets_Cap)
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: self tests failed" & NL);
      end if;

      --  A cap set that the checkpoint keeps, so that the boot test can
      --  see that a set made before a checkpoint is there after a restore:
      --  the cap set server reports its sets when it is restored (#825).
      --  It holds the registry's Commands interface and the clock, which
      --  are persistent too.

      if Create_Cap_Set_Cap /= Null_Capability then
         if Init.Calls.Create_Cap_Set_With
           (Create_Cap_Set_Cap, (Commands_Cap, Clock_Cap))
           = Null_Capability
         then
            Init.Calls.Send_String
              (Console_Stream_Cap, "init: no cap set for the restore test"
               & NL);
         end if;
      end if;

      --  The auth server, which keeps the users and their passwords
      --  (#455).

      Init.Users.Start_Auth
        (Find_Cap        => Find_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         Commands        => Commands_Cap,
         Auth            => Auth_Cap,
         Admin           => Auth_Admin_Cap);
      if Auth_Cap = Null_Capability
        or else Auth_Admin_Cap = Null_Capability
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to start auth" & NL);
      end if;

      --  The rights broker, which holds the clock's Clock_Admin interface
      --  and decides who may have it (ADR 0021). Init hands the interface
      --  over, and keeps no copy of it.

      Init.Users.Start_Rights
        (Find_Cap        => Find_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         Console         => Console_Stream_Cap,
         Clock_Admin     => Clock_Admin_Cap,
         Rights          => Rights_Cap,
         Admin           => Rights_Admin_Cap);
      if Rights_Cap = Null_Capability
        or else Rights_Admin_Cap = Null_Capability
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to start rights" & NL);
      end if;
      Rose.System_Calls.Delete_Cap (Clock_Admin_Cap);
      Clock_Admin_Cap := Null_Capability;

      --  The first user, with a home directory (ADR 0003), whose name and
      --  password the person installing types (#456). Init runs only on
      --  a fresh boot, so it is created once.

      Init.Users.Ask_First_User
        (Console       => Console_Terminal_Cap,
         Output        => Console_Stream_Cap,
         Name          => First_User,
         Name_Last     => First_Last,
         Password      => First_Password,
         Password_Last => Password_Last);

      Init.Calls.Send_String
        (Console_Stream_Cap,
         "creating user " & First_User (1 .. First_Last) & NL);
      Init.Users.Create_First_User
        (Name     => First_User (1 .. First_Last),
         Password => First_Password (1 .. Password_Last),
         Admin    => Auth_Admin_Cap,
         User     => User_Cap,
         Home     => Home_Cap);
      if User_Cap = Rose.Capabilities.Null_Capability then
         Init.Calls.Send_String
           (Console_Stream_Cap,
            "init: unable to create user " & First_User (1 .. First_Last)
            & NL);
      elsif Init.Users.Check_Login
          (Auth_Cap, First_User (1 .. First_Last),
           First_Password (1 .. Password_Last))
        and then not Init.Users.Check_Login
          (Auth_Cap, First_User (1 .. First_Last),
           First_Password (1 .. Password_Last) & "x")
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "auth: login checked" & NL);
      else
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: login check failed" & NL);
      end if;

      --  The first user may have every category of authority, under sudo
      --  (ADR 0021). Every later user starts with none. Check that the
      --  broker has it: it refuses an ordinary request for the clock's
      --  admin role, and grants one under sudo. Init needs the broker's
      --  Rights_Admin interface no more after that.

      if User_Cap /= Null_Capability then
         if not Init.Users.Allow
           (Rights_Admin_Cap, First_User (1 .. First_Last), "*",
            Init.Users.Mode_Elevate)
         then
            Init.Calls.Send_String
              (Console_Stream_Cap,
               "init: unable to set the policy of "
               & First_User (1 .. First_Last) & NL);
         elsif Init.Users.Check_Rights
           (Rights_Cap, User_Cap, "Clock_Admin_Service")
         then
            Init.Calls.Send_String
              (Console_Stream_Cap, "rights: policy checked" & NL);
         else
            Init.Calls.Send_String
              (Console_Stream_Cap, "init: rights check failed" & NL);
         end if;
      end if;
      Rose.System_Calls.Delete_Cap (Rights_Admin_Cap);
      Rights_Admin_Cap := Null_Capability;

      --  The person installing has seen the install finish. The restart
      --  waits for them, and comes before login runs, which would read
      --  the console too (#456).

      Init.Users.Wait_For_Enter
        (Console => Console_Terminal_Cap,
         Output  => Console_Stream_Cap,
         Message => NL & "The system is installed. Press enter to reboot."
                    & NL);

      --  The console runs login, which asks for a user name and password
      --  and runs petal for the user (#457, #458).

      Init.Calls.Send_String
        (Console_Stream_Cap, "launching login" & NL);
      if not Init.Users.Start_Login
        (Find_Cap        => Find_Cap,
         Console         => Console_Terminal_Cap,
         Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap      => Delete_Cap,
         Rescind_Cap     => Rescind_Cap,
         Commands        => Commands_Cap,
         Timer           => Init.Boot.Timer_Interface_Cap,
         Clock           => Clock_Cap,
         Rights          => Rights_Cap,
         Auth            => Auth_Cap)
      then
         Init.Calls.Send_String
           (Console_Stream_Cap, "init: unable to launch login" & NL);
      end if;

      --  Login is the last launch. Init's derived capabilities are with
      --  hold_derived (#691), and its test set with the userland cap set
      --  server (#825), which are persistent, so the first checkpoint has
      --  them.

      Init.Boot_Steps.End_Boot_Cap_Sets;

      --  The new system is complete.  Once its first checkpoint is in
      --  the log, restart, so that the machine boots it from hd0 (#413).
      --  The install media's GRUB menu boots hd0 when it has Rose.

      Init.Calls.Send_String
        (Console_Stream_Cap, "install complete; restarting from hd0" & NL);
      if Take_Checkpoint then
         Wait (3000);
         Init.Calls.Send (Restart_Cap);
      end if;

      Init.Calls.Send_String
        (Console_Stream_Cap, "init: exiting" & NL);
      Init.Calls.Send (Exit_Cap);

   end Run_Init;

end Init.Run;
