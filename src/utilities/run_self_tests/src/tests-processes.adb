with Rose.System_Calls.Own_Bank;
with Ada.Text_IO;

with Ada.Streams;
with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Storage.Bank;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Processes.Launch;
with Rose.Interfaces.Common.Named.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Processes.Process_Control;
with Rose.Interfaces.Processes.Process_Control.Client;
with Rose.Interfaces.Files.Writable_File.Client;

with Tests.Files;
with Tests.Registry;

package body Tests.Processes is

   use Ada.Streams;
   use System.Storage_Elements;

   package Files renames Rose.Interfaces.Files.Writable_File.Client;

   Hello : constant Stream_Element_Array (1 .. 5) :=
     (Character'Pos ('h'), Character'Pos ('e'), Character'Pos ('l'),
      Character'Pos ('l'), Character'Pos ('o'));

   function Answers
     (Interface_Cap : Rose.Capabilities.Capability;
      Operation     : Natural)
      return Boolean;
   --  True if the object that Interface_Cap names gives a cap for its
   --  operation number Operation, counted from 0 in the order of its
   --  interface. It asks for that one cap, as a client does when it
   --  loads its caps (idle#101).

   function Fail (Message : String) return Boolean;
   --  Report Message, and fail the test.

   -------------
   -- Answers --
   -------------

   function Answers
     (Interface_Cap : Rose.Capabilities.Capability;
      Operation     : Natural)
      return Boolean
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Send_Word (Params, Natural (0));
      Rose.System_Calls.Send_Word (Params, Operation);
      Rose.System_Calls.Send_Word (Params, Natural (1));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return False;
      end if;

      Rose.System_Calls.Delete_Cap (Params.Caps (0));
      return True;
   end Answers;

   -------------------
   -- Bank_End_Test --
   -------------------

   function Bank_End_Test return Boolean is
      use type Rose.Capabilities.Capability;
      Own     : constant Rose.Capabilities.Capability :=
                  Rose.System_Calls.Own_Bank.Cap;
   begin
      Ada.Text_IO.Put ("Bank end test");

      if Own = Rose.Capabilities.Null_Capability then
         return Fail ("no bank of its own");
      end if;

      declare
         Own_Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                      Rose.Interfaces.Storage.Bank.Client.From_Cap (Own);
         Sub_Bank : Rose.Interfaces.Storage.Bank.Bank_Object'Class :=
                      Own_Bank.Create_Sub_Bank (0, 0);
         File     : Files.Writable_File_Client;
      begin
         File.Open (Tests.Registry.Launch_File (Sub_Bank.Bank_Cap));
         if File.Get_Interface_Cap = Rose.Capabilities.Null_Capability then
            return Fail ("no file server");
         end if;
         Tests.Files.Append (File, Hello);
         if File.Size /= Hello'Length then
            return Fail ("the file doesn't work");
         end if;

         --  Destroying the bank ends the process that was launched
         --  against it, without asking it.

         Sub_Bank.Destroy;

         begin
            Ada.Text_IO.Put (Storage_Count'Image (File.Size));
            return Fail ("the file answers after its bank was destroyed");
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;
      end;

      return True;
   end Bank_End_Test;

   ----------------------
   -- End_Process_Test --
   ----------------------

   function End_Process_Test return Boolean is
      Launcher : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("process_probe");

      function Launch_Probe
        return Rose.Interfaces.Processes.Process_Control
          .Process_Control_Object'Class
      is (Launcher.Launch (Rose.System_Calls.Own_Bank.Cap));
      --  A probe, which is launched with Launch and so has a
      --  Process_Control cap, as a command doesn't (#831).

      First : Rose.Interfaces.Processes.Process_Control
        .Process_Control_Object'Class := Launch_Probe;
      Probe : Rose.Interfaces.Common.Named.Client.Named_Client;
   begin
      Ada.Text_IO.Put ("End process test");

      Probe.Open (First.Published_Interface);
      if Probe.Name /= "probe" then
         return Fail ("the probe doesn't work");
      end if;

      --  The launcher holds the probe's Process_Control cap, not the
      --  probe's own Process cap (#99). Process_Control has three
      --  operations, Published_Interface, Destroy and Get_Object_Id, so
      --  the cap answers for the third but not for a fourth: it can't
      --  change the probe's segments, or exit or publish as the probe.

      if not Answers (First.Process_Control_Cap, 2)
        or else Answers (First.Process_Control_Cap, 3)
      then
         return Fail ("the launcher holds more than a Process_Control cap");
      end if;

      --  The launcher ends the probe's process. The caps that are held
      --  for it then fail (#320).

      First.Destroy;

      begin
         Ada.Text_IO.Put (" " & Probe.Name);
         return Fail ("the probe answers after its process ended");
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            null;
      end;

      --  A process launched now can take the ended one's place in mem's
      --  tables, which is what a Process cap names. Ending the old one
      --  again must not reach it.

      declare
         Second       : Rose.Interfaces.Processes.Process_Control
           .Process_Control_Object'Class :=
           Launch_Probe;
         Second_Probe : Rose.Interfaces.Common.Named.Client.Named_Client;
      begin
         Second_Probe.Open (Second.Published_Interface);

         begin
            First.Destroy;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               null;
         end;

         begin
            if Second_Probe.Name /= "probe" then
               return Fail ("the second probe doesn't work");
            end if;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               return Fail ("the old process cap ended a new process");
         end;

         Second.Destroy;
      end;

      return True;
   end End_Process_Test;

   ----------
   -- Fail --
   ----------

   function Fail (Message : String) return Boolean is
   begin
      Ada.Text_IO.Put (": ");
      Ada.Text_IO.Put (Message);
      return False;
   end Fail;

   ------------------------
   -- Process_Limit_Test --
   ------------------------

   function Process_Limit_Test return Boolean is
      use type Rose.Capabilities.Capability;
      package Control renames
        Rose.Interfaces.Processes.Process_Control.Client;

      Max_Probes : constant := 300;
      --  More than the system can run, at 64 processes or at 256 (#913).

      Probes     : array (1 .. Max_Probes) of Control.Process_Control_Client;
      Count      : Natural := 0;
      Launcher   : Rose.Interfaces.Processes.Launch.Launch_Object'Class :=
                     Tests.Registry.Launcher ("process_probe");

      function Launch_Probe return Rose.Capabilities.Capability;
      --  The Process_Control cap of a new probe, or the null cap if the
      --  launch fails.

      ------------------
      -- Launch_Probe --
      ------------------

      function Launch_Probe return Rose.Capabilities.Capability is
      begin
         declare
            Probe : Rose.Interfaces.Processes.Process_Control
              .Process_Control_Object'Class :=
                Launcher.Launch (Rose.System_Calls.Own_Bank.Cap);
         begin
            return Probe.Process_Control_Cap;
         end;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Rose.Capabilities.Null_Capability;
      end Launch_Probe;

   begin
      Ada.Text_IO.Put ("Process limit test");

      while Count < Max_Probes loop
         declare
            Cap : constant Rose.Capabilities.Capability := Launch_Probe;
         begin
            exit when Cap = Rose.Capabilities.Null_Capability;
            Count := Count + 1;
            Probes (Count).Open (Cap);
         end;
      end loop;

      Ada.Text_IO.Put (":" & Natural'Image (Count) & " probes");

      for Probe of Probes (1 .. Count) loop
         Probe.Destroy;
      end loop;

      if Count = Max_Probes then
         return Fail ("never ran out of processes");
      end if;

      --  Ending them makes room again.

      declare
         Cap   : constant Rose.Capabilities.Capability := Launch_Probe;
         Again : Control.Process_Control_Client;
      begin
         if Cap = Rose.Capabilities.Null_Capability then
            return Fail ("no launch after ending the probes");
         end if;
         Again.Open (Cap);
         Again.Destroy;
      end;

      return True;
   end Process_Limit_Test;

end Tests.Processes;
