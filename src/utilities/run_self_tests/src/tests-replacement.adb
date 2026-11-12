with Ada.Streams;
with Ada.Text_IO;
with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Bank;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Held_Call.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Named.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Cap_Set;
with Rose.Interfaces.Releasable_Cap_Set.Client;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Writer;
with Rose.Interfaces.Replaceable.Client;
with Rose.Interfaces.Replacement;
with Rose.Interfaces.Replacement.Client;
with Rose.Interfaces.Replacer.Client;
with Rose.Interfaces.Saveable.Client;
with Rose.Interfaces.Transactional_Writable_File;
with Rose.Interfaces.Writable_File.Client;

with Tests.Registry;

package body Tests.Replacement is

   use type Ada.Streams.Stream_Element_Array;
   use type Rose.Capabilities.Capability;

   Name_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  The buffer of the launch call, which holds the name. The kernel
   --  shares a buffer by its pages, and the receiver sees it at the start
   --  of the first, so it starts a page.

   type Started is
      record
         Served  : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Control : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Saver   : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Held    : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
      end record;
   --  The Named, Replaceable, Saveable and Held_Call interfaces of a
   --  server.

   Null_Cap : constant Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   function Start
     (Name        : String;
      Previous    : Rose.Capabilities.Capability := Null_Cap;
      Saved       : Rose.Capabilities.Capability := Null_Cap;
      Target      : Rose.Capabilities.Capability := Null_Cap;
      Bank        : Rose.Capabilities.Capability := Null_Cap;
      Replacement : Rose.Capabilities.Capability := Null_Cap)
      return Started;
   --  Launch replaceable_test as a command, with Name, with Previous and
   --  Saved if they are caps, and with Target, which makes it a helper
   --  that waits on Target, and return its interfaces, or null caps if
   --  the launch or its call failed. It is launched against Bank, or the
   --  test's own bank if Bank is null. If Replacement isn't null, it is
   --  the new version of that replacement, which is told of it (Allow)
   --  before its launch call.

   function Is_Started (Server : Started) return Boolean
   is (Server.Served /= Rose.Capabilities.Null_Capability
       and then Server.Control /= Rose.Capabilities.Null_Capability
       and then Server.Saver /= Rose.Capabilities.Null_Capability
       and then Server.Held /= Rose.Capabilities.Null_Capability);

   function Name_Of (Server : Started) return String;
   --  What Server's Named interface says.

   function Wait_For_Caller
     (Server : Started;
      Count  : Positive := 1)
      return Boolean;
   --  Ask Server how many calls of Wait it has deferred until it has
   --  Count of them,
   --  which gives a helper started for it time to call. False if it never
   --  has one. A helper whose call has just failed takes a while to call
   --  again, since it unwinds an exception, and it runs only when a timer
   --  interrupt takes the processor from this test and its server, which
   --  call each other in turn, so the limit is high.

   function State_Of
     (Control : Rose.Capabilities.Capability)
      return Ada.Streams.Stream_Element_Array;
   --  The whole state, through the Replaceable interface Control.

   function After_Identifiers
     (State : Ada.Streams.Stream_Element_Array)
      return Ada.Streams.Stream_Element_Array;
   --  State without its header and identifiers. Each State's reader is
   --  an object with an identifier, and a released reader's identifier is
   --  retired, not given out again (#769), so the next state has one more
   --  identifier in use, which a new version must not give out either.
   --  The rest is the server's, and doesn't change.

   -----------------------
   -- After_Identifiers --
   -----------------------

   function After_Identifiers
     (State : Ada.Streams.Stream_Element_Array)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
      Header : constant Stream_Element_Offset := 8;
      --  Rose.Replacement's header and version.
      Count  : Stream_Element_Offset := 0;
      Start  : Stream_Element_Offset;
   begin
      if State'Length < Header + 4 then
         return State;
      end if;

      --  The number of identifiers, a Natural, little-endian, and then
      --  each identifier in two bytes.

      for I in reverse Stream_Element_Offset range 1 .. 4 loop
         Count := Count * 256
           + Stream_Element_Offset (State (State'First + Header + I - 1));
      end loop;
      Start := State'First + Header + 4 + 2 * Count;
      if Start > State'Last + 1 then
         return State;
      end if;
      return State (Start .. State'Last);
   end After_Identifiers;

   -------------
   -- Name_Of --
   -------------

   function Name_Of (Server : Started) return String is
      Named : Rose.Interfaces.Named.Client.Named_Client :=
        Rose.Interfaces.Named.Client.From_Cap (Server.Served);
   begin
      return Named.Name;
   exception
      when others =>
         return "(no name)";
   end Name_Of;

   -----------------------
   -- Replace_File_Test --
   -----------------------

   function Replace_File_Test return Boolean is
      use Ada.Streams;
      use System.Storage_Elements;

      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);

      function Bytes (Text : String) return Stream_Element_Array;

      function Read_All
        (Reader : in out Rose.Interfaces.Releasable_Reader
           .Releasable_Reader_Object'Class)
         return String;
      --  What Reader reads until it reaches the end.

      -----------
      -- Bytes --
      -----------

      function Bytes (Text : String) return Stream_Element_Array is
         Result : Stream_Element_Array (1 .. Text'Length);
      begin
         for I in Text'Range loop
            Result (Stream_Element_Offset (I - Text'First + 1)) :=
              Character'Pos (Text (I));
         end loop;
         return Result;
      end Bytes;

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

      --------------
      -- Read_All --
      --------------

      function Read_All
        (Reader : in out Rose.Interfaces.Releasable_Reader
           .Releasable_Reader_Object'Class)
         return String
      is
         Buffer : Stream_Element_Array (1 .. 64);
         Last   : Stream_Element_Offset;
         Result : String (1 .. 256);
         Length : Natural := 0;
      begin
         loop
            Reader.Read (Buffer, Last);
            exit when Last < Buffer'First;
            for Item of Buffer (Buffer'First .. Last) loop
               exit when Length = Result'Last;
               Length := Length + 1;
               Result (Length) := Character'Val (Item);
            end loop;
         end loop;
         return Result (1 .. Length);
      end Read_All;

   begin
      Ada.Text_IO.Put ("Replace file test");

      if Tests.Registry.Replacer = Null_Cap
        or else Tests.Registry.Cap_Sets = Null_Cap
      then
         Ada.Text_IO.Put (" (skipped: no Replacer)");
         return True;
      end if;

      --  As in Replace_Process_Test, everything is in a bank of the
      --  test's own, which it destroys at the end (#724).

      declare
         Own       : Rose.Interfaces.Bank.Client.Bank_Client :=
           Rose.Interfaces.Bank.Client.From_Cap
             (Rose.System_Calls.Own_Bank.Cap);
         Test_Bank : Rose.Interfaces.Bank.Bank_Object'Class :=
           Own.Create_Sub_Bank (0, 0);
         Replacer  : Rose.Interfaces.Replacer.Client.Replacer_Client :=
           Rose.Interfaces.Replacer.Client.From_Cap (Tests.Registry.Replacer);
         Endpoint  : constant Rose.Capabilities.Capability :=
           Tests.Registry.File_Endpoint (Test_Bank.Bank_Cap);
         Served    : Rose.Capabilities.Capability := Null_Cap;
         Control   : Rose.Capabilities.Capability := Null_Cap;
         File      : Rose.Interfaces.Writable_File.Client
           .Writable_File_Client;
      begin
         if Endpoint /= Null_Cap then
            begin
               Tests.Registry.Start_File (Endpoint, Served, Control);
            exception
               when Rose.Interfaces.Objects.Invocation_Error =>
                  null;
            end;
         end if;

         if Served = Null_Cap or else Control = Null_Cap then
            Test_Bank.Destroy;
            Ada.Text_IO.Put (": the file did not start");
            return False;
         end if;

         File.Open (Served);

         declare
            First   : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class := File.Rewrite;
            Writer  : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class := File.Append;
            Reader  : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := File.Open_Reader;
            Start   : Stream_Element_Array (1 .. 2);
            Last    : Stream_Element_Offset;
         begin
            First.Write (Bytes ("hello"));
            First.Release;
            Reader.Read (Start, Last);
            Check ("the reader did not start", Last = Start'Last);

            --  A version, with a writer of its own, which is open across
            --  the handover too.

            declare
               Version        : Rose.Interfaces.Transactional_Writable_File
                 .Transactional_Writable_File_Object'Class :=
                   File.New_Version;
               Version_Writer : Rose.Interfaces.Releasable_Writer
                 .Releasable_Writer_Object'Class := Version.Append;
               Replacement    : Rose.Interfaces.Replacement
                 .Replacement_Object'Class :=
                   Replacer.Replace_Process (Control);
               Committed      : Boolean := False;
            begin
               Check ("no replacement",
                      Replacement.Replacement_Cap /= Null_Cap);

               if Replacement.Replacement_Cap /= Null_Cap then
                  declare
                     Ready       : Boolean := False;
                     Sibling     : Rose.Interfaces.Bank.Bank_Object'Class :=
                       Replacement.Bank;
                     New_Version : Rose.Capabilities.Capability;
                     New_Served  : Rose.Capabilities.Capability;
                     New_Control : Rose.Capabilities.Capability;
                  begin
                     for Attempt in 1 .. 100_000 loop
                        Ready := Replacement.Ready;
                        exit when Ready;
                     end loop;
                     Check ("the old version is never ready", Ready);

                     New_Version :=
                       Tests.Registry.File_Endpoint (Sibling.Bank_Cap);
                     Check ("the new version did not launch",
                            New_Version /= Null_Cap);

                     if Ready and then New_Version /= Null_Cap then
                        Replacement.Allow (New_Version);
                        begin
                           Tests.Registry.Start_File
                             (New_Version, New_Served, New_Control,
                              Previous => Control);
                           Committed := Replacement.Commit;
                        exception
                           when Rose.Interfaces.Objects.Invocation_Error =>
                              Check ("the new version refused", False);
                        end;
                     end if;
                  end;

                  Check ("the replacement did not commit", Committed);
                  Replacement.Release;
               end if;

               --  Until a replacement commits or is released, the old
               --  version's calls are held, so nothing is asked of the
               --  file unless it committed.

               if Committed then
                  Writer.Write (Bytes (" world"));
                  Check ("the reader did not go on",
                         Read_All (Reader) = "llo world");
                  Check ("the size is wrong", File.Size = 11);

                  Version_Writer.Write (Bytes ("!"));
                  Version.Commit;
                  Check ("the version did not commit",
                         File.Size = 6);

                  declare
                     Again : Rose.Interfaces.Releasable_Reader
                       .Releasable_Reader_Object'Class := File.Open_Reader;
                  begin
                     Check ("the file is not the version's",
                            Read_All (Again) = "hello!");
                     Again.Release;
                  end;

                  Version_Writer.Release;
                  Version.Release;
                  Writer.Release;
                  Reader.Release;
               end if;
            end;
         exception
            when Rose.Interfaces.Objects.Invocation_Error =>
               Check ("a call to the file failed", False);
         end;

         Test_Bank.Destroy;
      end;

      return Passed;
   end Replace_File_Test;

   --------------------------
   -- Replace_Process_Test --
   --------------------------

   function Replace_Process_Test return Boolean is
      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);

      function Wait_Until_Ready
        (Replacement : in out Rose.Interfaces.Replacement
           .Replacement_Object'Class)
         return Boolean;
      --  Ask Replacement until the old version is between calls.

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

      ----------------------
      -- Wait_Until_Ready --
      ----------------------

      function Wait_Until_Ready
        (Replacement : in out Rose.Interfaces.Replacement
           .Replacement_Object'Class)
         return Boolean
      is
      begin
         for Attempt in 1 .. 100_000 loop
            if Replacement.Ready then
               return True;
            end if;
         end loop;
         return False;
      end Wait_Until_Ready;

   begin
      Ada.Text_IO.Put ("Replace process test");

      if Tests.Registry.Replacer = Null_Cap
        or else Tests.Registry.Cap_Sets = Null_Cap
      then
         Ada.Text_IO.Put (" (skipped: no Replacer)");
         return True;
      end if;

      --  Everything runs in a bank of the test's own, which ends every
      --  process launched in it when it is destroyed, so that the test
      --  leaves no servers behind (#724).

      declare
         Own       : Rose.Interfaces.Bank.Client.Bank_Client :=
           Rose.Interfaces.Bank.Client.From_Cap
             (Rose.System_Calls.Own_Bank.Cap);
         Test_Bank : Rose.Interfaces.Bank.Bank_Object'Class :=
           Own.Create_Sub_Bank (0, 0);
         In_Bank   : constant Rose.Capabilities.Capability :=
           Test_Bank.Bank_Cap;
         Replacer  : Rose.Interfaces.Replacer.Client.Replacer_Client :=
           Rose.Interfaces.Replacer.Client.From_Cap (Tests.Registry.Replacer);
      begin
         Check ("no test bank", In_Bank /= Null_Cap);

         --  A replacement that commits.

         declare
            Old_Version : constant Started :=
              Start ("gina", Bank => In_Bank);
         begin
            Check ("the old version did not start",
                   Is_Started (Old_Version));

            if Is_Started (Old_Version) then
               declare
                  Name : constant String := Name_Of (Old_Version);
               begin
                  Check ("old " & Name, Name = "gina 0");
               end;

               declare
                  Deferred    : constant Started :=
                    Start ("helper", Target => Old_Version.Held,
                           Bank => In_Bank);
                  Replacement : Rose.Interfaces.Replacement
                    .Replacement_Object'Class :=
                      Replacer.Replace_Process (Old_Version.Control);
               begin
                  Check ("the deferred call was not made",
                         Is_Started (Deferred));

                  --  The helper's call is deferred before the hold, which
                  --  lets no call reach the old version but the new
                  --  version's.

                  Check ("no replacement",
                         Replacement.Replacement_Cap /= Null_Cap);

                  if Replacement.Replacement_Cap /= Null_Cap then
                     declare
                        Held_Up : constant Started :=
                          Start ("repeat", Target => Old_Version.Held,
                                 Bank => In_Bank);
                        --  Its call, to Wait_With, is made while the old
                        --  version's calls are held, and waits. It isn't
                        --  Wait, which has room for one deferred call.
                        Ready   : constant Boolean :=
                          Wait_Until_Ready (Replacement);
                        Sibling : Rose.Interfaces.Bank.Bank_Object'Class :=
                          Replacement.Bank;
                        New_Version : constant Started :=
                          Start ("ignored",
                                 Previous    => Old_Version.Control,
                                 Bank        => Sibling.Bank_Cap,
                                 Replacement =>
                                   Replacement.Replacement_Cap);
                        Committed   : Boolean;
                     begin
                        Check ("the second helper did not start",
                               Is_Started (Held_Up));
                        Check ("the old version is never ready", Ready);
                        Check ("the new version did not start",
                               Is_Started (New_Version));
                        Committed :=
                          Is_Started (New_Version)
                          and then Replacement.Commit;
                        Check ("the replacement did not commit", Committed);

                        --  Until a replacement commits or is released,
                        --  the old version's calls are held, and a call
                        --  to it would wait for good.

                        if not Committed then
                           Replacement.Release;
                           Test_Bank.Destroy;
                           return False;
                        end if;

                        --  The old version's caps reach the new version,
                        --  which took its name and count. The new
                        --  version's own launch-time caps reached the
                        --  process that has that object id now, the old
                        --  version, which has ended.

                        declare
                           Name : constant String := Name_Of (Old_Version);
                        begin
                           Check ("replaced " & Name, Name = "gina 1");
                        end;

                        declare
                           Name : constant String := Name_Of (New_Version);
                        begin
                           Check ("the retired one answered " & Name,
                                  Name = "(no name)");
                        end;

                        --  The new version answers both calls: the one it
                        --  took over, and the one made during the
                        --  handover, which it deferred in turn.

                        declare
                           Held : Rose.Interfaces.Held_Call.Client
                             .Held_Call_Client :=
                               Rose.Interfaces.Held_Call.Client.From_Cap
                                 (Old_Version.Held);
                        begin
                           Check ("the new version has not both calls",
                                  Wait_For_Caller (Old_Version, 2));
                           Held.Answer (11);
                        end;

                        declare
                           Name : constant String := Name_Of (Deferred);
                        begin
                           Check ("deferred " & Name, Name = "waited 11");
                        end;

                        declare
                           Name : constant String := Name_Of (Held_Up);
                        begin
                           Check ("held " & Name,
                                  Name = "waited 11 after 0 errors");
                        end;
                     end;

                     Replacement.Release;
                  end if;
               end;
            end if;
         end;

         --  A replacement that the new version refuses, by failing its
         --  launch, doesn't commit, and releasing it leaves the old
         --  version as it was, with the call that it deferred.

         declare
            Old_Version : constant Started :=
              Start ("refuse", Bank => In_Bank);
         begin
            Check ("the refused one did not start",
                   Is_Started (Old_Version));

            if Is_Started (Old_Version) then
               declare
                  Deferred    : constant Started :=
                    Start ("helper", Target => Old_Version.Held,
                           Bank => In_Bank);
                  Waited      : constant Boolean :=
                    Wait_For_Caller (Old_Version);
                  Replacement : Rose.Interfaces.Replacement
                    .Replacement_Object'Class :=
                      Replacer.Replace_Process (Old_Version.Control);
               begin
                  Check ("the third helper's call was not deferred",
                         Is_Started (Deferred) and then Waited);
                  Check ("no second replacement",
                         Replacement.Replacement_Cap /= Null_Cap);

                  if Replacement.Replacement_Cap /= Null_Cap then
                     declare
                        Ready   : constant Boolean :=
                          Wait_Until_Ready (Replacement);
                        Sibling : Rose.Interfaces.Bank.Bank_Object'Class :=
                          Replacement.Bank;
                     begin
                        Check ("the refused one is never ready", Ready);
                        Check ("a version took over from refuse",
                               not Is_Started
                                 (Start ("barry",
                                         Previous    => Old_Version.Control,
                                         Bank        => Sibling.Bank_Cap,
                                         Replacement =>
                                           Replacement.Replacement_Cap)));
                        Check ("a refused replacement committed",
                               not Replacement.Commit);
                     end;
                     Replacement.Release;
                  end if;

                  declare
                     Name : constant String := Name_Of (Old_Version);
                  begin
                     Check ("kept " & Name, Name = "refuse 0");
                  end;

                  declare
                     Held : Rose.Interfaces.Held_Call.Client
                       .Held_Call_Client :=
                         Rose.Interfaces.Held_Call.Client.From_Cap
                           (Old_Version.Held);
                  begin
                     Check ("the old version lost its deferred call",
                            Held.Waiting = 1);
                     Held.Answer (12);
                  end;

                  declare
                     Name : constant String := Name_Of (Deferred);
                  begin
                     Check ("kept deferred " & Name, Name = "waited 12");
                  end;
               end;
            end if;
         end;

         Test_Bank.Destroy;
      end;

      return Passed;

   exception
      when others =>
         Ada.Text_IO.Put (": raised");
         return False;
   end Replace_Process_Test;

   ----------------------
   -- Replacement_Test --
   ----------------------

   function Replacement_Test return Boolean is
      Passed : Boolean := True;

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
      Ada.Text_IO.Put ("Replacement test");

      if Tests.Registry.Cap_Sets = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no cap set server)");
         return True;
      end if;

      declare
         Old_Version : constant Started := Start ("fred");
      begin
         if not Is_Started (Old_Version) then
            Ada.Text_IO.Put (": cannot start replaceable_test");
            return False;
         end if;

         declare
            Name : constant String := Name_Of (Old_Version);
         begin
            Check ("name " & Name, Name = "fred 0");
         end;

         --  A helper waits on the old version, which defers its call, so
         --  that the old version owes a reply when it is replaced (#879).

         declare
            Helper : constant Started :=
              Start ("helper", Target => Old_Version.Held);
         begin
            Check ("the helper did not start", Is_Started (Helper));
            Check ("the helper's call was not deferred",
                   Wait_For_Caller (Old_Version));

            --  Reading the state twice gives the same state, and doesn't
            --  change what the server serves.

            declare
               First  : constant Ada.Streams.Stream_Element_Array :=
                 State_Of (Old_Version.Control);
               Second : constant Ada.Streams.Stream_Element_Array :=
                 State_Of (Old_Version.Control);
            begin
               Check ("no state", First'Length > 8);
               Check ("state changed",
                      After_Identifiers (First) = After_Identifiers (Second));
            end;

            declare
               Name : constant String := Name_Of (Old_Version);
            begin
               Check ("then " & Name, Name = "fred 1");
            end;

            --  The caps are the one it holds, the cap set server's
            --  Constructor, which works, and not the reply cap of the
            --  deferred call. Releasing them leaves the call deferred.

            declare
               Old : Rose.Interfaces.Replaceable.Client.Replaceable_Client :=
                 Rose.Interfaces.Replaceable.Client.From_Cap
                   (Old_Version.Control);
               Set : Rose.Interfaces.Releasable_Cap_Set
                 .Releasable_Cap_Set_Object'Class := Old.Caps;
            begin
               Check ("the caps are not one cap", Set.Length = 1);
               if Set.Length = 1 then
                  declare
                     Service : Rose.Interfaces.Constructor.Client
                       .Constructor_Client :=
                         Rose.Interfaces.Constructor.Client.From_Cap
                           (Set.Get_Cap (1));
                     Made    : Rose.Interfaces.Releasable_Cap_Set.Client
                       .Releasable_Cap_Set_Client :=
                         Rose.Interfaces.Releasable_Cap_Set.Client.From_Cap
                           (Service.Create);
                  begin
                     Check ("the cap-set-service cap does not work",
                            Made.Length = 0);
                     Made.Release;
                  end;
               end if;
               Set.Release;
            end;

            Check ("releasing the caps ended the deferred call",
                   Wait_For_Caller (Old_Version));

            --  A second one takes over from the first, and starts from
            --  its name and count, not from the name that it is given. It
            --  answers the call that the first one deferred.

            declare
               New_Version : constant Started :=
                 Start ("anne", Previous => Old_Version.Control);
            begin
               Check ("the new version did not start",
                      Is_Started (New_Version));

               if Is_Started (New_Version) then
                  declare
                     Name : constant String := Name_Of (New_Version);
                  begin
                     Check ("new " & Name, Name = "fred 2");
                  end;

                  declare
                     Held : Rose.Interfaces.Held_Call.Client
                       .Held_Call_Client :=
                         Rose.Interfaces.Held_Call.Client.From_Cap
                           (New_Version.Held);
                  begin
                     Check ("the new version has not the deferred call",
                            Held.Waiting = 1);
                     Held.Answer (7);
                  end;

                  declare
                     Name : constant String := Name_Of (Helper);
                  begin
                     Check ("the helper " & Name, Name = "waited 7");
                  end;
               end if;
            end;
         end;

         declare
            Name : constant String := Name_Of (Old_Version);
         begin
            Check ("old " & Name, Name = "fred 2");
         end;

         --  A third one restores the first's saved state, which it gets
         --  through the Saveable interface. It starts from the first's
         --  name and count, takes its cap from its own argument, as the
         --  state carries none, and is a new instance: the first goes on
         --  as it was (ADR 0032, section 10, #881).

         declare
            Saveable : Rose.Interfaces.Saveable.Client.Saveable_Client :=
              Rose.Interfaces.Saveable.Client.From_Cap (Old_Version.Saver);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
            Restored : constant Started :=
              Start ("anne", Saved => Saved.Reader_Cap);
         begin
            Saved.Release;
            Check ("the restored one did not start", Is_Started (Restored));

            if Is_Started (Restored) then
               declare
                  Name : constant String := Name_Of (Restored);
               begin
                  Check ("restored " & Name, Name = "fred 3");
               end;

               declare
                  Copy : Rose.Interfaces.Replaceable.Client
                    .Replaceable_Client :=
                      Rose.Interfaces.Replaceable.Client.From_Cap
                        (Restored.Control);
                  Set  : Rose.Interfaces.Releasable_Cap_Set
                    .Releasable_Cap_Set_Object'Class := Copy.Caps;
               begin
                  Check ("the restored one has no Cap_Set_Service",
                         Set.Length = 1);
                  Set.Release;
               end;
            end if;
         end;

         declare
            Name : constant String := Name_Of (Old_Version);
         begin
            Check ("first after restore " & Name, Name = "fred 3");
         end;

         --  Both a version to take over and a saved state fail the
         --  launch, and so does a stream that isn't a state: here, a state
         --  without its first word.

         declare
            Saveable : Rose.Interfaces.Saveable.Client.Saveable_Client :=
              Rose.Interfaces.Saveable.Client.From_Cap (Old_Version.Saver);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
         begin
            Check ("both a version and a state started",
                   not Is_Started
                     (Start ("both",
                             Previous => Old_Version.Control,
                             Saved    => Saved.Reader_Cap)));
            Saved.Release;
         end;

         declare
            Saveable : Rose.Interfaces.Saveable.Client.Saveable_Client :=
              Rose.Interfaces.Saveable.Client.From_Cap (Old_Version.Saver);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
            Skipped  : Ada.Streams.Stream_Element_Array (1 .. 4);
            Last     : Ada.Streams.Stream_Element_Offset;
         begin
            Saved.Read (Skipped, Last);
            Check ("a bad state started",
                   not Is_Started (Start ("bad", Saved => Saved.Reader_Cap)));
            Saved.Release;
         end;

         --  Releasing a set that holds a reply cap fails the call, rather
         --  than leaving its caller blocked for good, and frees the slot
         --  where the server has the caller's buffer. The helper's calls
         --  send a buffer, and there are more rounds than a server has
         --  slots (8), so a slot that isn't freed stops the kernel. The
         --  reply caps are given out once.

         declare
            Rounds : constant := 10;
            Helper : constant Started :=
              Start ("repeat", Target => Old_Version.Held);
            Old    : Rose.Interfaces.Replaceable.Client.Replaceable_Client :=
              Rose.Interfaces.Replaceable.Client.From_Cap
                (Old_Version.Control);
         begin
            Check ("the second helper did not start", Is_Started (Helper));

            for Round in 1 .. Rounds loop
               if not Wait_For_Caller (Old_Version) then
                  Check ("round" & Natural'Image (Round) & " not deferred",
                         False);
                  exit;
               end if;

               declare
                  Replies : Rose.Interfaces.Releasable_Cap_Set
                    .Releasable_Cap_Set_Object'Class := Old.Replies;
               begin
                  if Round = 1 then
                     Check ("the replies are not one cap",
                            Replies.Length = 1);

                     declare
                        Twice : Boolean;
                     begin
                        begin
                           declare
                              Second : Rose.Interfaces.Releasable_Cap_Set
                                .Releasable_Cap_Set_Object'Class :=
                                  Old.Replies;
                           begin
                              Second.Release;
                           end;
                           Twice := True;
                        exception
                           when others =>
                              Twice := False;
                        end;
                        Check ("the replies were given out twice",
                               not Twice);
                     end;
                  end if;

                  Replies.Release;
               end;
            end loop;

            if Wait_For_Caller (Old_Version) then
               declare
                  Held : Rose.Interfaces.Held_Call.Client.Held_Call_Client :=
                    Rose.Interfaces.Held_Call.Client.From_Cap
                      (Old_Version.Held);
               begin
                  Held.Answer (5);
               end;
            end if;

            declare
               Name : constant String := Name_Of (Helper);
            begin
               Check ("helper 2: " & Name,
                      Name = "waited 5 after 10 errors");
            end;
         end;
      end;

      --  A version that refuses to take over leaves the old one to answer
      --  the call that it deferred.

      declare
         Refused : constant Started := Start ("refuse");
      begin
         Check ("the refused version's old version did not start",
                Is_Started (Refused));

         if Is_Started (Refused) then
            declare
               Helper : constant Started :=
                 Start ("helper", Target => Refused.Held);
            begin
               Check ("the third helper did not start", Is_Started (Helper));
               Check ("the third helper's call was not deferred",
                      Wait_For_Caller (Refused));

               Check ("a version took over from refuse",
                      not Is_Started
                        (Start ("barry", Previous => Refused.Control)));

               declare
                  Held : Rose.Interfaces.Held_Call.Client.Held_Call_Client :=
                    Rose.Interfaces.Held_Call.Client.From_Cap (Refused.Held);
               begin
                  Check ("the old version lost the deferred call",
                         Held.Waiting = 1);
                  Held.Answer (9);
               end;

               declare
                  Name : constant String := Name_Of (Helper);
               begin
                  Check ("the third helper " & Name, Name = "waited 9");
               end;
            end;
         end if;
      end;

      return Passed;

   exception
      when others =>
         Ada.Text_IO.Put (": raised");
         return False;
   end Replacement_Test;

   -----------
   -- Start --
   -----------

   function Start
     (Name        : String;
      Previous    : Rose.Capabilities.Capability := Null_Cap;
      Saved       : Rose.Capabilities.Capability := Null_Cap;
      Target      : Rose.Capabilities.Capability := Null_Cap;
      Bank        : Rose.Capabilities.Capability := Null_Cap;
      Replacement : Rose.Capabilities.Capability := Null_Cap)
      return Started
   is
      use System.Storage_Elements;
      use type Rose.Words.Word_32;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("replaceable_test");
      Endpoint : constant Rose.Capabilities.Capability :=
        Launcher.Launch_Command
          ((if Bank /= Null_Cap then Bank
            else Rose.System_Calls.Own_Bank.Cap),
           No_Caps);
      Padded   : constant Storage_Offset :=
        Storage_Offset ((Name'Length + 3) / 4 * 4);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      if Endpoint = Rose.Capabilities.Null_Capability then
         return (others => <>);
      end if;

      if Replacement /= Null_Cap then
         declare
            R : Rose.Interfaces.Replacement.Client.Replacement_Client :=
              Rose.Interfaces.Replacement.Client.From_Cap (Replacement);
         begin
            R.Allow (Endpoint);
         end;
      end if;

      --  The launch call: the create endpoint cap, the cap set server's
      --  Constructor, the target, which is null unless it is a helper,
      --  the version to take over from and the saved state, each if there
      --  is one, and the name as one buffer item, its length
      --  little-endian and then its text, padded to four bytes. A saved
      --  state without a version sends a null cap in the version's place,
      --  as petal does.

      Name_Buffer (1 .. 4 + Padded) := (others => 0);
      Name_Buffer (1) := Storage_Element (Name'Length);
      for I in Name'Range loop
         Name_Buffer (Storage_Offset (5 + I - Name'First)) :=
           Character'Pos (Name (I));
      end loop;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Cap_Sets);
      Rose.System_Calls.Send_Cap (Params, Target);
      if Previous /= Null_Cap or else Saved /= Null_Cap then
         Rose.System_Calls.Send_Cap (Params, Previous);
      end if;
      if Saved /= Null_Cap then
         Rose.System_Calls.Send_Cap (Params, Saved);
      end if;
      Rose.System_Calls.Send_Buffer
        (Params, 4 + Padded, Name_Buffer'Address, False);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 4);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= 0
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return (others => <>);
      end if;

      return (Served  => Params.Caps (0),
              Control => Params.Caps (1),
              Saver   => Params.Caps (2),
              Held    => Params.Caps (3));
   end Start;

   --------------
   -- State_Of --
   --------------

   function State_Of
     (Control : Rose.Capabilities.Capability)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
      Old    : Rose.Interfaces.Replaceable.Client.Replaceable_Client :=
        Rose.Interfaces.Replaceable.Client.From_Cap (Control);
      Reader : Rose.Interfaces.Releasable_Reader
        .Releasable_Reader_Object'Class := Old.State;
      Data   : Stream_Element_Array (1 .. 4096);
      Length : Stream_Element_Offset := 0;
      Last   : Stream_Element_Offset;
   begin
      while Length < Data'Last loop
         Reader.Read (Data (Length + 1 .. Data'Last), Last);
         exit when Last <= Length;
         Length := Last;
      end loop;
      Reader.Release;
      return Data (1 .. Length);
   end State_Of;

   ---------------------
   -- Wait_For_Caller --
   ---------------------

   function Wait_For_Caller
     (Server : Started;
      Count  : Positive := 1)
      return Boolean
   is
      Held : Rose.Interfaces.Held_Call.Client.Held_Call_Client :=
        Rose.Interfaces.Held_Call.Client.From_Cap (Server.Held);
   begin
      for Attempt in 1 .. 100_000 loop
         if Held.Waiting >= Count then
            return True;
         end if;
      end loop;
      return False;
   end Wait_For_Caller;

end Tests.Replacement;
