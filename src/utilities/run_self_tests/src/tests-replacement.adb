with Ada.Streams;
with Ada.Text_IO;
with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Launch;
with Rose.Interfaces.Named.Client;
with Rose.Interfaces.Releasable_Cap_Set;
with Rose.Interfaces.Releasable_Cap_Set.Client;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Replaceable.Client;
with Rose.Interfaces.Saveable.Client;

with Tests.Registry;

package body Tests.Replacement is

   use type Ada.Streams.Stream_Element_Array;
   use type Rose.Capabilities.Capability;

   Name_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  The buffer of the launch call, which holds the name. The kernel
   --  shares a buffer by its pages, and the receiver sees it at the start
   --  of the first, so it starts a page.

   procedure Start
     (Name     : String;
      Previous : Rose.Capabilities.Capability;
      Saved    : Rose.Capabilities.Capability;
      Served   : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Saver    : out Rose.Capabilities.Capability);
   --  Launch replaceable_test as a command, with Name, and with Previous
   --  and Saved if they are caps, and return its Named, Replaceable and
   --  Saveable interfaces, or null caps if the launch or its call failed.

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

      Served_1, Control_1, Saver_1 : Rose.Capabilities.Capability;
      Served_2, Control_2, Saver_2 : Rose.Capabilities.Capability;
      Null_Cap : constant Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
   begin
      Ada.Text_IO.Put ("Replacement test");

      if Tests.Registry.Cap_Sets = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no cap set server)");
         return True;
      end if;

      Start ("fred", Null_Cap, Null_Cap, Served_1, Control_1, Saver_1);
      if Served_1 = Rose.Capabilities.Null_Capability
        or else Control_1 = Rose.Capabilities.Null_Capability
        or else Saver_1 = Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put (": cannot start replaceable_test");
         return False;
      end if;

      declare
         Named_1 : Rose.Interfaces.Named.Client.Named_Client :=
           Rose.Interfaces.Named.Client.From_Cap (Served_1);
      begin
         declare
            Name : constant String := Named_1.Name;
         begin
            Check ("name " & Name, Name = "fred 0");
         end;

         --  Reading the state twice gives the same state, and doesn't
         --  change what the server serves.

         declare
            First  : constant Ada.Streams.Stream_Element_Array :=
              State_Of (Control_1);
            Second : constant Ada.Streams.Stream_Element_Array :=
              State_Of (Control_1);
         begin
            Check ("no state", First'Length > 8);
            Check ("state changed",
                   After_Identifiers (First) = After_Identifiers (Second));
         end;

         declare
            Name : constant String := Named_1.Name;
         begin
            Check ("then " & Name, Name = "fred 1");
         end;

         --  The caps are the one it holds, the cap set server's
         --  Constructor, which works.

         declare
            Old : Rose.Interfaces.Replaceable.Client.Replaceable_Client :=
              Rose.Interfaces.Replaceable.Client.From_Cap (Control_1);
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

         --  A second one takes over from the first, and starts from its
         --  name and count, not from the name that it is given.

         Start ("anne", Control_1, Null_Cap, Served_2, Control_2, Saver_2);
         Check ("the new version did not start",
                Served_2 /= Rose.Capabilities.Null_Capability
                and then Control_2 /= Rose.Capabilities.Null_Capability);

         if Served_2 /= Rose.Capabilities.Null_Capability then
            declare
               Named_2 : Rose.Interfaces.Named.Client.Named_Client :=
                 Rose.Interfaces.Named.Client.From_Cap (Served_2);
               Name    : constant String := Named_2.Name;
            begin
               Check ("new " & Name, Name = "fred 2");
            end;
         end if;

         declare
            Name : constant String := Named_1.Name;
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
              Rose.Interfaces.Saveable.Client.From_Cap (Saver_1);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
            Served_3, Control_3, Saver_3 : Rose.Capabilities.Capability;
         begin
            Start ("anne", Null_Cap, Saved.Reader_Cap,
                   Served_3, Control_3, Saver_3);
            Saved.Release;
            Check ("the restored one did not start",
                   Served_3 /= Null_Cap and then Control_3 /= Null_Cap);

            if Served_3 /= Null_Cap then
               declare
                  Named_3 : Rose.Interfaces.Named.Client.Named_Client :=
                    Rose.Interfaces.Named.Client.From_Cap (Served_3);
                  Name    : constant String := Named_3.Name;
               begin
                  Check ("restored " & Name, Name = "fred 3");
               end;
            end if;

            if Control_3 /= Null_Cap then
               declare
                  Restored : Rose.Interfaces.Replaceable.Client
                    .Replaceable_Client :=
                      Rose.Interfaces.Replaceable.Client.From_Cap (Control_3);
                  Set      : Rose.Interfaces.Releasable_Cap_Set
                    .Releasable_Cap_Set_Object'Class := Restored.Caps;
               begin
                  Check ("the restored one has no Cap_Set_Service",
                         Set.Length = 1);
                  Set.Release;
               end;
            end if;
         end;

         declare
            Name : constant String := Named_1.Name;
         begin
            Check ("first after restore " & Name, Name = "fred 3");
         end;

         --  Both a version to take over and a saved state fail the
         --  launch, and so does a stream that isn't a state: here, a state
         --  without its first word.

         declare
            Saveable : Rose.Interfaces.Saveable.Client.Saveable_Client :=
              Rose.Interfaces.Saveable.Client.From_Cap (Saver_1);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
            Served_4, Control_4, Saver_4 : Rose.Capabilities.Capability;
         begin
            Start ("both", Control_1, Saved.Reader_Cap,
                   Served_4, Control_4, Saver_4);
            Check ("both a version and a state started",
                   Served_4 = Null_Cap);
            Saved.Release;
         end;

         declare
            Saveable : Rose.Interfaces.Saveable.Client.Saveable_Client :=
              Rose.Interfaces.Saveable.Client.From_Cap (Saver_1);
            Saved    : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := Saveable.State;
            Skipped  : Ada.Streams.Stream_Element_Array (1 .. 4);
            Last     : Ada.Streams.Stream_Element_Offset;
            Served_5, Control_5, Saver_5 : Rose.Capabilities.Capability;
         begin
            Saved.Read (Skipped, Last);
            Start ("bad", Null_Cap, Saved.Reader_Cap,
                   Served_5, Control_5, Saver_5);
            Check ("a bad state started", Served_5 = Null_Cap);
            Saved.Release;
         end;
      end;

      return Passed;
   end Replacement_Test;

   -----------
   -- Start --
   -----------

   procedure Start
     (Name     : String;
      Previous : Rose.Capabilities.Capability;
      Saved    : Rose.Capabilities.Capability;
      Served   : out Rose.Capabilities.Capability;
      Control  : out Rose.Capabilities.Capability;
      Saver    : out Rose.Capabilities.Capability)
   is
      use System.Storage_Elements;
      use type Rose.Words.Word_32;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Launch_Object'Class :=
        Tests.Registry.Launcher ("replaceable_test");
      Endpoint : constant Rose.Capabilities.Capability :=
        Launcher.Launch_Command (Rose.System_Calls.Own_Bank.Cap, No_Caps);
      Padded   : constant Storage_Offset :=
        Storage_Offset ((Name'Length + 3) / 4 * 4);
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      Served := Rose.Capabilities.Null_Capability;
      Control := Rose.Capabilities.Null_Capability;
      Saver := Rose.Capabilities.Null_Capability;

      if Endpoint = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  The launch call: the create endpoint cap, the cap set server's
      --  Constructor, the version to take over from and the saved state,
      --  each if there is one, and the name as one buffer item, its
      --  length little-endian and then its text, padded to four bytes. A
      --  saved state without a version sends a null cap in the version's
      --  place, as petal does.

      Name_Buffer (1 .. 4 + Padded) := (others => 0);
      Name_Buffer (1) := Storage_Element (Name'Length);
      for I in Name'Range loop
         Name_Buffer (Storage_Offset (5 + I - Name'First)) :=
           Character'Pos (Name (I));
      end loop;

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Tests.Registry.Cap_Sets);
      if Previous /= Rose.Capabilities.Null_Capability
        or else Saved /= Rose.Capabilities.Null_Capability
      then
         Rose.System_Calls.Send_Cap (Params, Previous);
      end if;
      if Saved /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Send_Cap (Params, Saved);
      end if;
      Rose.System_Calls.Send_Buffer
        (Params, 4 + Padded, Name_Buffer'Address, False);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 3);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= 0
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return;
      end if;

      Served := Params.Caps (0);
      Control := Params.Caps (1);
      Saver := Params.Caps (2);
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

end Tests.Replacement;
