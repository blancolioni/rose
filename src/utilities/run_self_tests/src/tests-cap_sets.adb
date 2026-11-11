with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;

with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Releasable_Cap_Set.Client;

with Tests.Registry;

package body Tests.Cap_Sets is

   use type Rose.Capabilities.Capability;

   Least_Filled : constant := 3_000;
   --  The sets share the server's cap table, of 4095, with each other and
   --  the server's own caps, so a set that is the only large one takes at
   --  least this many.

   ------------------
   -- Cap_Set_Test --
   ------------------

   function Cap_Set_Test return Boolean is
      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);

      procedure Fill
        (Set    : in out Rose.Interfaces.Releasable_Cap_Set.Client
                    .Releasable_Cap_Set_Client;
         Cap    : Rose.Capabilities.Capability;
         Filled : out Natural;
         Raised : out Boolean);
      --  Append copies of Cap to Set, as many as a call can carry each
      --  time, until Append raises. Filled is how many it took. Raised is
      --  False if it took far more than the server's cap table holds,
      --  without raising.

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

      ----------
      -- Fill --
      ----------

      procedure Fill
        (Set    : in out Rose.Interfaces.Releasable_Cap_Set.Client
                    .Releasable_Cap_Set_Client;
         Cap    : Rose.Capabilities.Capability;
         Filled : out Natural;
         Raised : out Boolean)
      is
         Copies : constant Rose.Capabilities.Capability_Array
           (1 .. Rose.Invocation.Max_Capabilities) := (others => Cap);
      begin
         Filled := 0;
         Raised := False;
         while Filled < 8_192 loop
            begin
               Set.Append (Copies);
            exception
               when others =>
                  Raised := True;
                  return;
            end;
            Filled := Filled + Copies'Length;
         end loop;
      end Fill;

      Cap      : constant Rose.Capabilities.Capability := Tests.Registry.Clock;
      Filled   : Natural;
      Refilled : Natural;
      Raised   : Boolean;
   begin
      Ada.Text_IO.Put ("Cap set test");

      if Tests.Registry.Cap_Sets = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no cap set server)");
         return True;
      end if;

      if Cap = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no cap to put in a set)");
         return True;
      end if;

      declare
         use Rose.Interfaces.Releasable_Cap_Set.Client;
         Constructor : Rose.Interfaces.Constructor.Client.Constructor_Client
           := Rose.Interfaces.Constructor.Client.From_Cap
                (Tests.Registry.Cap_Sets);
         Set         : Releasable_Cap_Set_Client :=
                         From_Cap (Constructor.Create);
      begin
         Check ("a new set is not empty", Set.Length = 0);

         Fill (Set, Cap, Filled, Raised);
         Check ("a full set did not raise", Raised);
         Check ("a set held only" & Natural'Image (Filled) & " caps",
                Filled >= Least_Filled);
         Check ("the Append that raised added caps",
                Set.Length = Filled);
         Check ("the first cap did not come back",
                Set.Get_Cap (1) /= Rose.Capabilities.Null_Capability);

         begin
            declare
               Past : constant Rose.Capabilities.Capability :=
                        Set.Get_Cap (Filled + 1);
            begin
               Check ("a cap came back from past the end",
                      Past = Rose.Capabilities.Null_Capability);
            end;
         exception
            when others =>
               null;
         end;

         Set.Release;

         begin
            declare
               Length : constant Natural := Set.Length;
            begin
               Check ("a released set answered, with a length of"
                      & Natural'Image (Length), False);
            end;
         exception
            when others =>
               null;
         end;

         --  Releasing the set deleted its caps, so a new one has the room
         --  that it had.

         declare
            Again : Releasable_Cap_Set_Client :=
                      From_Cap (Constructor.Create);
         begin
            Fill (Again, Cap, Refilled, Raised);
            Check ("the second full set did not raise", Raised);
            Check ("the second set held" & Natural'Image (Refilled)
                   & " caps, after" & Natural'Image (Filled),
                   Refilled >= Filled);
            Again.Release;
         end;
      end;

      return Passed;
   end Cap_Set_Test;

end Tests.Cap_Sets;
