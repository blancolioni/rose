with Ada.Text_IO;

with Rose.Capabilities;

with Rose.Interfaces.Caps.Bank_Constructor.Client;
with Rose.Interfaces.Caps.Constructor.Client;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Caps.Releasable_Cap_Set.Client;

with Tests.Registry;

package body Tests.Cap_Sets is

   use type Rose.Capabilities.Capability;

   Least_Filled : constant := 3_000;
   --  The sets share the server's cap table, of 4095, with each other and
   --  the server's own caps, so a set that is the only large one takes at
   --  least this many.

   -------------------
   -- Bank_Set_Test --
   -------------------

   function Bank_Set_Test return Boolean is
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

      Cap : constant Rose.Capabilities.Capability := Tests.Registry.Clock;
   begin
      Ada.Text_IO.Put ("Bank cap set test");

      if Tests.Registry.Bank_Cap_Sets = Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put (" (skipped: no cap set server)");
         return True;
      end if;

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      if Cap = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no cap to put in a set)");
         return True;
      end if;

      declare
         use Rose.Interfaces.Caps.Releasable_Cap_Set.Client;
         subtype Bank_Class is Rose.Interfaces.Storage.Bank.Bank_Object'Class;
         Constructor : Rose.Interfaces.Caps.Bank_Constructor.Client
           .Bank_Constructor_Client
           := Rose.Interfaces.Caps.Bank_Constructor.Client.From_Cap
                (Tests.Registry.Bank_Cap_Sets);
         Test_Bank   : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                         Rose.Interfaces.Storage.Bank.Client.From_Cap
                           (Tests.Registry.Bank);
         Doomed      : Bank_Class := Test_Bank.Create_Sub_Bank (0, 0);
         Kept        : Bank_Class := Test_Bank.Create_Sub_Bank (0, 0);
         Lost_Set    : Releasable_Cap_Set_Client :=
                         From_Cap (Constructor.Create_In (Doomed));
         Kept_Set    : Releasable_Cap_Set_Client :=
                         From_Cap (Constructor.Create_In (Kept));
      begin
         Lost_Set.Append (Cap);
         Kept_Set.Append (Cap);
         Check ("a set in a bank doesn't hold its cap",
                Lost_Set.Length = 1 and then Kept_Set.Length = 1);

         --  Destroying the bank makes the storage server signal the cap
         --  set server, which releases the set before it answers the
         --  next call.

         Doomed.Destroy;

         begin
            declare
               Length : constant Natural := Lost_Set.Length;
            begin
               Check ("the set answered after its bank was destroyed,"
                      & " with a length of" & Natural'Image (Length),
                      False);
            end;
         exception
            when others =>
               null;
         end;

         Check ("the set in another bank lost its cap",
                Kept_Set.Length = 1
                and then Kept_Set.Get_Cap (1)
                  /= Rose.Capabilities.Null_Capability);

         begin
            declare
               Late : Releasable_Cap_Set_Client :=
                        From_Cap (Constructor.Create_In (Doomed));
            begin
               Check ("a set was made in a destroyed bank", False);
               Late.Release;
            end;
         exception
            when others =>
               null;
         end;

         Kept_Set.Release;
         Kept.Destroy;
      end;

      return Passed;
   end Bank_Set_Test;

   ------------------
   -- Cap_Set_Test --
   ------------------

   function Cap_Set_Test return Boolean is
      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);

      procedure Fill
        (Set    : in out Rose.Interfaces.Caps.Releasable_Cap_Set.Client
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
        (Set    : in out Rose.Interfaces.Caps.Releasable_Cap_Set.Client
                    .Releasable_Cap_Set_Client;
         Cap    : Rose.Capabilities.Capability;
         Filled : out Natural;
         Raised : out Boolean)
      is
      begin
         Filled := 0;
         Raised := False;
         while Filled < 8_192 loop
            begin
               Set.Append (Cap);
            exception
               when others =>
                  Raised := True;
                  return;
            end;
            Filled := Filled + 1;
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
         use Rose.Interfaces.Caps.Releasable_Cap_Set.Client;
         Constructor : Rose.Interfaces.Caps.Constructor.Client
           .Constructor_Client
           := Rose.Interfaces.Caps.Constructor.Client.From_Cap
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
