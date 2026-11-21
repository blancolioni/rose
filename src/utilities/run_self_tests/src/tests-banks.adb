with System.Storage_Elements;

with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Server;
with Rose.System_Calls;
with Rose.Objects;
with Rose.Words;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Storage.Dependent;

with Tests.Registry;

package body Tests.Banks is

   use type Rose.Capabilities.Capability;

   Dependent_Bit : constant := 11;

   Signal_Count : Natural := 0;

   procedure On_Dependent_Bit;

   --------------------
   -- Dependent_Test --
   --------------------

   function Dependent_Test return Boolean is
      subtype Bank_Class is Rose.Interfaces.Storage.Bank.Bank_Object'Class;
      subtype Dependent_Class is
        Rose.Interfaces.Storage.Dependent.Dependent_Object'Class;
      Context : Rose.Server.Server_Context;
      Passed  : Boolean := True;
      Refused : Boolean := False;

      procedure Check (Name : String; Ok : Boolean);

      function New_Signal return Rose.Capabilities.Capability
      is (Rose.System_Calls.Create_Signal_Cap (Dependent_Bit));

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
      Ada.Text_IO.Put ("Bank dependent test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      if Rose.System_Calls.Create_Signal_Capability
        = Rose.Capabilities.Null_Capability
      then
         Ada.Text_IO.Put (" (skipped: no Create_Signal cap)");
         return True;
      end if;

      Rose.Server.Set_Signal_Handler
        (Context, Dependent_Bit, On_Dependent_Bit'Access);

      declare
         Test_Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                       Rose.Interfaces.Storage.Bank.Client.From_Cap
                         (Tests.Registry.Bank);
         Doomed    : Bank_Class := Test_Bank.Create_Sub_Bank (0, 0);
         Kept      : Bank_Class := Test_Bank.Create_Sub_Bank (0, 0);
         First     : Dependent_Class := Doomed.Add_Dependent (New_Signal);
         Second    : Dependent_Class := Kept.Add_Dependent (New_Signal);
      begin
         Check ("a new dependent says destroyed", not First.Destroyed);
         Check ("another new dependent says destroyed",
                not Second.Destroyed);

         Doomed.Destroy;

         Check ("the destroyed bank's dependent doesn't say so",
                First.Destroyed);
         Check ("another bank's dependent says destroyed",
                not Second.Destroyed);

         --  The signal was raised while nothing was receiving, so the
         --  next receive is the signal.

         Rose.Server.Receive_Message (Context);
         Check ("the dependent's signal handler was called"
                & Natural'Image (Signal_Count) & " times",
                Signal_Count = 1);

         --  A bank that has been destroyed takes no more dependents.

         begin
            declare
               Late : Dependent_Class := Doomed.Add_Dependent (New_Signal);
            begin
               Late.Release;
            end;
         exception
            when others =>
               Refused := True;
         end;
         Check ("a destroyed bank took a dependent", Refused);

         First.Release;
         Second.Release;
         Kept.Destroy;
      end;

      return Passed;
   exception
      when others =>
         Ada.Text_IO.Put (": an operation failed");
         return False;
   end Dependent_Test;

   ----------------
   -- Limit_Test --
   ----------------

   function Limit_Test return Boolean is
      use type Rose.Objects.Object_Id;
      use type Rose.Words.Word;
      use type Rose.Words.Word_64;

      subtype Bank_Class is Rose.Interfaces.Storage.Bank.Bank_Object'Class;
      package Regions renames Rose.Interfaces.Storage.Region.Client;

      Page_Size : constant := 4096;

      Data : constant System.Storage_Elements.Storage_Array
        (1 .. Page_Size) := (others => 16#5A#);
      --  A page to write. The store keeps no page that is all zero, and
      --  charges none for it.

      function Fail (Message : String) return Boolean;
      --  Report Message, and fail the test.

      function Reserve
        (Bank  : in out Bank_Class;
         Pages : Positive := 1)
         return Rose.Capabilities.Capability;
      --  The cap of a new region of Pages pages from Bank, or the null cap
      --  if the reservation fails.

      function Write
        (Region : Rose.Capabilities.Capability;
         Page   : Natural)
         return Boolean;
      --  Write Data to page Page of Region, counted from 0. False if the
      --  store refuses it.

      function Usage_Is
        (Bank         : in out Bank_Class;
         Page_Count   : Rose.Words.Word_64;
         Region_Count : Rose.Words.Word)
         return Boolean;
      --  True if Page_Count pages and Region_Count regions are charged to
      --  Bank.

      ----------
      -- Fail --
      ----------

      function Fail (Message : String) return Boolean is
      begin
         Ada.Text_IO.Put (": " & Message);
         return False;
      end Fail;

      -------------
      -- Reserve --
      -------------

      function Reserve
        (Bank  : in out Bank_Class;
         Pages : Positive := 1)
         return Rose.Capabilities.Capability
      is
      begin
         declare
            Region : Rose.Interfaces.Storage.Region.Region_Object'Class :=
                       Bank.Reserve_Storage
                         (Rose.Words.Word_64 (Pages) * Page_Size);
         begin
            return Region.Region_Cap;
         end;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return Rose.Capabilities.Null_Capability;
      end Reserve;

      --------------
      -- Usage_Is --
      --------------

      function Usage_Is
        (Bank         : in out Bank_Class;
         Page_Count   : Rose.Words.Word_64;
         Region_Count : Rose.Words.Word)
         return Boolean
      is
         Used_Pages   : Rose.Words.Word_64;
         Used_Regions : Rose.Words.Word;
         Page_Limit   : Rose.Words.Word_64;
         Region_Limit : Rose.Words.Word;
      begin
         Bank.Get_Usage (Used_Pages, Used_Regions, Page_Limit, Region_Limit);
         return Used_Pages = Page_Count and then Used_Regions = Region_Count;
      end Usage_Is;

      -----------
      -- Write --
      -----------

      function Write
        (Region : Rose.Capabilities.Capability;
         Page   : Natural)
         return Boolean
      is
         Client : Regions.Region_Client := Regions.From_Cap (Region);
         Base   : Rose.Objects.Object_Id;
         Bound  : Rose.Objects.Object_Id;
      begin
         Client.Get_Range (Base, Bound);
         Client.Put (Base + Rose.Objects.Object_Id (Page), Data);
         return True;
      exception
         when Rose.Interfaces.Objects.Invocation_Error =>
            return False;
      end Write;

      Top_Pages    : Rose.Words.Word_64;
      Top_Regions  : Rose.Words.Word;
      Page_Limit   : Rose.Words.Word_64;
      Region_Limit : Rose.Words.Word;

   begin
      Ada.Text_IO.Put ("Bank limit test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      declare
         Top : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                 Rose.Interfaces.Storage.Bank.Client.From_Cap
                   (Tests.Registry.Bank);
      begin
         Top.Get_Usage (Top_Pages, Top_Regions, Page_Limit, Region_Limit);

         --  A page limit: a bank is charged for the pages written to its
         --  regions, so two pages fit, and the third doesn't. Usage is
         --  charged to the bank above, and destroying the bank gives it
         --  back and ends the region.

         declare
            Sub    : Bank_Class := Top.Create_Sub_Bank (2, 0);
            Region : constant Rose.Capabilities.Capability :=
                       Reserve (Sub, 3);
         begin
            if Region = Rose.Capabilities.Null_Capability then
               return Fail ("a reservation under a page limit failed");
            end if;

            if not Write (Region, 0) or else not Write (Region, 1) then
               return Fail ("a write within the page limit failed");
            end if;

            if Write (Region, 2) then
               return Fail ("a write over the page limit succeeded");
            end if;

            if not Usage_Is (Sub, 2, 1) then
               return Fail ("the usage of the bank is wrong");
            end if;

            if not Usage_Is (Top, Top_Pages + 2, Top_Regions + 1) then
               return Fail ("the usage isn't charged to the bank above");
            end if;

            Sub.Destroy;

            if not Usage_Is (Top, Top_Pages, Top_Regions) then
               return Fail ("destroying a bank didn't give back its usage");
            end if;

            if Write (Region, 0) then
               return Fail ("a region of a destroyed bank still takes pages");
            end if;
         end;

         --  A region limit.

         declare
            Sub    : Bank_Class := Top.Create_Sub_Bank (0, 2);
            First  : constant Rose.Capabilities.Capability := Reserve (Sub);
            Second : constant Rose.Capabilities.Capability := Reserve (Sub);
            Third  : constant Rose.Capabilities.Capability := Reserve (Sub);
         begin
            if First = Rose.Capabilities.Null_Capability
              or else Second = Rose.Capabilities.Null_Capability
            then
               return Fail ("a reservation within the region limit failed");
            end if;

            if Third /= Rose.Capabilities.Null_Capability then
               return Fail ("a reservation over the region limit succeeded");
            end if;

            if not Usage_Is (Sub, 0, 2) then
               return Fail ("the usage of the region-limited bank is wrong");
            end if;

            Sub.Destroy;
         end;

         --  A limit holds for the banks below it, and destroying a bank
         --  destroys the banks below it.

         declare
            Outer  : Bank_Class := Top.Create_Sub_Bank (1, 0);
            Inner  : Bank_Class := Outer.Create_Sub_Bank (0, 0);
            Region : constant Rose.Capabilities.Capability :=
                       Reserve (Inner, 2);
         begin
            if Region = Rose.Capabilities.Null_Capability
              or else not Write (Region, 0)
            then
               return Fail ("a write within a limit above failed");
            end if;

            if Write (Region, 1) then
               return Fail ("a write over a limit above succeeded");
            end if;

            Outer.Destroy;

            if not Usage_Is (Top, Top_Pages, Top_Regions) then
               return Fail ("destroying a bank left its sub-banks' usage");
            end if;
         end;
      end;

      return True;
   end Limit_Test;

   ----------------------
   -- On_Dependent_Bit --
   ----------------------

   procedure On_Dependent_Bit is
   begin
      Signal_Count := Signal_Count + 1;
   end On_Dependent_Bit;

   ----------------
   -- Reuse_Test --
   ----------------

   function Reuse_Test return Boolean is
      use type Rose.Words.Word;
      use type Rose.Words.Word_64;
      subtype Bank_Class is Rose.Interfaces.Storage.Bank.Bank_Object'Class;

      Page_Limit   : constant := 7;
      Region_Limit : constant := 3;

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
      Ada.Text_IO.Put ("Bank reuse test");

      if Tests.Registry.Bank = Rose.Capabilities.Null_Capability then
         Ada.Text_IO.Put (" (skipped: no bank)");
         return True;
      end if;

      declare
         Test_Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                       Rose.Interfaces.Storage.Bank.Client.From_Cap
                         (Tests.Registry.Bank);
         Old_Bank  : Bank_Class := Test_Bank.Create_Sub_Bank (0, 0);
         Old_Index : constant Rose.Words.Word := Old_Bank.Index;
      begin

         --  The destroyed bank's slot is the first free one, so the next
         --  bank takes it. The cap to the old bank is still in this
         --  process's table.

         Old_Bank.Destroy;

         declare
            New_Bank     : Bank_Class :=
                             Test_Bank.Create_Sub_Bank
                               (Page_Limit, Region_Limit);
            Pages        : Rose.Words.Word_64;
            Regions      : Rose.Words.Word;
            Pages_Limit  : Rose.Words.Word_64;
            Region_Count : Rose.Words.Word;
         begin
            Check ("the new bank has the destroyed bank's index",
                   New_Bank.Index /= Old_Index);

            Old_Bank.Get_Usage (Pages, Regions, Pages_Limit, Region_Count);
            Check ("the destroyed bank's cap has the new bank's limits",
                   Pages_Limit /= Page_Limit
                   or else Region_Count /= Region_Limit);

            New_Bank.Destroy;
         end;
      end;

      return Passed;
   exception
      when others =>
         Ada.Text_IO.Put (": an operation failed");
         return False;
   end Reuse_Test;

end Tests.Banks;
