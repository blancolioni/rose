with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Dependent;

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
      subtype Bank_Class is Rose.Interfaces.Bank.Bank_Object'Class;
      subtype Dependent_Class is
        Rose.Interfaces.Dependent.Dependent_Object'Class;
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
         Test_Bank : Rose.Interfaces.Bank.Client.Bank_Client :=
                       Rose.Interfaces.Bank.Client.From_Cap
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

   ----------------------
   -- On_Dependent_Bit --
   ----------------------

   procedure On_Dependent_Bit is
   begin
      Signal_Count := Signal_Count + 1;
   end On_Dependent_Bit;

end Tests.Banks;
