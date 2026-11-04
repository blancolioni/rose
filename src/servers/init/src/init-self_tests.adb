with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Launch.Client;

with Init.Calls;
with Init.Grants;

package body Init.Self_Tests is

   use type Rose.Capabilities.Capability;

   Completed : constant Rose.Words.Word := 0;
   --  The first word of a command's reply when it completes
   --  (Rose.Command_Line.Launch).

   Max_Depth : constant := 8;
   --  How far down a capability can be derived.

   NL : constant Character := Character'Val (10);

   subtype Chain_Index is Natural range 0 .. Max_Depth + 1;
   type Chain_Array is
     array (Chain_Index) of Rose.Capabilities.Capability;

   procedure Call_On
     (Cap    : Rose.Capabilities.Capability;
      Target : Rose.Capabilities.Capability);
   --  Invoke Cap with Target as its one capability, and ignore the reply.

   function Derive
     (Derive_Cap : Rose.Capabilities.Capability;
      Cap        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  A derived copy of Cap, or the null capability if there is none.

   Test_Page_Limit   : constant := 16;
   Test_Region_Limit : constant := 4;
   --  The test bank's limits: the shared segment test reserves one
   --  region of two pages (ADR 0025).

   function Test_Bank
     (Storage : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  A new sub-bank of Storage's root bank, or the null cap if Storage
   --  is null.

   function Works
     (Cap : Rose.Capabilities.Capability)
      return Boolean;
   --  A call through Cap, a copy of the registry's capability, is
   --  answered.

   -------------
   -- Call_On --
   -------------

   procedure Call_On
     (Cap    : Rose.Capabilities.Capability;
      Target : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Cap (Params, Target);
      Rose.System_Calls.Invoke_Capability (Params);
   end Call_On;

   ------------
   -- Derive --
   ------------

   function Derive
     (Derive_Cap : Rose.Capabilities.Capability;
      Cap        : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Derive_Cap);
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Params.Caps (0);
   end Derive;

   -----------------
   -- Derive_Test --
   -----------------

   function Derive_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Create_Cap_Set  : Rose.Capabilities.Capability)
      return Boolean
   is
      Passed : Boolean := True;

      procedure Check (Name : String; Ok : Boolean);
      --  Report the check Name, and fail the test if it did not hold.

      procedure Derive_Chain (Chain : out Chain_Array; Length : out Natural);
      --  Derive from Find_Cap, then from that, as far as it goes.

      procedure Delete_Chain (Chain : Chain_Array; Length : Natural);
      --  Delete what Derive_Chain made.

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         Init.Calls.Send_String
           (Standard_Output,
            "derive test: " & Name & (if Ok then ": ok" else ": FAIL")
            & NL);
         Passed := Passed and then Ok;
      end Check;

      ------------------
      -- Delete_Chain --
      ------------------

      procedure Delete_Chain (Chain : Chain_Array; Length : Natural) is
      begin
         for I in 1 .. Length loop
            Call_On (Delete_Cap, Chain (I));
         end loop;
      end Delete_Chain;

      ------------------
      -- Derive_Chain --
      ------------------

      procedure Derive_Chain (Chain : out Chain_Array; Length : out Natural)
      is
      begin
         Chain := (0 => Find_Cap, others => Rose.Capabilities.Null_Capability);
         Length := 0;

         for I in 1 .. Chain_Index'Last loop
            Chain (I) := Derive (Derive_Cap, Chain (I - 1));
            exit when Chain (I) = Rose.Capabilities.Null_Capability;
            Length := I;
         end loop;
      end Derive_Chain;

      First   : Rose.Capabilities.Capability;
      Child   : Rose.Capabilities.Capability;
      Sibling : Rose.Capabilities.Capability;
      Chain   : Chain_Array;
      Length  : Natural;
      Kept    : Rose.Capabilities.Capability;
      Spare   : Rose.Capabilities.Capability;
      Below   : Rose.Capabilities.Capability;
      Held    : Rose.Capabilities.Capability;
   begin
      First   := Derive (Derive_Cap, Find_Cap);
      Child   := Derive (Derive_Cap, First);
      Sibling := Derive (Derive_Cap, Find_Cap);

      Check ("capabilities are derived",
             First /= Rose.Capabilities.Null_Capability
             and then Child /= Rose.Capabilities.Null_Capability
             and then Sibling /= Rose.Capabilities.Null_Capability);
      Check ("they work",
             Works (First) and then Works (Child) and then Works (Sibling));

      Call_On (Rescind_Cap, First);

      Check ("the rescinded capability goes on working", Works (First));
      Check ("what was derived from it does not", not Works (Child));
      Check ("its sibling and its parent go on working",
             Works (Sibling) and then Works (Find_Cap));

      Call_On (Delete_Cap, First);
      Call_On (Delete_Cap, Child);
      Call_On (Delete_Cap, Sibling);

      --  The records that those used are free again, so there is room to
      --  derive to the depth that is allowed, twice, and not further.

      for Round in 1 .. 2 loop
         Derive_Chain (Chain, Length);
         Check ("derived to a depth of eight, and no more",
                Length = Max_Depth);
         Check ("the deepest one works",
                Length = Max_Depth and then Works (Chain (Max_Depth)));
         Delete_Chain (Chain, Length);
      end loop;

      --  Two are kept, so that the checkpoint has derivation records to
      --  restore (the boot test looks for them).  A record is the lowest
      --  that is free, so Below, which is derived from Kept, has a lower
      --  number than it: a restore has to put Kept back first.  Init is
      --  not restored, so the caps server holds them, in a cap set, and
      --  init lets go of its own.

      Spare := Derive (Derive_Cap, Find_Cap);
      Kept  := Derive (Derive_Cap, Find_Cap);
      Call_On (Delete_Cap, Spare);
      Below := Derive (Derive_Cap, Kept);

      Check ("two are derived to be kept",
             Kept /= Rose.Capabilities.Null_Capability
             and then Below /= Rose.Capabilities.Null_Capability
             and then Works (Kept) and then Works (Below));

      Held := Init.Calls.Create_Cap_Set_With
        (Create_Cap_Set, (Kept, Below));

      Check ("the caps server holds them",
             Held /= Rose.Capabilities.Null_Capability);

      Call_On (Delete_Cap, Kept);
      Call_On (Delete_Cap, Below);

      return Passed;
   end Derive_Test;

   ---------
   -- Run --
   ---------

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Storage         : Rose.Capabilities.Capability;
      Screen          : Rose.Capabilities.Capability;
      Create_Signal   : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "run_self_tests");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command (Launcher, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  The command replies when the tests have finished.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Clock);
      Rose.System_Calls.Send_Cap (Params, Test_Bank (Storage));
      Rose.System_Calls.Send_Cap (Params, Screen);
      Rose.System_Calls.Send_Cap (Params, Create_Signal);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      return not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Params.Data (0) = Completed;
   end Run;

   ---------------
   -- Test_Bank --
   ---------------

   function Test_Bank
     (Storage : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is (Init.Grants.Bank (Storage, Test_Page_Limit, Test_Region_Limit));

   -----------
   -- Works --
   -----------

   function Works
     (Cap : Rose.Capabilities.Capability)
      return Boolean
   is
   begin
      return Init.Calls.Find_In_Map (Cap, "run_self_tests")
        /= Rose.Capabilities.Null_Capability;
   end Works;

end Init.Self_Tests;
