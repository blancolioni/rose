with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Init_Capabilities;
with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Constructor.Client;
with Rose.Interfaces.Launch.Client;
with Rose.Interfaces.Named.Client;
with Rose.Interfaces.Process_Banks.Client;
with Rose.Interfaces.Releasable_Cap_Set.Client;
with Rose.Interfaces.Replacement.Client;
with Rose.Interfaces.Replacer.Client;
with Rose.Objects;

with Init.Boot_Steps;
with Init.Calls;
with Init.Grants;
with Init.Installer;

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

   procedure Kernel_Call
     (Kernel_Cap : Rose.Capabilities.Capability;
      First      : Rose.Capabilities.Capability;
      Second     : Rose.Capabilities.Capability;
      Success    : out Boolean;
      Result     : out Rose.Words.Word);
   --  Invoke one of init's handover caps (ADR 0032) with the processes
   --  that First and Second, unless it is null, name. Success is False if
   --  the kernel refused, and Result is the reply's data word, or 0.

   function Launch_Probe
     (Launch_Cap      : Rose.Capabilities.Capability;
      Bank            : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Number          : Natural;
      Target          : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;
   --  Launch swap_probe through Launch_Cap, against Bank, with Number and
   --  Target, and return its Named interface, or the null cap if it did
   --  not start.

   Probe_Page_Limit   : constant := 2048;
   Probe_Region_Limit : constant := 128;
   --  The swap test's bank, which holds three probes.

   Test_Page_Limit   : constant := 16;
   Test_Region_Limit : constant := 4;
   --  The test bank's limits: the shared segment test reserves one
   --  region of two pages (ADR 0025).

   function Replace
     (Replace_Process : Rose.Capabilities.Capability;
      Control         : Rose.Capabilities.Capability;
      Program         : Rose.Capabilities.Capability;
      Null_Caps       : Natural;
      Empty_Strings   : Natural;
      Results         : Natural)
      return Boolean;
   --  Replace the process whose Replaceable interface is Control, as an
   --  updater does (ADR 0032, section 1), through Replace_Process, mem's
   --  Replacer. The new version is the command whose Launch interface is
   --  Program, launched against the replacement's bank. Its launch call
   --  sends Null_Caps null caps, which it takes from the old version
   --  (#901), then Control as its Previous, and Empty_Strings empty
   --  strings, and expects Results caps back. True if it committed.

   function Test_Bank return Rose.Capabilities.Capability;
   --  A new sub-bank of the system bank, or the null cap if there is no
   --  system bank.

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
      Cap_Sets        : Rose.Capabilities.Capability)
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

      --  Deleting a parent, as ending its holder does, doesn't revoke
      --  what was derived from it: only rescinding does (ADR 0032, #864).

      First := Derive (Derive_Cap, Find_Cap);
      Child := Derive (Derive_Cap, First);
      Call_On (Delete_Cap, First);
      Check ("deleting a parent leaves its child working", Works (Child));
      Call_On (Delete_Cap, Child);

      --  A parent that goes through a cap set, as a handover's caps do,
      --  keeps its power to revoke: the copy taken from the set is the
      --  parent, with the same derivation record.

      if Cap_Sets /= Rose.Capabilities.Null_Capability then
         declare
            package Sets renames Rose.Interfaces.Releasable_Cap_Set.Client;
            Service : Rose.Interfaces.Constructor.Client.Constructor_Client;
            Set     : Sets.Releasable_Cap_Set_Client;
            Taken   : Rose.Capabilities.Capability;
         begin
            Rose.Interfaces.Constructor.Client.Open (Service, Cap_Sets);
            Sets.Open
              (Set, Rose.Interfaces.Constructor.Client.Create (Service));

            First := Derive (Derive_Cap, Find_Cap);
            Child := Derive (Derive_Cap, First);
            Sets.Append (Set, (1 => First));
            Call_On (Delete_Cap, First);

            Check ("a parent in a cap set leaves its child working",
                   not Sets.Has_Error and then Works (Child));

            Taken := Sets.Get_Cap (Set, 1);
            Check ("the parent taken from the set works",
                   Taken /= Rose.Capabilities.Null_Capability
                   and then Works (Taken));

            Call_On (Rescind_Cap, Taken);
            Check ("rescinding it revokes the child",
                   Works (Taken) and then not Works (Child));

            Call_On (Delete_Cap, Taken);
            Call_On (Delete_Cap, Child);
            Sets.Release (Set);
         end;
      end if;

      return Passed;
   end Derive_Test;

   -----------------------
   -- Exec_Forward_Test --
   -----------------------

   function Exec_Forward_Test
     (Update_Cap    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Old_Launch    : Rose.Capabilities.Capability;
      Install_Cap   : Rose.Capabilities.Capability)
      return Forward_Result
   is
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Client.Launch_Client;
      Bank     : Rose.Interfaces.Bank.Client.Bank_Client;
      Bank_Cap : Rose.Capabilities.Capability;
      Endpoint : Rose.Capabilities.Capability;
   begin
      if Update_Cap = Rose.Capabilities.Null_Capability then
         return No_Update_Cap;
      elsif Binary_Stream = Rose.Capabilities.Null_Capability
        or else Old_Launch = Rose.Capabilities.Null_Capability
      then
         return Nothing_From_Restore;
      end if;

      --  An install cap makes the new version count as changed, though
      --  its image is the same (ADR 0031, section 4).

      if Init.Installer.Update_Executable
        (Update_Cap, Binary_Stream, "replaceable_test", (1 => Install_Cap))
        = Rose.Capabilities.Null_Capability
      then
         return Update_Failed;
      end if;

      --  The test bank is too small for a command: replaceable_test needs
      --  a stack, a heap and its data.

      Bank_Cap := Init.Grants.Bank (Pages => 1024, Regions => 16);
      if Bank_Cap = Rose.Capabilities.Null_Capability then
         return No_Test_Bank;
      end if;
      Rose.Interfaces.Bank.Client.Open (Bank, Bank_Cap);

      Rose.Interfaces.Launch.Client.Open (Launcher, Old_Launch);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Bank_Cap, No_Caps);

      --  The command waits for its launch call, which never comes:
      --  destroying its bank ends it.

      Rose.Interfaces.Bank.Client.Destroy (Bank);
      return (if Endpoint = Rose.Capabilities.Null_Capability
              then Launch_Failed else Passed);
   end Exec_Forward_Test;

   --------------------
   -- Hand_Over_Test --
   --------------------

   function Hand_Over_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Process_Banks   : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      use type Rose.Words.Word_64;
      package Bank_Client renames Rose.Interfaces.Bank.Client;
      package Named renames Rose.Interfaces.Named.Client;
      package PB renames Rose.Interfaces.Process_Banks.Client;

      Old_Process : constant Rose.Objects.Object_Id := 16#7FFF_FFF0#;
      New_Process : constant Rose.Objects.Object_Id := 16#7FFF_FFF1#;
      --  Object ids that no process has, so mem ignores them.

      type Usage is
         record
            Pages   : Rose.Words.Word_64 := 0;
            Regions : Rose.Words.Word := 0;
         end record;

      Launch_Cap : constant Rose.Capabilities.Capability :=
                     Init.Calls.Find_In_Map (Find_Cap, "swap_probe");
      Bank_Cap   : constant Rose.Capabilities.Capability :=
                     Init.Grants.Bank (Probe_Page_Limit, Probe_Region_Limit);

      Passed     : Boolean := True;
      Banks      : PB.Process_Banks_Client;
      Test       : Bank_Client.Bank_Client;
      Old_Bank   : Bank_Client.Bank_Client;
      New_Bank   : Bank_Client.Bank_Client;
      Sub_Bank   : Bank_Client.Bank_Client;
      Probe      : Rose.Capabilities.Capability;
      Client     : Named.Named_Client;
      Old_Before : Usage;
      New_Before : Usage;
      Sub_Usage  : Usage;
      Test_Usage : Usage;

      function Answers return Boolean;
      --  The probe's Name is 4.

      procedure Check (Name : String; Ok : Boolean);
      --  Report the check Name, and fail the test if it did not hold.

      function Index_Of (Process : Rose.Objects.Object_Id)
                         return Rose.Words.Word;
      --  The index of Process's bank, or 0 if it has none.

      function Usage_Of (Bank : Bank_Client.Bank_Client) return Usage;
      --  What has been reserved from Bank and below it.

      -------------
      -- Answers --
      -------------

      function Answers return Boolean is
         Text : String (1 .. 16);
         Last : Natural;
      begin
         Named.Name (Client, Text, Last);
         return not Named.Has_Error
           and then Last in Text'Range
           and then Text (1 .. Last) = "4";
      end Answers;

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         Init.Calls.Send_String
           (Standard_Output,
            "hand over test: " & Name & (if Ok then ": ok" else ": FAIL")
            & NL);
         Passed := Passed and then Ok;
      end Check;

      --------------
      -- Index_Of --
      --------------

      function Index_Of (Process : Rose.Objects.Object_Id)
                         return Rose.Words.Word
      is
         Bank : constant Bank_Client.Bank_Client :=
                  PB.Process_Bank (Banks, Process);
      begin
         if Bank_Client.Get_Interface_Cap (Bank)
           = Rose.Capabilities.Null_Capability
         then
            return 0;
         end if;
         return Bank_Client.Index (Bank);
      end Index_Of;

      --------------
      -- Usage_Of --
      --------------

      function Usage_Of (Bank : Bank_Client.Bank_Client) return Usage is
         Result       : Usage;
         Page_Limit   : Rose.Words.Word_64;
         Region_Limit : Rose.Words.Word;
      begin
         Bank_Client.Get_Usage
           (Bank, Result.Pages, Result.Regions, Page_Limit, Region_Limit);
         return Result;
      end Usage_Of;

   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability
        or else Bank_Cap = Rose.Capabilities.Null_Capability
        or else Process_Banks = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      PB.Open (Banks, Process_Banks);
      Bank_Client.Open (Test, Bank_Cap);

      Old_Bank :=
        PB.Create_Process_Bank (Banks, Bank_Client.Index (Test), Old_Process);
      New_Bank :=
        PB.Create_Process_Bank (Banks, Bank_Client.Index (Test), New_Process);

      if Bank_Client.Get_Interface_Cap (Old_Bank)
        = Rose.Capabilities.Null_Capability
        or else Bank_Client.Get_Interface_Cap (New_Bank)
        = Rose.Capabilities.Null_Capability
      then
         Check ("make the two process banks", False);
         Bank_Client.Destroy (Test);
         return False;
      end if;

      Sub_Bank := Bank_Client.Create_Sub_Bank (Old_Bank, 0, 0);
      Probe := Launch_Probe (Launch_Cap,
                             Bank_Client.Get_Interface_Cap (Sub_Bank),
                             Create_Endpoint, 4,
                             Rose.Capabilities.Null_Capability);
      if Probe = Rose.Capabilities.Null_Capability then
         Check ("launch a probe below the old bank", False);
         Bank_Client.Destroy (Test);
         return False;
      end if;

      Named.Open (Client, Probe);
      Check ("the probe answers", Answers);

      Old_Before := Usage_Of (Old_Bank);
      New_Before := Usage_Of (New_Bank);
      Sub_Usage := Usage_Of (Sub_Bank);
      Test_Usage := Usage_Of (Test);
      Check ("the probe's storage is charged to the old bank",
             Sub_Usage.Regions > 0
             and then Old_Before.Regions >= Sub_Usage.Regions);

      PB.Hand_Over (Banks, Old_Process, New_Process);

      Check ("the owners are swapped",
             Index_Of (New_Process) = Bank_Client.Index (Old_Bank)
             and then Index_Of (Old_Process) = Bank_Client.Index (New_Bank));
      Check ("the sub-bank keeps its charges",
             Usage_Of (Sub_Bank) = Sub_Usage);
      Check ("its charges moved to the new bank",
             Usage_Of (Old_Bank)
             = (Old_Before.Pages - Sub_Usage.Pages,
                Old_Before.Regions - Sub_Usage.Regions)
             and then Usage_Of (New_Bank)
             = (New_Before.Pages + Sub_Usage.Pages,
                New_Before.Regions + Sub_Usage.Regions));
      Check ("the bank above them is unchanged",
             Usage_Of (Test) = Test_Usage);

      --  Mem ends the old version, which now has New's object id, and the
      --  store destroys its bank.

      PB.End_Process (Banks, New_Process);

      Check ("the old bank is gone", Index_Of (New_Process) = 0);
      Check ("the probe still answers", Answers);

      --  Destroying the test bank ends the probe.

      Bank_Client.Destroy (Test);
      return Passed;
   end Hand_Over_Test;

   -----------------
   -- Kernel_Call --
   -----------------

   procedure Kernel_Call
     (Kernel_Cap : Rose.Capabilities.Capability;
      First      : Rose.Capabilities.Capability;
      Second     : Rose.Capabilities.Capability;
      Success    : out Boolean;
      Result     : out Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Kernel_Cap);
      Rose.System_Calls.Send_Cap (Params, First);
      if Second /= Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Send_Cap (Params, Second);
      end if;
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      Success := not Params.Control.Flags (Rose.Invocation.Error);
      Result :=
        (if Success
           and then Params.Control.Flags (Rose.Invocation.Send_Words)
         then Params.Data (0)
         else 0);
   end Kernel_Call;

   ------------------
   -- Launch_Probe --
   ------------------

   function Launch_Probe
     (Launch_Cap      : Rose.Capabilities.Capability;
      Bank            : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Number          : Natural;
      Target          : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use type Rose.Words.Word;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint : Rose.Capabilities.Capability;
      Params   : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Bank, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return Rose.Capabilities.Null_Capability;
      end if;

      --  Its role's cap and its arguments, as src/utilities/swap_probe/
      --  rose-core-swap_probe.idl declares them: Create_Endpoint, then
      --  Target, and Number in the first data word. It replies with its
      --  Named interface, before it asks Target.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Target);
      Rose.System_Calls.Send_Word (Params, Number);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Params.Caps (0);
   end Launch_Probe;

   -------------
   -- Replace --
   -------------

   function Replace
     (Replace_Process : Rose.Capabilities.Capability;
      Control         : Rose.Capabilities.Capability;
      Program         : Rose.Capabilities.Capability;
      Null_Caps       : Natural;
      Empty_Strings   : Natural;
      Results         : Natural)
      return Boolean
   is
      use type Rose.Words.Word;
      package Replacements renames Rose.Interfaces.Replacement.Client;
      No_Caps     : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher    : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint    : Rose.Capabilities.Capability :=
        Rose.Capabilities.Null_Capability;
      Params      : aliased Rose.Invocation.Invocation_Record;
      Replacer    : Rose.Interfaces.Replacer.Client.Replacer_Client;
      Replacement : Replacements.Replacement_Client;
      Ready       : Boolean := False;
      Committed   : Boolean := False;
   begin
      if Replace_Process = Rose.Capabilities.Null_Capability
        or else Control = Rose.Capabilities.Null_Capability
        or else Program = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      --  Only the new version's calls reach the old version until
      --  Commit.

      Rose.Interfaces.Replacer.Client.Open (Replacer, Replace_Process);
      Replacement :=
        Rose.Interfaces.Replacer.Client.Replace_Process (Replacer, Control);
      if Replacements.Get_Interface_Cap (Replacement)
        = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      for Attempt in 1 .. 100_000 loop
         Ready := Replacements.Ready (Replacement);
         exit when Ready;
      end loop;

      if Ready then
         declare
            Sibling : constant Rose.Interfaces.Bank.Client.Bank_Client :=
              Replacements.Bank (Replacement);
         begin
            Rose.Interfaces.Launch.Client.Open (Launcher, Program);
            Endpoint :=
              Rose.Interfaces.Launch.Client.Launch_Command
                (Launcher,
                 Rose.Interfaces.Bank.Client.Get_Interface_Cap (Sibling),
                 No_Caps);
         end;
      end if;

      if Endpoint /= Rose.Capabilities.Null_Capability then
         Replacements.Allow (Replacement, Endpoint);

         --  The null caps are sent as the updater sends them (#830).

         Rose.System_Calls.Initialize_Send (Params, Endpoint);
         for I in 1 .. Null_Caps loop
            Rose.System_Calls.Send_Cap
              (Params, Rose.Capabilities.Null_Capability);
         end loop;
         Rose.System_Calls.Send_Cap (Params, Control);
         if Empty_Strings > 0 then
            Rose.System_Calls.Start_Items (Params);
            for I in 1 .. Empty_Strings loop
               Rose.System_Calls.Send_Text_Item (Params, "");
            end loop;
         end if;
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Receive_Caps (Params, Results);
         Rose.System_Calls.Invoke_Capability (Params);

         Committed :=
           not Params.Control.Flags (Rose.Invocation.Error)
           and then Params.Control.Flags (Rose.Invocation.Send_Words)
           and then Params.Data (0) = Completed
           and then Replacements.Commit (Replacement);
      end if;

      Replacements.Release (Replacement);
      return Committed;
   end Replace;

   -----------------------
   -- Replace_Held_File --
   -----------------------

   function Replace_Held_File
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Replace_Process : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Holder_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "hold_file");
      File_Cap   : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "file");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
      Control    : Rose.Capabilities.Capability;

      function Completes return Boolean
      is (not Params.Control.Flags (Rose.Invocation.Error)
          and then Params.Control.Flags (Rose.Invocation.Send_Words)
          and then Params.Data (0) = Completed
          and then Params.Control.Flags (Rose.Invocation.Send_Caps));
      --  The launch call in Params completed, with caps.

   begin
      if Holder_Cap = Rose.Capabilities.Null_Capability
        or else File_Cap = Rose.Capabilities.Null_Capability
        or else Replace_Process = Rose.Capabilities.Null_Capability
        or else Cap_Sets = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Holder_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  Its roles' caps, as src/utilities/hold_file/
      --  rose-core-hold_file.idl declares them: Create_Endpoint,
      --  Standard_Output, Delete_Cap, Find_Command and Cap_Set_Service. It
      --  replies with the file's Replaceable, once it holds the file.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Delete_Cap);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Cap_Sets);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if not Completes then
         return False;
      end if;
      Control := Params.Caps (0);

      --  The file command's launch call, as src/drivers/file/
      --  rose-core-file.idl declares it: Create_Endpoint, Delete_Cap and
      --  Cap_Set_Service, then Previous, and its results Served and
      --  Control.

      return Replace
        (Replace_Process => Replace_Process,
         Control         => Control,
         Program         => File_Cap,
         Null_Caps       => 3,
         Empty_Strings   => 0,
         Results         => 2);
   end Replace_Held_File;

   -----------------------
   -- Replace_Held_User --
   -----------------------

   function Replace_Held_User
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Replace_Process : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Holder_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "hold_home");
      User_Cap   : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "user");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
   begin
      if Holder_Cap = Rose.Capabilities.Null_Capability
        or else User_Cap = Rose.Capabilities.Null_Capability
        or else Cap_Sets = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Holder_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  Its roles' caps, as src/utilities/hold_home/
      --  rose-core-hold_home.idl declares them: Create_Endpoint,
      --  Standard_Output, Find_Command and Cap_Set_Service. It replies
      --  with the user server's Replaceable, once it holds the user.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Cap_Sets);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) /= Completed
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return False;
      end if;

      --  The user command's launch call, as src/utilities/user/
      --  rose-core-user.idl declares it: Create_Endpoint,
      --  Cap_Set_Service, Home and Bank, then Previous, and its name,
      --  which a new version takes from the old one; and its results
      --  User and Control.

      return Replace
        (Replace_Process => Replace_Process,
         Control         => Params.Caps (0),
         Program         => User_Cap,
         Null_Caps       => 4,
         Empty_Strings   => 1,
         Results         => 2);
   end Replace_Held_User;

   ---------
   -- Run --
   ---------

   function Run
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Clock           : Rose.Capabilities.Capability;
      Screen          : Rose.Capabilities.Capability;
      Create_Signal   : Rose.Capabilities.Capability;
      Cap_Sets        : Rose.Capabilities.Capability;
      Replace_Process : Rose.Capabilities.Capability)
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
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  The command replies when the tests have finished.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Commands);
      Rose.System_Calls.Send_Cap (Params, Clock);
      Rose.System_Calls.Send_Cap (Params, Test_Bank);
      Rose.System_Calls.Send_Cap (Params, Screen);
      Rose.System_Calls.Send_Cap (Params, Create_Signal);
      Rose.System_Calls.Send_Cap (Params, Cap_Sets);
      Rose.System_Calls.Send_Cap (Params, Replace_Process);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      return not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Params.Data (0) = Completed;
   end Run;

   --------------------------
   -- Start_Derived_Holder --
   --------------------------

   function Start_Derived_Holder
     (Find_Cap        : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Commands        : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability;
      Rescind_Cap     : Rose.Capabilities.Capability;
      Delete_Cap      : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      Launch_Cap : constant Rose.Capabilities.Capability :=
        Init.Calls.Find_In_Map (Find_Cap, "hold_derived");
      No_Caps    : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher   : Rose.Interfaces.Launch.Client.Launch_Client;
      Endpoint   : Rose.Capabilities.Capability;
      Params     : aliased Rose.Invocation.Invocation_Record;
      Spare      : Rose.Capabilities.Capability;
      Parent     : Rose.Capabilities.Capability;
      Child      : Rose.Capabilities.Capability;
   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  A record is the lowest that is free, so Child, which is derived
      --  from Parent, gets a lower number than it: a restore has to put
      --  Parent back first.

      Spare  := Derive (Derive_Cap, Commands);
      Parent := Derive (Derive_Cap, Commands);
      Call_On (Delete_Cap, Spare);
      Child  := Derive (Derive_Cap, Parent);

      if Parent = Rose.Capabilities.Null_Capability
        or else Child = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Launch.Client.Open (Launcher, Launch_Cap);
      Endpoint :=
        Rose.Interfaces.Launch.Client.Launch_Command
          (Launcher, Rose.Capabilities.Null_Capability, No_Caps);
      if Endpoint = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      --  Its roles' caps, as src/utilities/hold_derived/
      --  rose-core-hold_derived.idl declares them: Create_Endpoint,
      --  Standard_Output and Rescind_Cap, then its arguments, Parent and
      --  Child.  The command replies once it holds them.

      Rose.System_Calls.Initialize_Send (Params, Endpoint);
      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);
      Rose.System_Calls.Send_Cap (Params, Standard_Output);
      Rose.System_Calls.Send_Cap (Params, Rescind_Cap);
      Rose.System_Calls.Send_Cap (Params, Parent);
      Rose.System_Calls.Send_Cap (Params, Child);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      --  Hold_derived has its own copies, which name the same records.

      Call_On (Delete_Cap, Parent);
      Call_On (Delete_Cap, Child);

      return not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
        and then Params.Data (0) = Completed;
   end Start_Derived_Holder;

   ---------------
   -- Swap_Test --
   ---------------

   function Swap_Test
     (Find_Cap        : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Create_Endpoint : Rose.Capabilities.Capability;
      Derive_Cap      : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Words.Word;
      package Named renames Rose.Interfaces.Named.Client;
      package Init_Caps renames Rose.Init_Capabilities;

      Calling : constant Rose.Words.Word := 2;
      --  Process_State's word for a process in a call of its own.

      Launch_Cap : constant Rose.Capabilities.Capability :=
                     Init.Calls.Find_In_Map (Find_Cap, "swap_probe");
      Bank_Cap   : constant Rose.Capabilities.Capability :=
                     Init.Grants.Bank (Probe_Page_Limit, Probe_Region_Limit);
      Bank       : Rose.Interfaces.Bank.Client.Bank_Client;

      Passed     : Boolean := True;
      Probe_1    : Rose.Capabilities.Capability;
      Probe_2    : Rose.Capabilities.Capability;
      Probe_3    : Rose.Capabilities.Capability;
      Client_1   : Named.Named_Client;
      Client_2   : Named.Named_Client;
      Client_3   : Named.Named_Client;
      Derived    : Named.Named_Client;
      Ok         : Boolean;
      State      : Rose.Words.Word := 0;

      function Answers
        (Client   : Named.Named_Client;
         Expected : String)
         return Boolean;
      --  Client's Name is Expected.

      procedure Check (Name : String; Ok : Boolean);
      --  Report the check Name, and fail the test if it did not hold.

      function Refused (Client : Named.Named_Client) return Boolean;
      --  A call to Client's Name fails.

      -------------
      -- Answers --
      -------------

      function Answers
        (Client   : Named.Named_Client;
         Expected : String)
         return Boolean
      is
         Text : String (1 .. 16);
         Last : Natural;
      begin
         Named.Name (Client, Text, Last);
         return not Named.Has_Error
           and then Last in Text'Range
           and then Text (1 .. Last) = Expected;
      end Answers;

      -----------
      -- Check --
      -----------

      procedure Check (Name : String; Ok : Boolean) is
      begin
         Init.Calls.Send_String
           (Standard_Output,
            "swap test: " & Name & (if Ok then ": ok" else ": FAIL")
            & NL);
         Passed := Passed and then Ok;
      end Check;

      -------------
      -- Refused --
      -------------

      function Refused (Client : Named.Named_Client) return Boolean is
         Text : String (1 .. 16);
         Last : Natural;
      begin
         Named.Name (Client, Text, Last);
         return Named.Has_Error;
      end Refused;

   begin
      if Launch_Cap = Rose.Capabilities.Null_Capability
        or else Bank_Cap = Rose.Capabilities.Null_Capability
      then
         return False;
      end if;

      Rose.Interfaces.Bank.Client.Open (Bank, Bank_Cap);

      Probe_1 := Launch_Probe (Launch_Cap, Bank_Cap, Create_Endpoint, 1,
                               Rose.Capabilities.Null_Capability);
      Probe_2 := Launch_Probe (Launch_Cap, Bank_Cap, Create_Endpoint, 2,
                               Rose.Capabilities.Null_Capability);

      if Probe_1 = Rose.Capabilities.Null_Capability
        or else Probe_2 = Rose.Capabilities.Null_Capability
      then
         Check ("launch probes 1 and 2", False);
         Rose.Interfaces.Bank.Client.Destroy (Bank);
         return False;
      end if;

      Named.Open (Client_1, Probe_1);
      Named.Open (Client_2, Probe_2);
      Check ("probes 1 and 2 answer their numbers",
             Answers (Client_1, "1") and then Answers (Client_2, "2"));

      Named.Open_Cap_Set
        (Derived, Derive (Derive_Cap, Named.Get_Name_Cap (Client_1)));

      --  Hold every call to probe 1, and launch probe 3, which calls it
      --  once its launch call has returned. Wait until that call waits.

      Kernel_Call (Init_Caps.Hold_Calls_Cap, Probe_1,
                   Rose.Capabilities.Null_Capability, Ok, State);
      Check ("hold the calls to probe 1", Ok);

      Probe_3 := Launch_Probe (Launch_Cap, Bank_Cap, Create_Endpoint, 3,
                               Probe_1);
      Check ("launch probe 3",
             Probe_3 /= Rose.Capabilities.Null_Capability);

      if Probe_3 /= Rose.Capabilities.Null_Capability then
         for Attempt in 1 .. 100 loop
            Kernel_Call (Init_Caps.Process_State_Cap, Probe_3,
                         Rose.Capabilities.Null_Capability, Ok, State);
            exit when not Ok or else State = Calling;
            Init.Boot_Steps.Wait (10);
         end loop;
         Check ("probe 3's call to probe 1 waits",
                Ok and then State = Calling);
      end if;

      Kernel_Call (Init_Caps.Swap_Processes_Cap, Probe_1, Probe_2, Ok, State);
      Check ("swap probes 1 and 2", Ok);

      --  Probe_1 now names probe 2's process, which has the calls held.

      Kernel_Call (Init_Caps.Release_Calls_Cap, Probe_1,
                   Rose.Capabilities.Null_Capability, Ok, State);
      Check ("release the calls", Ok);

      Check ("a cap to probe 1 reaches probe 2", Answers (Client_1, "2"));
      Check ("a cap derived from one reaches probe 2",
             Answers (Derived, "2"));

      if Probe_3 /= Rose.Capabilities.Null_Capability then
         Named.Open (Client_3, Probe_3);
         Check ("probe 3's waiting call was answered by probe 2",
                Answers (Client_3, "2"));
      end if;

      Check ("a cap to probe 2 is refused", Refused (Client_2));

      --  Ending the bank ends the three probes, including probe 1's
      --  process, which is retired.

      Rose.Interfaces.Bank.Client.Destroy (Bank);
      return Passed;
   end Swap_Test;

   ---------------
   -- Test_Bank --
   ---------------

   function Test_Bank return Rose.Capabilities.Capability
   is (Init.Grants.Bank (Test_Page_Limit, Test_Region_Limit));

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
