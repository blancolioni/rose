with Rose.Console_IO;
with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Processes.Launch.Client;
with Rose.Interfaces.Processes.Replaceable;
with Rose.Interfaces.Processes.Replacement.Client;
with Rose.Interfaces.Processes.Replacer.Client;

with Restore.Checkpoints;

package body Restore.Replacing is

   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word;

   Name_Length  : constant := 16;
   Max_Programs : constant := 64;

   type Program_Record is
      record
         Name   : String (1 .. Name_Length) := (others => ' ');
         Last   : Natural := 0;
         Launch : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
      end record;

   Programs      : array (1 .. Max_Programs) of Program_Record;
   Program_Count : Natural := 0;

   Metadata_Text : String (1 .. 4096);
   --  A command's metadata, which says where Previous goes

   Completed : constant Rose.Words.Word := 0;
   --  The first word of the reply to a launch call whose command
   --  completed

   type Outcome is
     (Replaced, No_Metadata, No_Replacer, No_Replacement, Not_Ready,
      Launch_Failed, Not_Taken_Over);
   --  Replaced, or the step at which a replacement stopped

   Last_Error : Integer := 0;
   --  The position of the invocation error at which the last replacement
   --  stopped, if a call failed

   procedure Find_Previous
     (Text  : String;
      Index : out Natural;
      Found : out Boolean);
   --  The index of the Previous argument among a command's caps, from
   --  the "previous cap N" line of its metadata, Text

   function Launch_Of (Name : String) return Rose.Capabilities.Capability;
   --  The new Launch interface of the program Name, or null if it wasn't
   --  installed again

   function Replace
     (Control    : Rose.Capabilities.Capability;
      New_Launch : Rose.Capabilities.Capability)
      return Outcome;
   --  Replace the server whose Replaceable is Control with a process of
   --  the program whose Launch interface is New_Launch, as ADR 0032
   --  section 1 describes

   -----------------
   -- Add_Program --
   -----------------

   procedure Add_Program
     (Name   : String;
      Launch : Rose.Capabilities.Capability)
   is
   begin
      if Program_Count < Max_Programs
        and then Name'Length <= Name_Length
        and then Launch /= Rose.Capabilities.Null_Capability
      then
         Program_Count := Program_Count + 1;
         Programs (Program_Count).Name (1 .. Name'Length) := Name;
         Programs (Program_Count).Last := Name'Length;
         Programs (Program_Count).Launch := Launch;
      end if;
   end Add_Program;

   -------------------
   -- Find_Previous --
   -------------------

   procedure Find_Previous
     (Text  : String;
      Index : out Natural;
      Found : out Boolean)
   is
      Key : constant String := "previous cap ";
   begin
      Index := 0;
      Found := False;
      for Start in Text'First .. Text'Last - Key'Length + 1 loop
         if Text (Start .. Start + Key'Length - 1) = Key
           and then (Start = Text'First
                     or else Text (Start - 1) = Character'Val (10))
         then
            declare
               Position : Positive := Start + Key'Length;
            begin
               while Position <= Text'Last
                 and then Text (Position) in '0' .. '9'
               loop
                  Index :=
                    Index * 10
                    + Character'Pos (Text (Position)) - Character'Pos ('0');
                  Found := True;
                  Position := Position + 1;
               end loop;
            end;
            return;
         end if;
      end loop;
   end Find_Previous;

   ---------------
   -- Launch_Of --
   ---------------

   function Launch_Of (Name : String) return Rose.Capabilities.Capability is
   begin
      for Item of Programs (1 .. Program_Count) loop
         if Item.Name (1 .. Item.Last) = Name then
            return Item.Launch;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Launch_Of;

   -------------
   -- Replace --
   -------------

   function Replace
     (Control    : Rose.Capabilities.Capability;
      New_Launch : Rose.Capabilities.Capability)
      return Outcome
   is
      package Replacements renames Rose.Interfaces.Processes.Replacement
        .Client;
      No_Caps  : Rose.Capabilities.Capability_Array (1 .. 0);
      Launcher : Rose.Interfaces.Processes.Launch.Client.Launch_Client;
      Replacer : Rose.Interfaces.Processes.Replacer.Client.Replacer_Client;
      Last     : Natural;
      Index    : Natural;
      Found    : Boolean;
      Endpoint : Rose.Capabilities.Capability;
      Params   : aliased Rose.Invocation.Invocation_Record;

      function Completes return Boolean
      is (not Params.Control.Flags (Rose.Invocation.Error)
          and then Params.Control.Flags (Rose.Invocation.Send_Words)
          and then Params.Data (0) = Completed);
      --  The launch call in Params completed

      function Wait_Until_Ready
        (Replacement : Replacements.Replacement_Client)
         return Boolean;
      --  Ask Replacement until the old version is between calls, up to
      --  a limit

      ----------------------
      -- Wait_Until_Ready --
      ----------------------

      function Wait_Until_Ready
        (Replacement : Replacements.Replacement_Client)
         return Boolean
      is
      begin
         for Attempt in 1 .. 2_000 loop
            if Replacements.Ready (Replacement) then
               return True;
            end if;
         end loop;
         return False;
      end Wait_Until_Ready;

   begin
      Rose.Interfaces.Processes.Launch.Client.Open (Launcher, New_Launch);
      Rose.Interfaces.Processes.Launch.Client
        .Metadata (Launcher, Metadata_Text, Last);
      Find_Previous (Metadata_Text (1 .. Last), Index, Found);
      if not Found then
         return No_Metadata;
      end if;

      Rose.Interfaces.Processes.Replacer.Client.Open (Replacer, Replacer_Cap);
      if Rose.Interfaces.Processes.Replacer.Client
        .Get_Replace_Process_Cap (Replacer)
        = Rose.Capabilities.Null_Capability
      then
         return No_Replacer;
      end if;

      declare
         Replacement : constant Replacements.Replacement_Client :=
           Rose.Interfaces.Processes.Replacer.Client.Replace_Process
             (Replacer, Control);
         Committed   : Boolean;
      begin
         if Rose.Interfaces.Processes.Replacer.Client.Has_Error then
            Last_Error :=
              Rose.Invocation.Invocation_Error'Pos
                (Rose.Interfaces.Processes.Replacer.Client.Get_Last_Error);
            return No_Replacement;
         elsif Replacements.Get_Interface_Cap (Replacement)
           = Rose.Capabilities.Null_Capability
         then
            return No_Replacement;
         end if;

         --  The old version has to be between calls (section 4).

         if not Wait_Until_Ready (Replacement) then
            Replacements.Release (Replacement);
            return Not_Ready;
         end if;

         --  The new version is launched against the bank that mem gives,
         --  a sibling of the old version's (section 6).

         declare
            Sibling : constant Rose.Interfaces.Storage.Bank.Client
              .Bank_Client :=
              Replacements.Bank (Replacement);
         begin
            Endpoint :=
              Rose.Interfaces.Processes.Launch.Client.Launch_Command
                (Launcher,
                 Rose.Interfaces.Storage.Bank.Client
                   .Get_Interface_Cap (Sibling),
                 No_Caps);
         end;

         if Endpoint = Rose.Capabilities.Null_Capability then
            Replacements.Release (Replacement);
            return Launch_Failed;
         end if;

         Replacements.Allow (Replacement, Endpoint);

         --  The launch call: a null cap for each cap argument before
         --  Previous, which the new version takes from the old one (#901),
         --  and Previous, the old version's Replaceable.

         Rose.System_Calls.Initialize_Send (Params, Endpoint);
         for I in 1 .. Index loop
            Rose.System_Calls.Send_Cap
              (Params, Rose.Capabilities.Null_Capability);
         end loop;
         Rose.System_Calls.Send_Cap (Params, Control);
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Receive_Caps (Params, 4);
         Rose.System_Calls.Invoke_Capability (Params);

         --  The old version has just answered the new version's calls,
         --  and has to be between calls again (#926).

         if Completes and then not Wait_Until_Ready (Replacement) then
            Replacements.Release (Replacement);
            return Not_Ready;
         end if;

         Committed := Completes and then Replacements.Commit (Replacement);
         Replacements.Release (Replacement);
         return (if Committed then Replaced else Not_Taken_Over);
      end;
   end Replace;

   ---------------------
   -- Replace_Servers --
   ---------------------

   procedure Replace_Servers is
      Replaced_Count : Natural := 0;
      Failed_Count   : Natural := 0;
      Name           : String (1 .. Name_Length);
      Last           : Natural;
   begin
      if Replacer_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line
           ("restore: no Replacer, so the servers are unchanged");
         return;
      end if;

      Rose.Console_IO.Put_Line ("restore: replacing the servers");

      for Index in 1 .. Restore.Checkpoints.Restored_Process_Count loop
         declare
            Control : constant Rose.Capabilities.Capability :=
              Restore.Checkpoints.Find_Endpoint
                (Index, Rose.Interfaces.Processes.Replaceable
                  .Replaceable_Interface);
            New_Launch : Rose.Capabilities.Capability;
         begin
            if Control /= Rose.Capabilities.Null_Capability then
               Restore.Checkpoints.Get_Restored_Name (Index, Name, Last);
               New_Launch := Launch_Of (Name (1 .. Last));

               if New_Launch = Rose.Capabilities.Null_Capability then
                  Rose.Console_IO.Put_Line
                    ("restore: " & Name (1 .. Last)
                     & " was not installed again, so it is unchanged");
               else
                  declare
                     Result : constant Outcome :=
                       Replace (Control, New_Launch);
                  begin
                     if Result = Replaced then
                        Replaced_Count := Replaced_Count + 1;
                        Rose.Console_IO.Put_Line
                          ("restore: replaced " & Name (1 .. Last));
                     else
                        Failed_Count := Failed_Count + 1;
                        Rose.Console_IO.Put
                          ("restore: " & Name (1 .. Last)
                           & " is unchanged: ");
                        case Result is
                           when No_Metadata =>
                              Rose.Console_IO.Put_Line
                                ("its command has no Previous");
                           when No_Replacer =>
                              Rose.Console_IO.Put_Line
                                ("there is no Replacer interface");
                           when No_Replacement =>
                              Rose.Console_IO.Put
                                ("mem refused to replace it, error ");
                              Rose.Console_IO.Put (Last_Error);
                              Rose.Console_IO.New_Line;
                           when Not_Ready =>
                              Rose.Console_IO.Put_Line
                                ("it never got between calls");
                           when Launch_Failed =>
                              Rose.Console_IO.Put_Line
                                ("its new version did not launch");
                           when Not_Taken_Over =>
                              Rose.Console_IO.Put_Line
                                ("its new version did not take over");
                           when Replaced =>
                              null;
                        end case;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      Rose.Console_IO.Put ("restore: servers replaced: ");
      Rose.Console_IO.Put (Integer (Replaced_Count));
      Rose.Console_IO.Put ("; unchanged: ");
      Rose.Console_IO.Put (Integer (Failed_Count));
      Rose.Console_IO.New_Line;
   end Replace_Servers;

end Restore.Replacing;
