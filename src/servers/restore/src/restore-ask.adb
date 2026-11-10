with Rose.Console_IO;
with Rose.Invocation;
with Rose.Objects;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Devices.Port_IO;
with Rose.Interfaces.Timeout;

package body Restore.Ask is

   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word_8;

   Poll_Milliseconds : constant := 30;
   --  How long to wait when the keyboard has nothing, so that the
   --  console, which runs at a lower priority, can draw what is typed.

   Output_Full : constant Rose.Words.Word_8 := 16#01#;
   Aux_Data    : constant Rose.Words.Word_8 := 16#20#;
   --  Bits of the controller's status: a byte is waiting, and it is from
   --  the mouse, not the keyboard.

   Extended_Prefix : constant Rose.Words.Word_8 := 16#E0#;
   Break_Bit       : constant Rose.Words.Word_8 := 16#80#;

   Enter_Key     : constant Character := Character'Val (10);
   Backspace_Key : constant Character := Character'Val (8);
   No_Key        : constant Character := Character'Val (0);

   Receive_Timeout_Cap : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   Send_Timeout_Cap    : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
   --  The ends of the endpoint the timer sends to, made the first time
   --  Wait is called.

   procedure Wait (Milliseconds : Rose.Words.Word);
   --  Wait for the timer.

   function Next_Key return Character;
   --  Wait for a key, and return it as a character: a lower case letter,
   --  a digit, a space, Enter_Key or Backspace_Key.  Other keys, key
   --  releases and the mouse's bytes are skipped.

   function To_Character (Scan_Code : Rose.Words.Word_8) return Character;
   --  The character for the key that Scan_Code, in scan code set 1,
   --  presses, or No_Key.

   -------------
   -- Can_Ask --
   -------------

   function Can_Ask return Boolean is
   begin
      return Timer_Cap /= Rose.Capabilities.Null_Capability
        and then Keyboard_Status_Cap /= Rose.Capabilities.Null_Capability
        and then Keyboard_Data_Cap /= Rose.Capabilities.Null_Capability;
   end Can_Ask;

   -------------
   -- Confirm --
   -------------

   function Confirm
     (Question : String;
      Answer   : String)
      return Boolean
   is
      Line : String (1 .. 40) := (others => ' ');
      Last : Natural := 0;
      Key  : Character;
   begin
      if not Can_Ask then
         return False;
      end if;

      Rose.Console_IO.Put (Question);
      Rose.Console_IO.Flush;

      loop
         Key := Next_Key;
         exit when Key = Enter_Key;

         --  The boot console doesn't move back for a backspace, so the
         --  line is written again on a new line without the last key.

         if Key = Backspace_Key then
            if Last > 0 then
               Last := Last - 1;
               Rose.Console_IO.New_Line;
               Rose.Console_IO.Put (Line (1 .. Last));
            end if;
         elsif Last < Line'Last then
            Last := Last + 1;
            Line (Last) := Key;
            Rose.Console_IO.Put (Key);
         end if;
         Rose.Console_IO.Flush;
      end loop;

      Rose.Console_IO.New_Line;
      return Line (1 .. Last) = Answer;
   end Confirm;

   --------------
   -- Next_Key --
   --------------

   function Next_Key return Character is
      Extended : Boolean := False;
   begin
      loop
         declare
            Status : constant Rose.Words.Word_8 :=
                       Rose.Devices.Port_IO.Port_In_8 (Keyboard_Status_Cap);
         begin
            if (Status and Output_Full) = 0 then
               Wait (Poll_Milliseconds);
            else
               declare
                  Code : constant Rose.Words.Word_8 :=
                           Rose.Devices.Port_IO.Port_In_8
                             (Keyboard_Data_Cap);
               begin
                  if (Status and Aux_Data) /= 0 then
                     null;
                  elsif Code = Extended_Prefix then
                     Extended := True;
                  elsif (Code and Break_Bit) /= 0 then
                     Extended := False;
                  else

                     --  Of the extended keys, only the keypad's enter
                     --  is one this understands, and its code is the
                     --  main enter key's.

                     declare
                        Key : constant Character := To_Character (Code);
                     begin
                        if Key /= No_Key
                          and then (not Extended or else Key = Enter_Key)
                        then
                           return Key;
                        end if;
                     end;
                     Extended := False;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Next_Key;

   ------------------
   -- To_Character --
   ------------------

   function To_Character (Scan_Code : Rose.Words.Word_8) return Character is
      Digits_Row : constant String := "1234567890";
      Top_Row    : constant String := "qwertyuiop";
      Home_Row   : constant String := "asdfghjkl";
      Bottom_Row : constant String := "zxcvbnm";

      function From
        (Row   : String;
         First : Rose.Words.Word_8)
         return Character
      is (Row (Row'First + Natural (Scan_Code - First)));

   begin
      case Scan_Code is
         when 16#02# .. 16#0B# => return From (Digits_Row, 16#02#);
         when 16#0E#           => return Backspace_Key;
         when 16#10# .. 16#19# => return From (Top_Row, 16#10#);
         when 16#1C#           => return Enter_Key;
         when 16#1E# .. 16#26# => return From (Home_Row, 16#1E#);
         when 16#2C# .. 16#32# => return From (Bottom_Row, 16#2C#);
         when 16#39#           => return ' ';
         when others           => return No_Key;
      end case;
   end To_Character;

   ----------
   -- Wait --
   ----------

   procedure Wait (Milliseconds : Rose.Words.Word) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Receive_Timeout_Cap = Rose.Capabilities.Null_Capability then
         Rose.System_Calls.Server.Create_Endpoint
           (Create_Cap   => Create_Endpoint_Cap,
            Endpoint_Id  => Rose.Interfaces.Timeout.On_Timeout_Endpoint,
            Identifier   => Rose.Objects.Capability_Identifier'(0),
            Entry_Cap    => Receive_Timeout_Cap,
            Endpoint_Cap => Send_Timeout_Cap);
      end if;

      --  The timer's Set_Timer returns a cap for cancelling the timer,
      --  which isn't needed: it is deleted at once.

      Rose.System_Calls.Initialize_Send (Params, Timer_Cap);
      Rose.System_Calls.Send_Word (Params, Milliseconds);
      Rose.System_Calls.Send_Cap (Params, Send_Timeout_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Rose.System_Calls.Delete_Cap (Params.Caps (0));
      end if;

      Rose.System_Calls.Initialize_Receive (Params, Receive_Timeout_Cap);
      Rose.System_Calls.Receive_Words
        (Params, Natural (Rose.Invocation.Parameter_Word_Index'Last) + 1);
      Rose.System_Calls.Receive_Caps
        (Params, Natural (Rose.Invocation.Capability_Index'Last) + 1);
      Rose.System_Calls.Invoke_Capability (Params);
   end Wait;

end Restore.Ask;
