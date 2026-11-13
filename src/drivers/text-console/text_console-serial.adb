with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

package body Text_Console.Serial is

   use System.Storage_Elements;

   Enabled     : Boolean := False;
   Data_Cap    : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   Status_Cap  : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   FIFO_Size : constant := 16;
   --  The bytes that COM2, a 16550, takes once it says that it is empty

   Header_Size : constant := 8;
   --  A tag starts with its type and its size, each four bytes. The
   --  command line's text follows, ending with a zero byte.

   Tag : Storage_Array (1 .. 512) := (others => 0);

   type Escape_State is (Normal, Escape, Control_Sequence);
   State : Escape_State := Normal;
   --  Where the copy is in an escape sequence: after ESC, or after
   --  ESC [, which ends with a byte from @ to ~.

   function Has_Argument
     (Command_Line : Rose.Capabilities.Capability;
      Name         : String)
      return Boolean;
   --  True if Name is a word of the command line

   procedure Write (Text : String);
   --  Write Text to COM2, a FIFO at a time

   ------------------
   -- Has_Argument --
   ------------------

   function Has_Argument
     (Command_Line : Rose.Capabilities.Capability;
      Name         : String)
      return Boolean
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Storage_Count;
      Start  : Storage_Offset;
   begin
      if Command_Line = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Tag := (others => 0);
      Rose.System_Calls.Initialize_Send (Params, Command_Line);
      Rose.System_Calls.Send_Buffer
        (Params, Tag'Length, Tag'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Words)
        or else Params.Data (0) <= Header_Size
      then
         return False;
      end if;

      Size :=
        Storage_Count'Min (Tag'Length, Storage_Count (Params.Data (0)));

      Start := Tag'First + Header_Size;
      while Start <= Size loop
         declare
            Finish : Storage_Offset := Start;
         begin
            while Finish <= Size
              and then Tag (Finish) /= 0
              and then Tag (Finish) /= Character'Pos (' ')
            loop
               Finish := Finish + 1;
            end loop;

            if Natural (Finish - Start) = Name'Length then
               declare
                  Same : Boolean := True;
               begin
                  for I in Name'Range loop
                     if Tag (Start + Storage_Offset (I - Name'First))
                       /= Character'Pos (Name (I))
                     then
                        Same := False;
                     end if;
                  end loop;
                  if Same then
                     return True;
                  end if;
               end;
            end if;

            exit when Finish > Size or else Tag (Finish) = 0;
            Start := Finish + 1;
         end;
      end loop;

      return False;
   end Has_Argument;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
      Plain : String (1 .. Text'Length);
      Last  : Natural := 0;
   begin
      if not Enabled then
         return;
      end if;

      for Ch of Text loop
         case State is
            when Normal =>
               if Character'Pos (Ch) = 27 then
                  State := Escape;
               else
                  Last := Last + 1;
                  Plain (Last) := Ch;
               end if;
            when Escape =>
               State := (if Ch = '[' then Control_Sequence
                         else Normal);
            when Control_Sequence =>
               if Character'Pos (Ch) in 16#40# .. 16#7E# then
                  State := Normal;
               end if;
         end case;
      end loop;

      if Last > 0 then
         Write (Plain (1 .. Last));
      end if;
   end Put;

   -----------
   -- Start --
   -----------

   procedure Start
     (Command_Line : Rose.Capabilities.Capability;
      Data         : Rose.Capabilities.Capability;
      Status       : Rose.Capabilities.Capability)
   is
   begin
      Data_Cap := Data;
      Status_Cap := Status;
      Enabled := Has_Argument (Command_Line, "serial-console");
   end Start;

   -----------
   -- Write --
   -----------

   procedure Write (Text : String) is
      use type Rose.Words.Word;

      procedure Wait_Until_Empty;
      --  Wait until the line status says that the transmitter is empty,
      --  or give up after a while, so that a port that never empties
      --  doesn't stop the console.

      ----------------------
      -- Wait_Until_Empty --
      ----------------------

      procedure Wait_Until_Empty is
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         for Attempt in 1 .. 10_000 loop
            Rose.System_Calls.Initialize_Send (Params, Status_Cap);
            Rose.System_Calls.Receive_Words (Params, 1);
            Rose.System_Calls.Invoke_Capability (Params);
            exit when Params.Control.Flags (Rose.Invocation.Error)
              or else (Params.Data (0) and 16#20#) /= 0;
         end loop;
      end Wait_Until_Empty;

      Start : Positive := Text'First;
   begin
      while Start <= Text'Last loop
         declare
            Last   : constant Natural :=
              Natural'Min (Start + FIFO_Size - 1, Text'Last);
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Wait_Until_Empty;
            Rose.System_Calls.Initialize_Send (Params, Data_Cap);
            for Ch of Text (Start .. Last) loop
               Rose.System_Calls.Send_Word
                 (Params, Rose.Words.Word (Character'Pos (Ch)));
            end loop;
            Rose.System_Calls.Invoke_Capability (Params);
            Start := Last + 1;
         end;
      end loop;
   end Write;

end Text_Console.Serial;
