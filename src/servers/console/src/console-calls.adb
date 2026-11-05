with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;

package body Console.Calls is

   Cursor_Enabled : constant Boolean := True;

   Serial_FIFO_Size : constant := 16;
   --  The bytes that COM1, a 16550, takes once it says that it is empty

   ---------------------
   -- Has_Framebuffer --
   ---------------------

   function Has_Framebuffer return Boolean is
      use type Rose.Words.Word;
      use type System.Storage_Elements.Storage_Element;
      Tag    : System.Storage_Elements.Storage_Array (0 .. 31);
      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Rose.Words.Word := 0;
   begin
      --  The kernel writes the tag straight into Tag, and can't take a
      --  page fault there (#713), so Tag's page is mapped first by
      --  clearing it.

      Tag := (others => 0);

      Rose.System_Calls.Initialize_Send (Params, Framebuffer_Tag_Cap);
      Rose.System_Calls.Send_Buffer
        (Params, Tag'Length, Tag'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
      then
         Size := Params.Data (0);
      end if;

      --  The type is at byte 29: 0 for indexed colour, 1 for RGB, and 2
      --  for EGA text.

      return Size >= 30 and then Tag (29) /= 2;
   end Has_Framebuffer;

   -----------------------
   -- Invoke_Memory_Cap --
   -----------------------

   procedure Invoke_Memory_Cap is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params :=
        Invocation_Record'
          (Control       =>
             Control_Word'
               (Flags          =>
                  (Send       => True,
                   Block      => True,
                   others     => False),
                others         => <>),
           Cap           => Console_Memory_Cap,
           others        => <>);

      Rose.System_Calls.Invoke_Capability (Params);
   end Invoke_Memory_Cap;

   --------------------------
   -- Send_Cursor_Position --
   --------------------------

   procedure Send_Cursor_Position (Position : Rose.Words.Word_16) is
      use Rose.Invocation, Rose.Words;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cursor_Enabled then
         Params :=
           Invocation_Record'
             (Control       =>
                Control_Word'
                  (Flags          =>
                     (Send       => True,
                      Block      => True,
                      Send_Words => True,
                      others     => False),
                   Last_Sent_Word => 3,
                   others         => <>),
              Cap           => Console_Cursor_Cap,
              Data          =>
                (0      => 16#0E00#,
                 1      => Word (Position / 256 mod 256) * 256 + 1,
                 2      => 16#0F00#,
                 3      => Word (Position mod 256) * 256 + 1,
                 others => <>),
              others        => <>);

         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Send_Cursor_Position;

   ------------------
   -- Write_Serial --
   ------------------

   procedure Write_Serial (Text : String) is
      use Rose.Words;

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
            Rose.System_Calls.Initialize_Send (Params, Serial_Status_Cap);
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
              Natural'Min (Start + Serial_FIFO_Size - 1, Text'Last);
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Wait_Until_Empty;
            Rose.System_Calls.Initialize_Send (Params, Serial_Data_Cap);
            for Ch of Text (Start .. Last) loop
               Rose.System_Calls.Send_Word
                 (Params, Natural'(Character'Pos (Ch)));
            end loop;
            Rose.System_Calls.Invoke_Capability (Params);
            Start := Last + 1;
         end;
      end loop;
   end Write_Serial;

end Console.Calls;
