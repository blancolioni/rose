with Rose.Invocation;
with Rose.System_Calls;

package body Console.Calls is

   Cursor_Enabled : constant Boolean := True;

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

end Console.Calls;
