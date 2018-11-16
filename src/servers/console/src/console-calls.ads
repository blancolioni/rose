with Rose.Capabilities;
with Rose.Words;

package Console.Calls is

   procedure Invoke_Memory_Cap;

   procedure Send_Cursor_Position (Position : Rose.Words.Word_16);

   procedure Invoke_Receive_Cap
     (Receive_Cap : Rose.Capabilities.Capability;
      Process     : not null access
        procedure (Text : String));

end Console.Calls;
