with Rose.System_Calls.Server;

with Console.Calls;
with Console.IO;

package body Console.Server is

   Entry_Cap    : Rose.Capabilities.Capability;
   Endpoint_Cap : Rose.Capabilities.Capability;

   ---------------------
   -- Create_Console --
   ---------------------

   procedure Create_Console_Server is
   begin
      Rose.System_Calls.Server.Create_Endpoint
        (Create_Cap   => Create_Endpoint_Cap,
         Endpoint_Id  => Endpoint_Id,
         Entry_Cap    => Entry_Cap,
         Endpoint_Cap => Endpoint_Cap);
   end Create_Console_Server;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      loop
         Console.Calls.Invoke_Receive_Cap
           (Receive_Cap => Entry_Cap,
            Process     =>
              Console.IO.Put'Access);
      end loop;
   end Start_Server;

end Console.Server;
