with System.Storage_Elements;

with Rose.Objects;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Stream_Writer.Server;

with Console.IO;

package body Console.Server is

   Server_Context : Rose.Server.Server_Context;

   procedure Handle_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   Console_Buffer_Size : constant := 4096;

   Console_Buffer      : System.Storage_Elements.Storage_Array
     (1 .. Console_Buffer_Size)
     with Alignment => 4096;

   ---------------------------
   -- Create_Console_Server --
   ---------------------------

   procedure Create_Console_Server is
   begin
      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => 1);
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer_Size);

      Rose.Interfaces.Stream_Writer.Server.Create_Server
        (Server_Context => Server_Context,
         Write          => Handle_Write'Access);
   end Create_Console_Server;

   ------------------
   -- Handle_Write --
   ------------------

   procedure Handle_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      Console.IO.Put (S);
   end Handle_Write;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Console.Server;
