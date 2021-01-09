with System.Storage_Elements;

with Rose.Objects;

with Rose.Server;

with Rose.Interfaces.Stream_Writer.Server;

with Console.IO;

package body Console.Server is

   Server_Context : Rose.Server.Server_Context;

   procedure Handle_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   ---------------------------
   -- Create_Console_Server --
   ---------------------------

   procedure Create_Console_Server is
   begin
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
