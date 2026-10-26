with System.Storage_Elements;

with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Stream_Writer.Server;

with VGA_Console.Screen;

package body VGA_Console.Server is

   Server_Context : Rose.Server.Server_Context;

   procedure On_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array);

   procedure On_Restored
     (Id : Rose.Objects.Capability_Identifier);

   Console_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   -----------------
   -- On_Restored --
   -----------------

   procedure On_Restored
     (Id : Rose.Objects.Capability_Identifier)
   is
      pragma Unreferenced (Id);
   begin
      VGA_Console.Screen.Redraw;
   end On_Restored;

   --------------
   -- On_Write --
   --------------

   procedure On_Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      VGA_Console.Screen.Put (S);
   end On_Write;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer'Last);

      VGA_Console.Screen.Map;

      Rose.Interfaces.Stream_Writer.Server.Publish_Interface
        (Server_Context => Server_Context,
         Write          => On_Write'Access);
      Rose.Interfaces.Restorable.Server.Attach_Interface
        (Server_Context => Server_Context,
         Restored       => On_Restored'Access);

      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end VGA_Console.Server;
