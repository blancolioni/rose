with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Stream_Writer.Server;

with VGA_Console.Screen;

package body VGA_Console.Server is

   --  The console is one object, served as two interfaces: a stream that
   --  writes to the screen, and the restore notification (ADR 0004).

   type Console is limited new
     Rose.Interfaces.Stream_Writer.Server.Stream_Writer_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding procedure Write
     (Item   : in out Console;
      Buffer : System.Storage_Elements.Storage_Array);

   overriding procedure Restored (Item : in out Console);

   Server_Context : Rose.Server.Server_Context;
   Screen         : aliased Console;

   Console_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Screen.Redraw;
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer'Last);

      VGA_Console.Screen.Map;

      Rose.Interfaces.Stream_Writer.Server.Publish
        (Server_Context, Screen'Access);
      Rose.Interfaces.Restorable.Server.Attach
        (Server_Context, Screen'Access);

      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Console;
      Buffer : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Item);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      VGA_Console.Screen.Put (S);
   end Write;

end VGA_Console.Server;
