with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Event_Listener.Server;
with Rose.Interfaces.Event_Source.Client;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Terminal.Server;

with VGA_Console.Line_Discipline;
with VGA_Console.Screen;

package body VGA_Console.Server is

   --  The console is one object, the terminal (#454): it draws what it is
   --  written, it listens to the keyboard's events, and its line
   --  discipline gives what is typed to whoever reads it. It also gets the
   --  restore notification (ADR 0004).

   type Console is limited new
     Rose.Interfaces.Terminal.Server.Terminal_Interface
     and Rose.Interfaces.Event_Listener.Server.Event_Listener_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding procedure Read
     (Item   : in out Console;
      Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count);

   overriding procedure Write
     (Item   : in out Console;
      Buffer : System.Storage_Elements.Storage_Array);

   overriding procedure Enable_Echo (Item : in out Console);

   overriding procedure Disable_Echo (Item : in out Console);

   overriding procedure On_Event
     (Item : in out Console;
      Code : Rose.Words.Word);

   overriding procedure Restored (Item : in out Console);

   --  The console's caps for its interfaces, from the server packages it
   --  is attached to.

   overriding function Terminal_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Stream_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Stream_Reader_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Stream_Writer_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Event_Listener_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   Server_Context : Rose.Server.Server_Context;
   The_Console    : aliased Console;

   Console_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   ------------------
   -- Disable_Echo --
   ------------------

   overriding procedure Disable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Echo (False);
   end Disable_Echo;

   -----------------
   -- Enable_Echo --
   -----------------

   overriding procedure Enable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Echo (True);
   end Enable_Echo;

   ------------------------
   -- Event_Listener_Cap --
   ------------------------

   overriding function Event_Listener_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Event_Listener.Server.Cap_Of (Item));

   --------------
   -- On_Event --
   --------------

   overriding procedure On_Event
     (Item : in out Console;
      Code : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
      Ready : Boolean;
   begin
      VGA_Console.Line_Discipline.On_Key (Code, Ready);
      if Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Endpoint);
      end if;
   end On_Event;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Console;
      Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Item);
   begin
      if VGA_Console.Line_Discipline.Ready then
         VGA_Console.Line_Discipline.Take (Buffer, Last);
      else

         --  Hold the request until a line is complete: On_Event unblocks
         --  it, and this is called again.

         Buffer := (others => 0);
         Last := 0;
         Rose.Server.Block_Current_Request (Server_Context);
      end if;
   end Read;

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

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
      Keyboard : Rose.Interfaces.Event_Source.Client.Event_Source_Client;
   begin
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer'Last);

      VGA_Console.Screen.Map;

      Rose.Interfaces.Terminal.Server.Publish
        (Server_Context, The_Console'Access);
      Rose.Interfaces.Event_Listener.Server.Attach
        (Server_Context, The_Console'Access);
      Rose.Interfaces.Restorable.Server.Attach
        (Server_Context, The_Console'Access);

      --  The console reads the keyboard itself (#454).

      Rose.Interfaces.Event_Source.Client.Open (Keyboard, Event_Source_Cap);
      Rose.Interfaces.Event_Source.Client.Add_Listener
        (Keyboard, The_Console);

      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

   ----------------
   -- Stream_Cap --
   ----------------

   overriding function Stream_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of
         (Item, Rose.Interfaces.Terminal.Stream_Interface));

   -----------------------
   -- Stream_Reader_Cap --
   -----------------------

   overriding function Stream_Reader_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of
         (Item, Rose.Interfaces.Terminal.Stream_Reader_Interface));

   -----------------------
   -- Stream_Writer_Cap --
   -----------------------

   overriding function Stream_Writer_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of
         (Item, Rose.Interfaces.Terminal.Stream_Writer_Interface));

   ------------------
   -- Terminal_Cap --
   ------------------

   overriding function Terminal_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of (Item));

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
