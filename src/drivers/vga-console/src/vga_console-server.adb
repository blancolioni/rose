with Ada.Streams;
with System.Storage_Elements;

with Interfaces;

with Rose.Capabilities;
with Rose.Keys;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Event_Listener.Server;
with Rose.Interfaces.Event_Source.Client;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Terminal.Server;

with Text_Console;

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
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Item   : in out Console;
      Buffer : Ada.Streams.Stream_Element_Array);

   overriding procedure Get_Size
     (Item    : in out Console;
      Columns : out Rose.Words.Word;
      Rows    : out Rose.Words.Word);

   overriding procedure Move_Cursor
     (Item   : in out Console;
      Column : Rose.Words.Word;
      Row    : Rose.Words.Word);

   overriding procedure Clear (Item : in out Console);

   overriding procedure Clear_To_End_Of_Line (Item : in out Console);

   overriding procedure Scroll
     (Item  : in out Console;
      Lines : Interfaces.Integer_32);

   overriding procedure Set_Attributes
     (Item          : in out Console;
      Bold          : Boolean;
      Reverse_Video : Boolean);

   overriding procedure Read_Key
     (Item      : in out Console;
      Key       : out Rose.Words.Word;
      Modifiers : out Rose.Words.Word);

   overriding procedure Disable_Line_Editing (Item : in out Console);

   overriding procedure Enable_Line_Editing (Item : in out Console);

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

   overriding function Reader_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Writer_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Event_Listener_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   procedure Unblock_Readers;
   --  A key has been typed: let a Read or a Read_Key that is held go
   --  ahead, if it can now.

   Server_Context : Rose.Server.Server_Context;
   The_Console    : aliased Console;

   Console_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Screen.Operate ((Kind => Text_Console.Clear));
   end Clear;

   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------

   overriding procedure Clear_To_End_Of_Line (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Screen.Operate
        ((Kind => Text_Console.Clear_To_End_Of_Line));
   end Clear_To_End_Of_Line;

   ------------------
   -- Disable_Echo --
   ------------------

   overriding procedure Disable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Echo (False);
   end Disable_Echo;

   --------------------------
   -- Disable_Line_Editing --
   --------------------------

   overriding procedure Disable_Line_Editing (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Editing (False);
   end Disable_Line_Editing;

   -----------------
   -- Enable_Echo --
   -----------------

   overriding procedure Enable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Echo (True);
   end Enable_Echo;

   -------------------------
   -- Enable_Line_Editing --
   -------------------------

   overriding procedure Enable_Line_Editing (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Line_Discipline.Set_Editing (True);
   end Enable_Line_Editing;

   --------------
   -- Get_Size --
   --------------

   overriding procedure Get_Size
     (Item    : in out Console;
      Columns : out Rose.Words.Word;
      Rows    : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Columns := Rose.Words.Word (VGA_Console.Screen.Columns);
      Rows := Rose.Words.Word (VGA_Console.Screen.Rows);
   end Get_Size;

   -----------------
   -- Move_Cursor --
   -----------------

   overriding procedure Move_Cursor
     (Item   : in out Console;
      Column : Rose.Words.Word;
      Row    : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Screen.Operate
        ((Text_Console.Move_Cursor,
          Natural (Rose.Words.Word'Min (Column, 65_535)),
          Natural (Rose.Words.Word'Min (Row, 65_535))));
   end Move_Cursor;

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
   begin
      VGA_Console.Line_Discipline.On_Key
        (Rose.Keys.Key_Of (Code), Rose.Keys.Modifiers_Of (Code));
      Unblock_Readers;
   end On_Event;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Console;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Item);
   begin
      if VGA_Console.Line_Discipline.Ready then
         VGA_Console.Line_Discipline.Take (Buffer, Last);
      else

         --  Hold the request until a line is complete: Unblock_Readers
         --  unblocks it, and this is called again.

         Buffer := (others => 0);
         Last := 0;
         Rose.Server.Block_Current_Request (Server_Context);
      end if;
   end Read;

   --------------
   -- Read_Key --
   --------------

   overriding procedure Read_Key
     (Item      : in out Console;
      Key       : out Rose.Words.Word;
      Modifiers : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Key := 0;
      Modifiers := 0;

      if VGA_Console.Line_Discipline.Editing then
         raise Program_Error with "line editing is on";
      elsif VGA_Console.Line_Discipline.Key_Ready then
         VGA_Console.Line_Discipline.Take_Key (Key, Modifiers);
      else

         --  Hold the request until a key is typed: Unblock_Readers
         --  unblocks it, and this is called again.

         Rose.Server.Block_Current_Request (Server_Context);
      end if;
   end Read_Key;

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

   ------------
   -- Scroll --
   ------------

   overriding procedure Scroll
     (Item  : in out Console;
      Lines : Interfaces.Integer_32)
   is
      pragma Unreferenced (Item);
      use type Interfaces.Integer_32;
   begin
      VGA_Console.Screen.Operate
        ((Text_Console.Scroll,
          Integer (Interfaces.Integer_32'Max
                     (-65_536, Interfaces.Integer_32'Min (Lines, 65_536)))));
   end Scroll;

   --------------------
   -- Set_Attributes --
   --------------------

   overriding procedure Set_Attributes
     (Item          : in out Console;
      Bold          : Boolean;
      Reverse_Video : Boolean)
   is
      pragma Unreferenced (Item);
   begin
      VGA_Console.Screen.Operate
        ((Text_Console.Set_Attributes, (Bold, Reverse_Video)));
   end Set_Attributes;

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
   -- Reader_Cap --
   -----------------------

   overriding function Reader_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of
         (Item, Rose.Interfaces.Terminal.Reader_Interface));

   -----------------------
   -- Writer_Cap --
   -----------------------

   overriding function Writer_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of
         (Item, Rose.Interfaces.Terminal.Writer_Interface));

   ------------------
   -- Terminal_Cap --
   ------------------

   overriding function Terminal_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Terminal.Server.Cap_Of (Item));

   ---------------------
   -- Unblock_Readers --
   ---------------------

   procedure Unblock_Readers is
   begin
      --  Each is unblocked only if it can go ahead now, since a request
      --  that blocks again would hold back the reply to the call that
      --  brought the key.

      if VGA_Console.Line_Discipline.Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Endpoint);
      end if;

      if VGA_Console.Line_Discipline.Key_Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Key_Endpoint);
      end if;
   end Unblock_Readers;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Console;
      Buffer : Ada.Streams.Stream_Element_Array)
   is
      pragma Unreferenced (Item);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      VGA_Console.Screen.Put (S);
   end Write;

end VGA_Console.Server;
