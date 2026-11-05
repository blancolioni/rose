with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Terminal.Server;

with Surface_Console.Line_Discipline;
with Surface_Console.Screen;

package body Surface_Console.Server is

   use System.Storage_Elements;

   --  The console is one object, the terminal (#454), as vga-console is.
   --  Its keys come from the surface's events: the surface signals it
   --  when there are some (ADR 0022, as amended by #653), and it reads
   --  them until there are none.

   Events_Signal : constant := 8;
   --  The console's own signal bit for the surface's events.

   Key_Event   : constant := 1;
   Event_Bytes : constant := 16;

   type Console is limited new
     Rose.Interfaces.Terminal.Server.Terminal_Interface
     with null record;

   overriding procedure Read
     (Item   : in out Console;
      Buffer : out Storage_Array;
      Last   : out Storage_Offset);

   overriding procedure Write
     (Item   : in out Console;
      Buffer : Storage_Array);

   overriding procedure Enable_Echo (Item : in out Console);

   overriding procedure Disable_Echo (Item : in out Console);

   overriding function Terminal_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Stream_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Reader_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   overriding function Stream_Writer_Cap
     (Item : in out Console)
      return Rose.Capabilities.Capability;

   procedure On_Events;
   --  The surface has events: read them all, and give each key to the
   --  line discipline.

   Server_Context : Rose.Server.Server_Context;
   The_Console    : aliased Console;

   Console_Buffer : Storage_Array (1 .. 4096)
     with Alignment => 4096;

   ------------------
   -- Disable_Echo --
   ------------------

   overriding procedure Disable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Echo (False);
   end Disable_Echo;

   -----------------
   -- Enable_Echo --
   -----------------

   overriding procedure Enable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Echo (True);
   end Enable_Echo;

   ---------------
   -- On_Events --
   ---------------

   procedure On_Events is
      use Rose.Words;
      Buffer : Storage_Array (1 .. 16 * Event_Bytes);
      Last   : Storage_Count;
      Ready  : Boolean := False;

      function Get_Word (Offset : Storage_Offset) return Word;
      --  The word at Offset in Buffer, in the machine's byte order

      --------------
      -- Get_Word --
      --------------

      function Get_Word (Offset : Storage_Offset) return Word is
         Result : Word := 0;
      begin
         for I in reverse Offset .. Offset + 3 loop
            Result := Result * 256 + Word (Buffer (I));
         end loop;
         return Result;
      end Get_Word;

   begin
      loop
         Surface_Console.Screen.Read_Events (Buffer, Last);
         exit when Last < Buffer'First;

         declare
            Start : Storage_Offset := Buffer'First;
         begin
            while Start + Event_Bytes - 1 <= Last loop
               if Get_Word (Start) = Key_Event then
                  declare
                     Line_Ready : Boolean;
                  begin
                     Surface_Console.Line_Discipline.On_Key
                       (Get_Word (Start + 4), Line_Ready);
                     Ready := Ready or else Line_Ready;
                  end;
               end if;
               Start := Start + Event_Bytes;
            end loop;
         end;
      end loop;

      if Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Endpoint);
      end if;
   end On_Events;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Console;
      Buffer : out Storage_Array;
      Last   : out Storage_Offset)
   is
      pragma Unreferenced (Item);
   begin
      if Surface_Console.Line_Discipline.Ready then
         Surface_Console.Line_Discipline.Take (Buffer, Last);
      else

         --  Hold the request until a line is complete: On_Events unblocks
         --  it, and this is called again.

         Buffer := (others => 0);
         Last := 0;
         Rose.Server.Block_Current_Request (Server_Context);
      end if;
   end Read;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Signal : Rose.Capabilities.Capability;
   begin
      Rose.System_Calls.Use_Buffer
        (Console_Buffer'Address, Console_Buffer'Last);

      Surface_Console.Screen.Open;

      Rose.Interfaces.Terminal.Server.Publish
        (Server_Context, The_Console'Access);

      Rose.Server.Set_Signal_Handler
        (Server_Context, Events_Signal, On_Events'Access);
      Signal := Rose.System_Calls.Create_Signal_Cap (Events_Signal);
      Surface_Console.Screen.Notify (Signal);

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
      Buffer : Storage_Array)
   is
      pragma Unreferenced (Item);
      S : String (1 .. Natural (Buffer'Length));
      for S'Address use Buffer'Address;
      pragma Import (Ada, S);
   begin
      Surface_Console.Screen.Put (S);
   end Write;

end Surface_Console.Server;
