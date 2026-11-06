with System.Storage_Elements;

with Interfaces;

with Rose.Capabilities;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Terminal.Server;

with Text_Console;

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

   procedure On_Events;
   --  The surface has events: read them all, and give each key to the
   --  line discipline.

   procedure Unblock_Readers;
   --  A key has been typed: let a Read or a Read_Key that is held go
   --  ahead, if it can now.

   Server_Context : Rose.Server.Server_Context;
   The_Console    : aliased Console;

   Console_Buffer : Storage_Array (1 .. 4096)
     with Alignment => 4096;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Screen.Operate ((Kind => Text_Console.Clear));
   end Clear;

   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------

   overriding procedure Clear_To_End_Of_Line (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Screen.Operate
        ((Kind => Text_Console.Clear_To_End_Of_Line));
   end Clear_To_End_Of_Line;

   ------------------
   -- Disable_Echo --
   ------------------

   overriding procedure Disable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Echo (False);
   end Disable_Echo;

   --------------------------
   -- Disable_Line_Editing --
   --------------------------

   overriding procedure Disable_Line_Editing (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Editing (False);
   end Disable_Line_Editing;

   -----------------
   -- Enable_Echo --
   -----------------

   overriding procedure Enable_Echo (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Echo (True);
   end Enable_Echo;

   -------------------------
   -- Enable_Line_Editing --
   -------------------------

   overriding procedure Enable_Line_Editing (Item : in out Console) is
      pragma Unreferenced (Item);
   begin
      Surface_Console.Line_Discipline.Set_Editing (True);
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
      Columns := Rose.Words.Word (Surface_Console.Screen.Columns);
      Rows := Rose.Words.Word (Surface_Console.Screen.Rows);
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
      Surface_Console.Screen.Operate
        ((Text_Console.Move_Cursor,
          Natural (Rose.Words.Word'Min (Column, 65_535)),
          Natural (Rose.Words.Word'Min (Row, 65_535))));
   end Move_Cursor;

   ---------------
   -- On_Events --
   ---------------

   procedure On_Events is
      use Rose.Words;
      Buffer : Storage_Array (1 .. 16 * Event_Bytes);
      Last   : Storage_Count;

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
                  Surface_Console.Line_Discipline.On_Key
                    (Get_Word (Start + 4) mod 2 ** 24,
                     Get_Word (Start + 8) mod 8);
               end if;
               Start := Start + Event_Bytes;
            end loop;
         end;
      end loop;

      Unblock_Readers;
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

      if Surface_Console.Line_Discipline.Editing then
         raise Program_Error with "line editing is on";
      elsif Surface_Console.Line_Discipline.Key_Ready then
         Surface_Console.Line_Discipline.Take_Key (Key, Modifiers);
      else

         --  Hold the request until a key is typed: Unblock_Readers
         --  unblocks it, and this is called again.

         Rose.Server.Block_Current_Request (Server_Context);
      end if;
   end Read_Key;

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
      Surface_Console.Screen.Operate
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
      Surface_Console.Screen.Operate
        ((Text_Console.Set_Attributes, (Bold, Reverse_Video)));
   end Set_Attributes;

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

      if Surface_Console.Line_Discipline.Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Endpoint);
      end if;

      if Surface_Console.Line_Discipline.Key_Ready then
         Rose.Server.Unblock_Endpoint
           (Server_Context, Rose.Interfaces.Terminal.Read_Key_Endpoint);
      end if;
   end Unblock_Readers;

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
