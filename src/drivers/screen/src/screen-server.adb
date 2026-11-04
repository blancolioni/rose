with System.Storage_Elements;

with Interfaces;

with Rose.Capabilities;
with Rose.Server;
with Rose.Shared_Regions;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Bank.Client;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Restorable.Server;
with Rose.Interfaces.Stream_Reader.Client;
with Rose.Interfaces.Stream_Reader.Server;
with Rose.Interfaces.Surface.Server;

with Screen.Framebuffer;

package body Screen.Server is

   use Rose.Words;
   use type System.Address;

   --  The whole screen as one surface (ADR 0022, section 1). Its holder
   --  draws into the buffer, which both of them map shared, and Damage
   --  copies what it changed to the framebuffer. After a restore, the
   --  framebuffer is mapped again and drawn from the buffer.

   type Screen_Surface is limited new
     Rose.Interfaces.Surface.Server.Surface_Interface
     and Rose.Interfaces.Restorable.Server.Restorable_Interface
     with null record;

   overriding procedure Get_Size
     (Item   : in out Screen_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word);

   overriding function Get_Buffer
     (Item : in out Screen_Surface)
      return Rose.Interfaces.Region.Region_Object'Class;

   overriding procedure Damage
     (Item   : in out Screen_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word);

   overriding function Events
     (Item : in out Screen_Surface)
      return Rose.Interfaces.Stream_Reader.Stream_Reader_Object'Class;

   overriding procedure Get_Pointer
     (Item    : in out Screen_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word);

   overriding procedure Set_Cursor
     (Item  : in out Screen_Surface;
      Shape : Rose.Interfaces.Surface.Cursor_Shape);

   overriding procedure Release (Item : in out Screen_Surface);

   overriding procedure Restored (Item : in out Screen_Surface);

   overriding function Surface_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability;

   overriding function Resource_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability;

   overriding function Restorable_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability;

   --  The surface's events. There are none yet: the keyboard comes to the
   --  screen with the console (#653), and the pointer with #588, so a
   --  Read waits.

   type Event_Stream is limited new
     Rose.Interfaces.Stream_Reader.Server.Stream_Reader_Interface
     with null record;

   overriding procedure Read
     (Item   : in out Event_Stream;
      Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count);

   overriding function Stream_Reader_Cap
     (Item : in out Event_Stream)
      return Rose.Capabilities.Capability;

   Server_Context : Rose.Server.Server_Context;
   The_Surface    : aliased Screen_Surface;
   The_Events     : aliased Event_Stream;

   Buffer_Address : System.Address := System.Null_Address;
   Buffer_Cap     : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;
   --  The surface's buffer, where this driver maps it, and its Region.
   --  Without a framebuffer, the surface is 0 by 0 and has no buffer.

   Invocation_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   procedure Redraw;
   --  Copy the whole buffer to the framebuffer.

   ------------
   -- Damage --
   ------------

   overriding procedure Damage
     (Item   : in out Screen_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
      Screen_Width  : constant Word := Screen.Framebuffer.Width;
      Screen_Height : constant Word := Screen.Framebuffer.Height;
   begin
      --  Cut the rectangle to the screen, without overflowing.

      if X < Screen_Width and then Y < Screen_Height then
         Screen.Framebuffer.Copy
           (Buffer  => Buffer_Address,
            X       => X,
            Y       => Y,
            Columns => Word'Min (Width, Screen_Width - X),
            Rows    => Word'Min (Height, Screen_Height - Y));
      end if;
   end Damage;

   ------------
   -- Events --
   ------------

   overriding function Events
     (Item : in out Screen_Surface)
      return Rose.Interfaces.Stream_Reader.Stream_Reader_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Stream_Reader.Client.From_Cap
        (Rose.Interfaces.Stream_Reader.Server.Cap_Of (The_Events));
   end Events;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Item : in out Screen_Surface)
      return Rose.Interfaces.Region.Region_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Region.Client.From_Cap (Buffer_Cap);
   end Get_Buffer;

   -----------------
   -- Get_Pointer --
   -----------------

   overriding procedure Get_Pointer
     (Item    : in out Screen_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      --  There is no pointer until #588.

      Over := False;
      X := 0;
      Y := 0;
      Buttons := 0;
   end Get_Pointer;

   --------------
   -- Get_Size --
   --------------

   overriding procedure Get_Size
     (Item   : in out Screen_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Width := Screen.Framebuffer.Width;
      Height := Screen.Framebuffer.Height;
   end Get_Size;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Event_Stream;
      Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Item);
   begin
      Buffer := (others => 0);
      Last := 0;
      Rose.Server.Block_Current_Request (Server_Context);
   end Read;

   ------------
   -- Redraw --
   ------------

   procedure Redraw is
   begin
      Screen.Framebuffer.Copy
        (Buffer  => Buffer_Address,
         X       => 0,
         Y       => 0,
         Columns => Screen.Framebuffer.Width,
         Rows    => Screen.Framebuffer.Height);
   end Redraw;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Screen_Surface) is
      pragma Unreferenced (Item);
   begin
      --  The screen surface is never destroyed: it is handed from one
      --  holder to the next (ADR 0022, section 1).

      null;
   end Release;

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Surface.Server.Cap_Of
         (Item, Rose.Interfaces.Surface.Resource_Interface));

   --------------------
   -- Restorable_Cap --
   --------------------

   overriding function Restorable_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Screen_Surface) is
      pragma Unreferenced (Item);
   begin
      --  The buffer came back with the checkpoint (ADR 0025), and the
      --  framebuffer's pages didn't.

      if Buffer_Address /= System.Null_Address then
         Screen.Framebuffer.Map;
         Redraw;
      end if;
   end Restored;

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Item  : in out Screen_Surface;
      Shape : Rose.Interfaces.Surface.Cursor_Shape)
   is
      pragma Unreferenced (Item, Shape);
   begin
      --  The screen has no cursor of its own: the display server draws
      --  one (ADR 0022, section 4).

      null;
   end Set_Cursor;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Have_Framebuffer : Boolean;
   begin
      Rose.System_Calls.Use_Buffer
        (Invocation_Buffer'Address, Invocation_Buffer'Last);

      Screen.Framebuffer.Initialize (Have_Framebuffer);

      if Have_Framebuffer then
         declare
            Bank   : Rose.Interfaces.Bank.Client.Bank_Client :=
                       Rose.Interfaces.Bank.Client.From_Cap (Bank_Cap);
            Region : Rose.Interfaces.Region.Region_Object'Class :=
                       Bank.Reserve_Storage
                         (Word_64 (Screen.Framebuffer.Width)
                          * Word_64 (Screen.Framebuffer.Height) * 4);
         begin
            Buffer_Cap := Region.Region_Cap;
            Buffer_Address := Rose.Shared_Regions.Map (Region);
         end;
         Redraw;
      end if;

      Rose.Interfaces.Surface.Server.Publish
        (Server_Context, The_Surface'Access);
      Rose.Interfaces.Restorable.Server.Attach
        (Server_Context, The_Surface'Access);
      Rose.Interfaces.Stream_Reader.Server.Attach
        (Server_Context, The_Events'Access);

      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

   -----------------------
   -- Stream_Reader_Cap --
   -----------------------

   overriding function Stream_Reader_Cap
     (Item : in out Event_Stream)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Stream_Reader.Server.Cap_Of (Item));

   -----------------
   -- Surface_Cap --
   -----------------

   overriding function Surface_Cap
     (Item : in out Screen_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Surface.Server.Cap_Of (Item));

end Screen.Server;
