with Ada.Streams;
with System.Storage_Elements;

with Interfaces;

with Rose.Drawing;
with Rose.Invocation;
with Rose.Server;
with Rose.Shared_Regions;
with Rose.Surface_Events;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Objects;
with Rose.Interfaces.Screens.Display.Server;
with Rose.Interfaces.Screens.Surface.Client;
with Rose.Interfaces.Screens.Surface.Server;
with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Storage.Region_Access;
with Rose.Interfaces.Storage.Region_Access.Client;
with Rose.Interfaces.Streams.Reader.Client;
with Rose.Interfaces.Streams.Releasable_Reader.Server;

with Trellis_Layout;

package body Trellis_Server is

   use Rose.Words;
   use type System.Address;
   use type Rose.Capabilities.Capability;

   package Surface_Server renames Rose.Interfaces.Screens.Surface.Server;
   package Reader_Server renames
     Rose.Interfaces.Streams.Releasable_Reader.Server;

   --  The design (#583): calls go down, from a client to trellis to the
   --  root, and signals come up. Trellis never calls a client: it invokes
   --  a client's signal cap, which never waits, so it can't deadlock with
   --  a client that is calling it. Windows tile and never overlap, so the
   --  root's buffer is the composed screen, and a client's damage is
   --  copied straight into it.

   Max_Windows : constant := Trellis_Layout.Max_Windows;

   subtype Window_Index is Positive range 1 .. Max_Windows;

   Windows_Allowed : constant := 1;
   --  Windows that can exist at once: one, which has the whole root,
   --  until the split.

   type Window_Surface is limited new Surface_Server.Surface_Interface
     with record
        Index : Window_Index := 1;
     end record;
   --  A client's surface, served by trellis.

   overriding procedure Get_Size
     (Item   : in out Window_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word);

   overriding function Get_Buffer
     (Item : in out Window_Surface)
      return Rose.Interfaces.Storage.Region_Access.Region_Access_Object'Class;

   overriding procedure Damage
     (Item   : in out Window_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word);

   overriding procedure Notify
     (Item   : in out Window_Surface;
      Signal : Rose.Capabilities.Capability);

   overriding function Events
     (Item : in out Window_Surface)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class;

   overriding procedure Get_Pointer
     (Item    : in out Window_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word);

   overriding procedure Set_Cursor
     (Item  : in out Window_Surface;
      Shape : Rose.Interfaces.Screens.Surface.Cursor_Shape);

   overriding procedure Release (Item : in out Window_Surface);

   overriding function Surface_Cap
     (Item : in out Window_Surface)
      return Rose.Capabilities.Capability;

   overriding function Resource_Cap
     (Item : in out Window_Surface)
      return Rose.Capabilities.Capability;

   type Window_Events is limited new
     Reader_Server.Releasable_Reader_Interface
     with record
        Index : Window_Index := 1;
     end record;
   --  A client's surface's events. They are a Releasable_Reader only so
   --  that trellis can detach them when the surface is released, which
   --  makes the caps to them fail, so that a client's events can't be
   --  read through the caps of an earlier client of the same window. A
   --  client is given only the Reader, and can't release it.

   overriding procedure Release (Item : in out Window_Events);

   overriding function Releasable_Reader_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability;

   overriding function Resource_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability;

   overriding procedure Read
     (Item   : in out Window_Events;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding function Reader_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability;

   type Display_Object is limited new
     Rose.Interfaces.Screens.Display.Server.Display_Interface
     with null record;

   overriding function Create_Surface
     (Item   : in out Display_Object;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
      return Rose.Interfaces.Screens.Surface.Surface_Object'Class;

   overriding function Display_Cap
     (Item : in out Display_Object)
      return Rose.Capabilities.Capability;

   type Window is
      record
         Used          : Boolean := False;
         Area          : Trellis_Layout.Rectangle;
         --  Where the window is on the root, which is its size.
         Region        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         Region_Access : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         Address       : System.Address := System.Null_Address;
         --  Its buffer: the Region, which only trellis holds, so that a
         --  client can't release the buffer while trellis copies from it
         --  (#403); the Region_Access that the client is given; and where
         --  trellis maps it.
         Queue         : Rose.Surface_Events.Event_Queue;
         Signal        : Rose.Capabilities.Capability :=
                           Rose.Capabilities.Null_Capability;
         --  The client's signal cap, from Notify, or null.
         Cursor        : Rose.Interfaces.Screens.Surface.Cursor_Shape :=
                           Rose.Interfaces.Screens.Surface.Arrow;
         --  The shape it asked for, which is drawn from #589.
      end record;

   Context     : Rose.Server.Server_Context;
   The_Display : aliased Display_Object;
   Windows     : array (Window_Index) of Window;
   Surfaces    : array (Window_Index) of aliased Window_Surface;
   Readers     : array (Window_Index) of aliased Window_Events;

   Root          : Rose.Interfaces.Screens.Surface.Client.Surface_Client;
   Root_Events   : Rose.Interfaces.Streams.Reader.Client.Reader_Client;
   Root_Address  : System.Address := System.Null_Address;
   Root_Width    : Natural := 0;
   Root_Height   : Natural := 0;

   Root_Signal_Bit : constant := 8;
   --  Trellis's own signal bit, which the root invokes when it queues an
   --  event.

   Black : constant Rose.Drawing.Pixel := 0;
   White : constant Rose.Drawing.Pixel := 16#00FF_FFFF#;

   Invocation_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   function Root_Bitmap return Rose.Drawing.Bitmap
   is (Rose.Drawing.To_Bitmap (Root_Address, Root_Width, Root_Height));

   procedure Damage_Root (Area : Trellis_Layout.Rectangle);
   --  Tell the root that Area of its buffer has changed.

   procedure Draw_Background (Area : Trellis_Layout.Rectangle);
   --  Draw the weave over Area of the root.

   procedure Event_Queued (Index : Window_Index);
   --  Give a waiting Read of window Index's events the event, and signal
   --  its client.

   procedure On_Root_Events;
   --  The root has signalled: read its events until there are none.

   procedure Free_Buffer (Index : Window_Index);
   --  Unmap and release window Index's buffer.

   function Window_Count return Natural;

   --------------------
   -- Create_Server --
   --------------------

   procedure Create_Server
     (Root   : in out Rose.Interfaces.Screens.Surface.Surface_Object'Class;
      Served : out Rose.Capabilities.Capability)
   is
      Width, Height : Word;
   begin
      Rose.System_Calls.Use_Buffer
        (Invocation_Buffer'Address, Invocation_Buffer'Last);

      Rose.Interfaces.Screens.Surface.Client.Open
        (Trellis_Server.Root, Root.Surface_Cap);
      Trellis_Server.Root.Get_Size (Width, Height);
      Root_Width := Natural (Width);
      Root_Height := Natural (Height);

      declare
         Buffer : Rose.Interfaces.Storage.Region_Access
           .Region_Access_Object'Class := Trellis_Server.Root.Get_Buffer;
      begin
         Root_Address := Rose.Shared_Regions.Map (Buffer);
      end;

      declare
         Events : Rose.Interfaces.Streams.Reader.Reader_Object'Class :=
                    Trellis_Server.Root.Events;
      begin
         Rose.Interfaces.Streams.Reader.Client.Open
           (Root_Events, Events.Reader_Cap);
      end;

      Draw_Background ((0, 0, Root_Width, Root_Height));
      Damage_Root ((0, 0, Root_Width, Root_Height));

      for Index in Window_Index loop
         Surfaces (Index).Index := Index;
         Readers (Index).Index := Index;
      end loop;

      Rose.Interfaces.Screens.Display.Server.Publish
        (Context, The_Display'Access);
      Served := The_Display.Display_Cap;

      Rose.Server.Set_Signal_Handler
        (Context, Root_Signal_Bit, On_Root_Events'Access);
      Trellis_Server.Root.Notify
        (Rose.System_Calls.Create_Signal_Cap (Root_Signal_Bit));
   end Create_Server;

   --------------------
   -- Create_Surface --
   --------------------

   overriding function Create_Surface
     (Item   : in out Display_Object;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
      return Rose.Interfaces.Screens.Surface.Surface_Object'Class
   is
      pragma Unreferenced (Item, Width, Height);
      --  The layout decides the size.
   begin
      if Window_Count >= Windows_Allowed then
         raise Rose.Interfaces.Objects.Invocation_Error
           with "no room for another surface";
      end if;

      for Index in Window_Index loop
         if not Windows (Index).Used then
            declare
               W    : Window renames Windows (Index);
               Area : constant Trellis_Layout.Rectangle :=
                        Trellis_Layout.Tile
                          (1, 1, Root_Width, Root_Height);
               Bank : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                        Rose.Interfaces.Storage.Bank.Client.From_Cap
                          (Rose.System_Calls.Own_Bank.Cap);
               Region : Rose.Interfaces.Storage.Region.Region_Object'Class
                 := Bank.Reserve_Storage
                   (Word_64 (Area.Width) * Word_64 (Area.Height) * 4);
            begin
               W.Area := Area;
               W.Region := Region.Region_Cap;
               W.Region_Access := Region.Region_Access_Cap;
               W.Address := Rose.Shared_Regions.Map (Region);
               W.Signal := Rose.Capabilities.Null_Capability;
               W.Cursor := Rose.Interfaces.Screens.Surface.Arrow;
               Rose.Surface_Events.Clear (W.Queue);
               W.Used := True;

               Surface_Server.Attach (Context, Surfaces (Index)'Access);
               Reader_Server.Attach (Context, Readers (Index)'Access);

               return Rose.Interfaces.Screens.Surface.Client.From_Cap
                 (Surfaces (Index).Surface_Cap);
            end;
         end if;
      end loop;

      raise Rose.Interfaces.Objects.Invocation_Error
        with "no room for another surface";
   end Create_Surface;

   ------------
   -- Damage --
   ------------

   overriding procedure Damage
     (Item   : in out Window_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
   is
      W    : Window renames Windows (Item.Index);
      Part : constant Trellis_Layout.Rectangle :=
               Trellis_Layout.Clip
                 (X, Y, Width, Height, W.Area.Width, W.Area.Height);
   begin
      if not W.Used or else Trellis_Layout.Is_Empty (Part) then
         return;
      end if;

      Rose.Drawing.Copy
        (Source => Rose.Drawing.To_Bitmap
                     (W.Address, W.Area.Width, W.Area.Height),
         Area   => (Part.X, Part.Y, Part.Width, Part.Height),
         Target => Root_Bitmap,
         X      => W.Area.X + Part.X,
         Y      => W.Area.Y + Part.Y);
      Damage_Root (Trellis_Layout.Offset (Part, W.Area.X, W.Area.Y));
   end Damage;

   -----------------
   -- Damage_Root --
   -----------------

   procedure Damage_Root (Area : Trellis_Layout.Rectangle) is
   begin
      if not Trellis_Layout.Is_Empty (Area) then
         Root.Damage
           (Word (Area.X), Word (Area.Y),
            Word (Area.Width), Word (Area.Height));
      end if;
   end Damage_Root;

   -----------------
   -- Display_Cap --
   -----------------

   overriding function Display_Cap
     (Item : in out Display_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Screens.Display.Server.Cap_Of (Context, Item));

   ---------------------
   -- Draw_Background --
   ---------------------

   procedure Draw_Background (Area : Trellis_Layout.Rectangle) is
      Pixels : array (0 .. Root_Width * Root_Height - 1)
        of Rose.Drawing.Pixel
        with Import, Address => Root_Address;
   begin
      for Y in Area.Y .. Area.Y + Area.Height - 1 loop
         for X in Area.X .. Area.X + Area.Width - 1 loop
            Pixels (Y * Root_Width + X) :=
              (if Trellis_Layout.Is_Weave (X, Y) then Black else White);
         end loop;
      end loop;
   end Draw_Background;

   ------------------
   -- Event_Queued --
   ------------------

   procedure Event_Queued (Index : Window_Index) is
      Signal : constant Rose.Capabilities.Capability :=
                 Windows (Index).Signal;
   begin
      Rose.Server.Unblock_Endpoint
        (Context, Rose.Interfaces.Streams.Reader.Read_Endpoint);

      if Signal /= Rose.Capabilities.Null_Capability then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Signal);
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end if;
   end Event_Queued;

   ------------
   -- Events --
   ------------

   overriding function Events
     (Item : in out Window_Surface)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class
   is (Rose.Interfaces.Streams.Reader.Client.From_Cap
         (Readers (Item.Index).Reader_Cap));

   -----------------
   -- Free_Buffer --
   -----------------

   procedure Free_Buffer (Index : Window_Index) is
      W : Window renames Windows (Index);
   begin
      if W.Address /= System.Null_Address then
         Rose.Shared_Regions.Unmap (W.Address);
         W.Address := System.Null_Address;
      end if;

      if W.Region /= Rose.Capabilities.Null_Capability then
         declare
            Region : Rose.Interfaces.Storage.Region.Client.Region_Client :=
                       Rose.Interfaces.Storage.Region.Client.From_Cap
                         (W.Region);
         begin
            Region.Release;
         end;
         W.Region := Rose.Capabilities.Null_Capability;
         W.Region_Access := Rose.Capabilities.Null_Capability;
      end if;
   end Free_Buffer;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Item : in out Window_Surface)
      return Rose.Interfaces.Storage.Region_Access.Region_Access_Object'Class
   is (Rose.Interfaces.Storage.Region_Access.Client.From_Cap
         (Windows (Item.Index).Region_Access));

   -----------------
   -- Get_Pointer --
   -----------------

   overriding procedure Get_Pointer
     (Item    : in out Window_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      --  The pointer is routed from #589. Until then, it is over no
      --  surface.

      Over := False;
      X := 0;
      Y := 0;
      Buttons := 0;
   end Get_Pointer;

   --------------
   -- Get_Size --
   --------------

   overriding procedure Get_Size
     (Item   : in out Window_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word)
   is
      W : Window renames Windows (Item.Index);
   begin
      Width := Word (W.Area.Width);
      Height := Word (W.Area.Height);
   end Get_Size;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Item   : in out Window_Surface;
      Signal : Rose.Capabilities.Capability)
   is
   begin
      Windows (Item.Index).Signal := Signal;
   end Notify;

   --------------------
   -- On_Root_Events --
   --------------------

   procedure On_Root_Events is
      use Ada.Streams;
      use Rose.Surface_Events;
      Buffer : Stream_Element_Array (1 .. 16 * Event_Bytes);
      Last   : Stream_Element_Offset;
   begin
      loop
         Root_Events.Read (Buffer, Last);
         exit when Last < Buffer'First;

         declare
            Start : Stream_Element_Offset := Buffer'First;
         begin
            while Start + Event_Bytes - 1 <= Last loop
               declare
                  Kind : constant Word_32 := Word_At (Buffer, Start);
               begin

                  --  Keys go to the first window until #584 brings
                  --  focus. The pointer and the wheel are routed from
                  --  #589; until then, they and any kind this version
                  --  doesn't know are dropped.

                  if Kind = Key_Event then
                     for Index in Window_Index loop
                        if Windows (Index).Used then
                           Add (Windows (Index).Queue, Kind,
                                Word_At (Buffer, Start + 4),
                                Word_At (Buffer, Start + 8),
                                Word_At (Buffer, Start + 12));
                           Event_Queued (Index);
                           exit;
                        end if;
                     end loop;
                  end if;
               end;
               Start := Start + Event_Bytes;
            end loop;
         end;
      end loop;
   end On_Root_Events;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Window_Events;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      W : Window renames Windows (Item.Index);
   begin
      if Rose.Surface_Events.Is_Empty (W.Queue)
        and then W.Signal = Rose.Capabilities.Null_Capability
      then
         Buffer := (others => 0);
         Last := 0;
         Rose.Server.Block_Current_Request (Context);
      else
         Rose.Surface_Events.Take (W.Queue, Buffer, Last);
      end if;
   end Read;

   ----------------
   -- Reader_Cap --
   ----------------

   overriding function Reader_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Streams.Releasable_Reader.Reader_Interface));

   ---------------------------
   -- Releasable_Reader_Cap --
   ---------------------------

   overriding function Releasable_Reader_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of (Context, Item));

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Window_Events) is
      pragma Unreferenced (Item);
   begin
      --  No client holds this interface: trellis detaches the events
      --  itself when their surface is released.

      null;
   end Release;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Window_Surface) is
      W : Window renames Windows (Item.Index);
   begin
      if not W.Used then
         return;
      end if;

      --  The generated handler detaches the surface. Its events go with
      --  it, and the background takes its place.

      Reader_Server.Detach (Context, Readers (Item.Index)'Access);
      Free_Buffer (Item.Index);
      Rose.Surface_Events.Clear (W.Queue);
      W.Signal := Rose.Capabilities.Null_Capability;
      W.Used := False;
      Draw_Background (W.Area);
      Damage_Root (W.Area);
   end Release;

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Window_Events)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Streams.Releasable_Reader.Resource_Interface));

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Window_Surface)
      return Rose.Capabilities.Capability
   is (Surface_Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Screens.Surface.Resource_Interface));

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Item  : in out Window_Surface;
      Shape : Rose.Interfaces.Screens.Surface.Cursor_Shape)
   is
   begin
      Windows (Item.Index).Cursor := Shape;
   end Set_Cursor;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------------
   -- Surface_Cap --
   -----------------

   overriding function Surface_Cap
     (Item : in out Window_Surface)
      return Rose.Capabilities.Capability
   is (Surface_Server.Cap_Of (Context, Item));

   ------------------
   -- Window_Count --
   ------------------

   function Window_Count return Natural is
      Count : Natural := 0;
   begin
      for W of Windows loop
         if W.Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Window_Count;

end Trellis_Server;
