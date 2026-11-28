with Ada.Streams;
with System.Storage_Elements;

with Interfaces;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Server;
with Rose.Shared_Regions;
with Rose.Surface_Events;
with Rose.System_Calls;
with Rose.System_Calls.Own_Bank;
with Rose.Words;

with Rose.Interfaces.Storage.Bank.Client;
with Rose.Interfaces.Storage.Region;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Storage.Region_Access.Client;
with Rose.Interfaces.Streams.Reader.Client;
with Rose.Interfaces.Streams.Reader.Server;
with Rose.Interfaces.Screens.Surface;
with Rose.Interfaces.Screens.Test_Surface.Server;

package body Memory_Surface_Server is

   use Rose.Words;
   use type System.Address;
   use type Rose.Capabilities.Capability;

   --  One surface, whose buffer is a region of the process's own bank,
   --  and whose events are whatever the test injects (#583). It draws
   --  nothing anywhere: a test reads the buffer to see what its holder
   --  drew.

   type Memory_Surface is limited new
     Rose.Interfaces.Screens.Test_Surface.Server.Test_Surface_Interface
     with null record;

   overriding procedure Get_Size
     (Item   : in out Memory_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word);

   overriding function Get_Buffer
     (Item : in out Memory_Surface)
      return Rose.Interfaces.Storage.Region_Access.Region_Access_Object'Class;

   overriding procedure Damage
     (Item   : in out Memory_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word);

   overriding function Events
     (Item : in out Memory_Surface)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class;

   overriding procedure Get_Pointer
     (Item    : in out Memory_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word);

   overriding procedure Notify
     (Item   : in out Memory_Surface;
      Signal : Rose.Capabilities.Capability);

   overriding procedure Set_Cursor
     (Item  : in out Memory_Surface;
      Shape : Rose.Interfaces.Screens.Surface.Cursor_Shape);

   overriding procedure Release (Item : in out Memory_Surface);

   overriding procedure Inject
     (Item   : in out Memory_Surface;
      Kind   : Rose.Words.Word;
      First  : Rose.Words.Word;
      Second : Rose.Words.Word;
      Third  : Rose.Words.Word);

   overriding procedure Set_Size
     (Item   : in out Memory_Surface;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word);

   overriding function Test_Surface_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability;

   overriding function Surface_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability;

   overriding function Resource_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability;

   type Event_Stream is limited new
     Rose.Interfaces.Streams.Reader.Server.Reader_Interface
     with null record;

   overriding procedure Read
     (Item   : in out Event_Stream;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding function Reader_Cap
     (Item : in out Event_Stream)
      return Rose.Capabilities.Capability;

   Context     : Rose.Server.Server_Context;
   The_Surface : aliased Memory_Surface;
   The_Events  : aliased Event_Stream;
   Queue       : Rose.Surface_Events.Event_Queue;

   Holder_Signal : Rose.Capabilities.Capability :=
                     Rose.Capabilities.Null_Capability;
   --  The signal cap that the holder gave with Notify, or null.

   Surface_Width  : Word := 0;
   Surface_Height : Word := 0;
   Buffer_Address : System.Address := System.Null_Address;
   Buffer_Region  : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;
   Buffer_Access  : Rose.Capabilities.Capability :=
                      Rose.Capabilities.Null_Capability;
   --  The buffer, where this process maps it, its Region, which only this
   --  process holds, and its Region_Access, which the holder is given.

   Invocation_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   Initial_Width  : constant := 320;
   Initial_Height : constant := 200;

   procedure New_Buffer (Width, Height : Word);
   --  Release the buffer, if there is one, and reserve and map another.

   procedure Event_Queued;
   --  Give a waiting Read the event, and signal the holder.

   ------------
   -- Damage --
   ------------

   overriding procedure Damage
     (Item   : in out Memory_Surface;
      X      : Rose.Words.Word;
      Y      : Rose.Words.Word;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
   is
      pragma Unreferenced (Item, X, Y, Width, Height);
   begin
      --  The pixels are already in the buffer, which is all there is.

      null;
   end Damage;

   ------------------
   -- Event_Queued --
   ------------------

   procedure Event_Queued is
   begin
      Rose.Server.Unblock_Endpoint
        (Context, Rose.Interfaces.Streams.Reader.Read_Endpoint);

      if Holder_Signal /= Rose.Capabilities.Null_Capability then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Holder_Signal);
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end if;
   end Event_Queued;

   ------------
   -- Events --
   ------------

   overriding function Events
     (Item : in out Memory_Surface)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Streams.Reader.Client.From_Cap
        (Rose.Interfaces.Streams.Reader.Server.Cap_Of
           (Context, The_Events));
   end Events;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Item : in out Memory_Surface)
      return Rose.Interfaces.Storage.Region_Access.Region_Access_Object'Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Storage.Region_Access.Client.From_Cap
        (Buffer_Access);
   end Get_Buffer;

   -----------------
   -- Get_Pointer --
   -----------------

   overriding procedure Get_Pointer
     (Item    : in out Memory_Surface;
      Over    : out Boolean;
      X       : out Interfaces.Integer_32;
      Y       : out Interfaces.Integer_32;
      Buttons : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Over := False;
      X := 0;
      Y := 0;
      Buttons := 0;
   end Get_Pointer;

   --------------
   -- Get_Size --
   --------------

   overriding procedure Get_Size
     (Item   : in out Memory_Surface;
      Width  : out Rose.Words.Word;
      Height : out Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Width := Surface_Width;
      Height := Surface_Height;
   end Get_Size;

   ------------
   -- Inject --
   ------------

   overriding procedure Inject
     (Item   : in out Memory_Surface;
      Kind   : Rose.Words.Word;
      First  : Rose.Words.Word;
      Second : Rose.Words.Word;
      Third  : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      Rose.Surface_Events.Add
        (Queue, Word_32 (Kind), Word_32 (First), Word_32 (Second),
         Word_32 (Third));
      Event_Queued;
   end Inject;

   ----------------
   -- New_Buffer --
   ----------------

   procedure New_Buffer (Width, Height : Word) is
   begin
      if Buffer_Address /= System.Null_Address then
         Rose.Shared_Regions.Unmap (Buffer_Address);
         Buffer_Address := System.Null_Address;
         declare
            Old : Rose.Interfaces.Storage.Region.Client.Region_Client :=
                    Rose.Interfaces.Storage.Region.Client.From_Cap
                      (Buffer_Region);
         begin
            Old.Release;
         end;
      end if;

      declare
         Bank   : Rose.Interfaces.Storage.Bank.Client.Bank_Client :=
                    Rose.Interfaces.Storage.Bank.Client.From_Cap
                      (Rose.System_Calls.Own_Bank.Cap);
         Region : Rose.Interfaces.Storage.Region.Region_Object'Class :=
                    Bank.Reserve_Storage
                      (Word_64 (Width) * Word_64 (Height) * 4);
      begin
         Buffer_Region := Region.Region_Cap;
         Buffer_Access := Region.Region_Access_Cap;
         Buffer_Address := Rose.Shared_Regions.Map (Region);
      end;

      Surface_Width := Width;
      Surface_Height := Height;
   end New_Buffer;

   ------------
   -- Notify --
   ------------

   overriding procedure Notify
     (Item   : in out Memory_Surface;
      Signal : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Item);
   begin
      Holder_Signal := Signal;
   end Notify;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Event_Stream;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Item);
   begin
      if Rose.Surface_Events.Is_Empty (Queue)
        and then Holder_Signal = Rose.Capabilities.Null_Capability
      then
         Buffer := (others => 0);
         Last := 0;
         Rose.Server.Block_Current_Request (Context);
      else
         Rose.Surface_Events.Take (Queue, Buffer, Last);
      end if;
   end Read;

   ----------------
   -- Reader_Cap --
   ----------------

   overriding function Reader_Cap
     (Item : in out Event_Stream)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Streams.Reader.Server.Cap_Of (Context, Item));

   -------------
   -- Release --
   -------------

   overriding procedure Release (Item : in out Memory_Surface) is
      pragma Unreferenced (Item);
   begin
      --  The test ends the process, and its bank goes with it.

      null;
   end Release;

   ------------------
   -- Resource_Cap --
   ------------------

   overriding function Resource_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Screens.Test_Surface.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Screens.Test_Surface.Resource_Interface));

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Item  : in out Memory_Surface;
      Shape : Rose.Interfaces.Screens.Surface.Cursor_Shape)
   is
      pragma Unreferenced (Item, Shape);
   begin
      null;
   end Set_Cursor;

   --------------
   -- Set_Size --
   --------------

   overriding procedure Set_Size
     (Item   : in out Memory_Surface;
      Width  : Rose.Words.Word;
      Height : Rose.Words.Word)
   is
      pragma Unreferenced (Item);
   begin
      New_Buffer (Width, Height);
      Rose.Surface_Events.Add
        (Queue, Rose.Surface_Events.Size_Event,
         Word_32 (Width), Word_32 (Height));
      Event_Queued;
   end Set_Size;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.System_Calls.Use_Buffer
        (Invocation_Buffer'Address, Invocation_Buffer'Last);
      New_Buffer (Initial_Width, Initial_Height);
      Rose.Interfaces.Screens.Test_Surface.Server.Publish
        (Context, The_Surface'Access);
      Rose.Interfaces.Streams.Reader.Server.Attach
        (Context, The_Events'Access);
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------------
   -- Surface_Cap --
   -----------------

   overriding function Surface_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Screens.Test_Surface.Server.Cap_Of
         (Context, Item,
          Rose.Interfaces.Screens.Test_Surface.Surface_Interface));

   ----------------------
   -- Test_Surface_Cap --
   ----------------------

   overriding function Test_Surface_Cap
     (Item : in out Memory_Surface)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Screens.Test_Surface.Server.Cap_Of (Context, Item));

end Memory_Surface_Server;
