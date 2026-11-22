with Ada.Streams;
with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Streams.Stream.Server;

package body Null_Stream_Server is

   package Stream_Server renames Rose.Interfaces.Streams.Stream.Server;

   Context : aliased Rose.Server.Server_Context;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Data goes through the invoke buffer; without one, a read or a
   --  write would fail.

   type Null_Object is limited new Stream_Server.Stream_Interface
     with null record;

   overriding function Reader_Cap
     (Item : in out Null_Object)
      return Rose.Capabilities.Capability
   is (Stream_Server.Cap_Of
         (Context, Item, Rose.Interfaces.Streams.Stream.Reader_Interface));

   overriding function Writer_Cap
     (Item : in out Null_Object)
      return Rose.Capabilities.Capability
   is (Stream_Server.Cap_Of
         (Context, Item, Rose.Interfaces.Streams.Stream.Writer_Interface));

   overriding function Stream_Cap
     (Item : in out Null_Object)
      return Rose.Capabilities.Capability
   is (Stream_Server.Cap_Of (Context, Item));

   overriding procedure Read
     (Item   : in out Null_Object;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  End of file, always.

   overriding procedure Write
     (Item   : in out Null_Object;
      Buffer : Ada.Streams.Stream_Element_Array)
   is null;
   --  Succeed, and do nothing else.

   The_Stream : aliased Null_Object;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Served : out Rose.Capabilities.Capability)
   is
   begin
      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);
      Stream_Server.Attach (Context, The_Stream'Access);
      Served := The_Stream.Stream_Cap;
   end Create_Server;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Null_Object;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Item);
      use type Ada.Streams.Stream_Element_Offset;
   begin
      Buffer := (others => 0);
      Last := Buffer'First - 1;
   end Read;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Null_Stream_Server;
