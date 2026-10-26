with System.Storage_Elements;

with Rose.Objects;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Writable_File.Server;

package body File.Server is

   use System.Storage_Elements;

   Max_Size : constant := 2 ** 16;
   --  User-mode malloc can't grow (#229), so the contents are a fixed
   --  buffer. Only the pages that are written use memory, but elf
   --  reserves store for the whole of .bss when the process starts, so
   --  the buffer is kept small.

   Context  : Rose.Server.Server_Context;
   Contents : Storage_Array (1 .. Max_Size);
   pragma Suppress_Initialization (Contents);
   --  It is in .bss, so it starts as zeroes. Initializing it would
   --  write every page when the process starts.

   Length   : Storage_Count := 0;
   Position : Storage_Count := 0;
   --  The number of bytes before the next one to read or write. One
   --  position is shared by every holder of the file.

   procedure Read
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : out Storage_Array;
      Last   : out Storage_Count);

   procedure Seek
     (Id                : Rose.Objects.Capability_Identifier;
      Offset_From_Start : Rose.Words.Word_64);

   function Size
     (Id : Rose.Objects.Capability_Identifier)
      return Storage_Count;

   procedure Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : Storage_Array);

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Rose.Interfaces.Writable_File.Server.Publish_Interface
        (Server_Context => Context,
         Read           => Read'Access,
         Seek           => Seek'Access,
         Size           => Size'Access,
         Write          => Write'Access);
   end Create_Server;

   ----------
   -- Read --
   ----------

   procedure Read
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : out Storage_Array;
      Last   : out Storage_Count)
   is
      pragma Unreferenced (Id);
      Count : constant Storage_Count :=
        (if Position >= Length then 0
         else Storage_Count'Min (Buffer'Length, Length - Position));
   begin
      Last := Buffer'First + Count - 1;
      Buffer (Buffer'First .. Last) :=
        Contents (Position + 1 .. Position + Count);
      Position := Position + Count;
   end Read;

   ----------
   -- Seek --
   ----------

   --  The position can be past the end of the file. A write there
   --  leaves zeroes in the gap.

   procedure Seek
     (Id                : Rose.Objects.Capability_Identifier;
      Offset_From_Start : Rose.Words.Word_64)
   is
      pragma Unreferenced (Id);
      use type Rose.Words.Word_64;
   begin
      if Offset_From_Start > Max_Size then
         Position := Max_Size;
      else
         Position := Storage_Count (Offset_From_Start);
      end if;
   end Seek;

   ----------
   -- Size --
   ----------

   function Size
     (Id : Rose.Objects.Capability_Identifier)
      return Storage_Count
   is
      pragma Unreferenced (Id);
   begin
      return Length;
   end Size;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------
   -- Write --
   -----------

   --  Bytes that would go past Max_Size are dropped, since a handler
   --  can't fail its request; the caller sees the shortfall in Size.

   procedure Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : Storage_Array)
   is
      pragma Unreferenced (Id);
      Count : constant Storage_Count :=
        Storage_Count'Min (Buffer'Length, Max_Size - Position);
   begin
      Contents (Position + 1 .. Position + Count) :=
        Buffer (Buffer'First .. Buffer'First + Count - 1);
      Position := Position + Count;
      Length := Storage_Count'Max (Length, Position);
   end Write;

end File.Server;
