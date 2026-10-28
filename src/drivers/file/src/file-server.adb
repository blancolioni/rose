with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.Seekable_File_Reader.Server;
with Rose.Interfaces.Writable_File.Server;

package body File.Server is

   use System.Storage_Elements;
   use type Rose.Objects.Capability_Identifier;

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

   --  Readers (ADR 0009). Each has its own position, and is reached
   --  through caps with an identifier of its own. Identifiers are never
   --  used again, so a cap to a released reader finds no reader, even
   --  when its slot holds a new one (ADR 0010, section 3).

   Max_Readers : constant := 16;

   No_Reader : constant Rose.Objects.Capability_Identifier := 0;

   type Reader_Record is
      record
         Identifier    : Rose.Objects.Capability_Identifier := No_Reader;
         Position      : Storage_Count := 0;
         --  The number of bytes before the next one to read.
         Interface_Cap : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The server's own copy of the cap that opened the reader.
      end record;

   type Reader_Array is array (1 .. Max_Readers) of Reader_Record;

   Readers         : Reader_Array;
   Next_Identifier : Rose.Objects.Capability_Identifier := 1;

   procedure Delete (Cap : Rose.Capabilities.Capability);
   --  Delete Cap from the server's cap table. The invocation is made
   --  here, and not through Rose.System_Calls.Delete_Cap, whose call to
   --  Invoke_Capability can be inlined, which breaks the invocation.

   function Find (Id : Rose.Objects.Capability_Identifier) return Natural;
   --  The slot of the reader Id, or 0 if there is no such reader.

   function Open
     (Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability;
   --  A new reader at the start of the file, reached through the
   --  interface Interface_Id, or a null cap if there is no free slot.

   function Open_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Open_Seekable_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Read
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : out Storage_Array;
      Last   : out Storage_Count);

   procedure Release (Id : Rose.Objects.Capability_Identifier);

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
        (Server_Context       => Context,
         Size                 => Size'Access,
         Open_Reader          => Open_Reader'Access,
         Open_Seekable_Reader => Open_Seekable_Reader'Access,
         Write                => Write'Access);
      Rose.Interfaces.Seekable_File_Reader.Server.Attach_Interface
        (Server_Context => Context,
         Read           => Read'Access,
         Release        => Release'Access,
         Seek           => Seek'Access,
         Instanced      => True);
   end Create_Server;

   ------------
   -- Delete --
   ------------

   procedure Delete (Cap : Rose.Capabilities.Capability) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send
        (Params, Rose.System_Calls.Delete_Cap_Capability);
      Rose.System_Calls.Send_Cap (Params, Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Delete;

   ----------
   -- Find --
   ----------

   function Find (Id : Rose.Objects.Capability_Identifier) return Natural is
   begin
      if Id /= No_Reader then
         for Index in Readers'Range loop
            if Readers (Index).Identifier = Id then
               return Index;
            end if;
         end loop;
      end if;
      return 0;
   end Find;

   ----------
   -- Open --
   ----------

   function Open
     (Interface_Id : Rose.Objects.Endpoint_Id)
      return Rose.Capabilities.Capability
   is
   begin
      for Index in Readers'Range loop
         if Readers (Index).Identifier = No_Reader then
            declare
               Reader : Reader_Record renames Readers (Index);
            begin
               Reader.Identifier := Next_Identifier;
               Reader.Position := 0;
               Reader.Interface_Cap :=
                 Rose.Server.Create_Endpoint (Interface_Id, Next_Identifier);
               Next_Identifier := Next_Identifier + 1;
               return Reader.Interface_Cap;
            end;
         end if;
      end loop;
      return Rose.Capabilities.Null_Capability;
   end Open;

   -----------------
   -- Open_Reader --
   -----------------

   --  A sequential reader is reached through the File_Reader interface
   --  of the reader, which has no Seek.

   function Open_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Open
        (Rose.Interfaces.Seekable_File_Reader.File_Reader_Interface);
   end Open_Reader;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   function Open_Seekable_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Open
        (Rose.Interfaces.Seekable_File_Reader.Seekable_File_Reader_Interface);
   end Open_Seekable_Reader;

   ----------
   -- Read --
   ----------

   procedure Read
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : out Storage_Array;
      Last   : out Storage_Count)
   is
      Slot : constant Natural := Find (Id);
   begin
      Last := Buffer'First - 1;
      if Slot = 0 then
         return;
      end if;

      declare
         Position : Storage_Count renames Readers (Slot).Position;
         Count    : constant Storage_Count :=
           (if Position >= Length then 0
            else Storage_Count'Min (Buffer'Length, Length - Position));
      begin
         Last := Buffer'First + Count - 1;
         Buffer (Buffer'First .. Last) :=
           Contents (Position + 1 .. Position + Count);
         Position := Position + Count;
      end;
   end Read;

   -------------
   -- Release --
   -------------

   --  The generated handler then deletes the reader's other caps.

   procedure Release (Id : Rose.Objects.Capability_Identifier) is
      Slot : constant Natural := Find (Id);
   begin
      if Slot /= 0 then
         Delete (Readers (Slot).Interface_Cap);
         Readers (Slot) := (others => <>);
      end if;
   end Release;

   ----------
   -- Seek --
   ----------

   --  A reader's position is capped at the end of the file (#215).

   procedure Seek
     (Id                : Rose.Objects.Capability_Identifier;
      Offset_From_Start : Rose.Words.Word_64)
   is
      use type Rose.Words.Word_64;
      Slot : constant Natural := Find (Id);
   begin
      if Slot /= 0 then
         Readers (Slot).Position :=
           (if Offset_From_Start > Rose.Words.Word_64 (Length)
            then Length
            else Storage_Count (Offset_From_Start));
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

   --  Until writers are worked out (#390), a write goes at the end of the
   --  file. Bytes that would go past Max_Size are dropped, since a
   --  handler can't fail its request; the caller sees the shortfall in
   --  Size.

   procedure Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : Storage_Array)
   is
      pragma Unreferenced (Id);
      Count : constant Storage_Count :=
        Storage_Count'Min (Buffer'Length, Max_Size - Length);
   begin
      Contents (Length + 1 .. Length + Count) :=
        Buffer (Buffer'First .. Buffer'First + Count - 1);
      Length := Length + Count;
   end Write;

end File.Server;
