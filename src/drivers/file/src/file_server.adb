with System.Storage_Elements;

with Rose.Objects;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Writer;
with Rose.Interfaces.Releasable_Writer.Client;
with Rose.Interfaces.Releasable_Writer.Server;
with Rose.Interfaces.Seekable_Reader;
with Rose.Interfaces.Seekable_Reader.Client;
with Rose.Interfaces.Seekable_Reader.Server;
with Rose.Interfaces.Transactional_Writable_File;
with Rose.Interfaces.Transactional_Writable_File.Client;
with Rose.Interfaces.Transactional_Writable_File.Server;
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Server;

package body File_Server is

   use Ada.Streams;
   use System.Storage_Elements;
   use type Rose.Capabilities.Capability;
   use type Rose.Words.Word_32;

   package Reader_Server renames Rose.Interfaces.Seekable_Reader.Server;
   package Writer_Server renames Rose.Interfaces.Releasable_Writer.Server;
   package File_Interfaces renames Rose.Interfaces.Writable_File.Server;
   package Version_Server renames
     Rose.Interfaces.Transactional_Writable_File.Server;
   package Versions renames Rose.Interfaces.Transactional_Writable_File;

   subtype Version_Class is
     Versions.Transactional_Writable_File_Object'Class;
   subtype Writer_Class is
     Rose.Interfaces.Releasable_Writer.Releasable_Writer_Object'Class;

   Format : constant Rose.Words.Word_32 := 1;
   --  The version of this server's part of the state (#867):
   --
   --    the file's identifier
   --    for the file, then the version: its length, then its contents
   --    whether the version is open, and if it is, its identifier
   --    the number of open readers, then for each, its identifier,
   --      buffer, whether it has ended, and position
   --    the number of open writers, then for each, its identifier,
   --      buffer, and whether it has ended
   --
   --  The version's contents are written only while it is open, since
   --  nothing reads them otherwise: a reader of a released version has
   --  ended.

   Service_Name : constant String := "Cap_Set_Service";
   --  The name of its argument, so that a restore finds it.

   Max_Size : constant := 2 ** 16;
   --  A file holds at most 64 KiB for now (#215), in a fixed buffer. A
   --  page of it uses memory only once it is written, and space on a
   --  device only once it is written back (#305).

   --  The file and its version (ADR 0026) each have a buffer.

   type Buffer_Index is (File_Buffer, Version_Buffer);

   Contents : array (Buffer_Index) of Storage_Array (1 .. Max_Size);
   pragma Suppress_Initialization (Contents);
   --  It is in .bss, so it starts as zeroes. Initializing it would
   --  write every page when the process starts.

   Lengths  : array (Buffer_Index) of Storage_Count := (others => 0);

   procedure Copy (From, To : Buffer_Index);
   --  Make the contents of To those of From, and move a reader of To that
   --  is past its new end back to it.

   function Open_Reader
     (Buffer : Buffer_Index)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class;

   function Open_Seekable_Reader
     (Buffer : Buffer_Index)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class;

   function Open_Writer
     (Buffer : Buffer_Index;
      Empty  : Boolean)
      return Writer_Class;
   --  A writer of Buffer, which is emptied first if Empty is True, and
   --  a reader past its new end moves back to it, as a seek past the end
   --  does (#215).

   procedure Write
     (Buffer : Buffer_Index;
      Data   : Storage_Array);
   --  A write goes at the end, since a writer has no position (ADR 0009,
   --  section 4). A write that would go past Max_Size raises, and writes
   --  nothing.

   --  The file is an object served through Writable_File.

   type File_Object is
     limited new File_Interfaces.Writable_File_Interface with null record;

   overriding function File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Interfaces.Cap_Of
         (Item, Rose.Interfaces.Writable_File.File_Interface));

   overriding function Writable_File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Interfaces.Cap_Of (Item));

   overriding function Size
     (Item : in out File_Object)
      return Storage_Count
   is (Lengths (File_Buffer));

   overriding function Open_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class
   is (Open_Reader (File_Buffer));

   overriding function Open_Seekable_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class
   is (Open_Seekable_Reader (File_Buffer));

   overriding function Append
     (Item : in out File_Object)
      return Writer_Class
   is (Open_Writer (File_Buffer, Empty => False));

   overriding function Rewrite
     (Item : in out File_Object)
      return Writer_Class
   is (Open_Writer (File_Buffer, Empty => True));

   overriding function New_Version
     (Item : in out File_Object)
      return Version_Class;

   --  The version is an object served through Transactional_Writable_File
   --  (ADR 0026). The file has at most one at a time. Releasing it
   --  detaches it, and New_Version attaches the same object again, with a
   --  new identifier, so that the caps of a released version fail.

   type Version_Object is
     limited new Version_Server.Transactional_Writable_File_Interface with
      record
         Open : Boolean := False;
      end record;

   overriding function File_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item, Versions.File_Interface));

   overriding function Writable_File_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item, Versions.Writable_File_Interface));

   overriding function Transactional_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item, Versions.Transactional_Interface));

   overriding function Resource_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item, Versions.Resource_Interface));

   overriding function Transactional_Writable_File_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item));

   overriding function Size
     (Item : in out Version_Object)
      return Storage_Count
   is (Lengths (Version_Buffer));

   overriding function Open_Reader
     (Item : in out Version_Object)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class
   is (Open_Reader (Version_Buffer));

   overriding function Open_Seekable_Reader
     (Item : in out Version_Object)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class
   is (Open_Seekable_Reader (Version_Buffer));

   overriding function Append
     (Item : in out Version_Object)
      return Writer_Class
   is (Open_Writer (Version_Buffer, Empty => False));

   overriding function Rewrite
     (Item : in out Version_Object)
      return Writer_Class
   is (Open_Writer (Version_Buffer, Empty => True));

   overriding function New_Version
     (Item : in out Version_Object)
      return Version_Class;

   overriding procedure Commit (Item : in out Version_Object);

   overriding procedure Rollback (Item : in out Version_Object);

   overriding procedure Release (Item : in out Version_Object);

   --  Readers (ADR 0009). Each has its own position. A reader is a
   --  resource: releasing it detaches it, and its object goes back to the
   --  pool, to be attached again by the next open, with a new identifier
   --  (ADR 0010, section 3). The process has no cap to rescind with, since
   --  rescinding one object's caps can't be done yet (#769), so a released
   --  reader's slot and identifier are never used again, and a call
   --  through its caps fails.

   type Reader_Object;

   type Reader_Access is access Reader_Object;

   type Reader_Object is
     limited new Reader_Server.Seekable_Reader_Interface with
      record
         Open       : Boolean := False;
         Buffer     : Buffer_Index := File_Buffer;
         --  What the reader reads.
         Ended      : Boolean := False;
         --  The reader read a version that has been released.
         Position   : Storage_Count := 0;
         --  The number of bytes before the next one to read.
         Identifier : Rose.Objects.Capability_Identifier := 0;
         --  The identifier it had in the version that this one took
         --  over from, which Create_Server attaches it with, or 0.
         Next       : Reader_Access;
         --  The next reader in the pool.
      end record;

   overriding function Reader_Cap
     (Item : in out Reader_Object)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Item, Rose.Interfaces.Seekable_Reader.Reader_Interface));

   overriding function Resource_Cap
     (Item : in out Reader_Object)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Item, Rose.Interfaces.Seekable_Reader.Resource_Interface));

   overriding function Releasable_Reader_Cap
     (Item : in out Reader_Object)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Item, Rose.Interfaces.Seekable_Reader.Releasable_Reader_Interface));

   overriding function Seekable_Cap
     (Item : in out Reader_Object)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of
         (Item, Rose.Interfaces.Seekable_Reader.Seekable_Interface));

   overriding function Seekable_Reader_Cap
     (Item : in out Reader_Object)
      return Rose.Capabilities.Capability
   is (Reader_Server.Cap_Of (Item));

   overriding procedure Read
     (Item   : in out Reader_Object;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Release (Item : in out Reader_Object);

   overriding procedure Seek
     (Item              : in out Reader_Object;
      Offset_From_Start : Rose.Words.Word_64);

   function Open (Buffer : Buffer_Index) return Reader_Access;
   --  A reader at the start of Buffer, attached.

   procedure Check (Item : Reader_Object'Class);
   --  Raise Ended_Version if Item read a version that has been released.

   --  Writers (ADR 0009, section 4). A writer has no position, since it
   --  writes at the end, so any number can be open at once. A writer is a
   --  resource, and comes from a pool, as a reader does.

   type Writer_Object;

   type Writer_Access is access Writer_Object;

   type Writer_Object is
     limited new Writer_Server.Releasable_Writer_Interface with
      record
         Open       : Boolean := False;
         Buffer     : Buffer_Index := File_Buffer;
         --  What the writer writes.
         Ended      : Boolean := False;
         --  The writer wrote a version that has been released.
         Identifier : Rose.Objects.Capability_Identifier := 0;
         --  As a reader's.
         Next       : Writer_Access;
         --  The next writer in the pool.
      end record;

   overriding function Writer_Cap
     (Item : in out Writer_Object)
      return Rose.Capabilities.Capability
   is (Writer_Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Writer.Writer_Interface));

   overriding function Resource_Cap
     (Item : in out Writer_Object)
      return Rose.Capabilities.Capability
   is (Writer_Server.Cap_Of
         (Item, Rose.Interfaces.Releasable_Writer.Resource_Interface));

   overriding function Releasable_Writer_Cap
     (Item : in out Writer_Object)
      return Rose.Capabilities.Capability
   is (Writer_Server.Cap_Of (Item));

   overriding procedure Write
     (Item   : in out Writer_Object;
      Buffer : Stream_Element_Array);

   overriding procedure Release (Item : in out Writer_Object);

   procedure Check (Item : Writer_Object'Class);
   --  Raise Ended_Version if Item wrote a version that has been released.

   Ended_Version : exception;

   Context     : aliased Rose.Server.Server_Context;
   The_File    : aliased File_Object;
   The_Version : aliased Version_Object;
   Readers     : Reader_Access;
   --  Every reader that has been opened, open or not.
   Writers     : Writer_Access;
   --  Every writer that has been opened, open or not.

   Endpoint_Version : aliased Version_Object;
   Endpoint_Reader  : aliased Reader_Object;
   Endpoint_Writer  : aliased Writer_Object;
   --  Objects that are attached when the server starts, and never given
   --  out. A server package registers its endpoints when its first object
   --  is attached, so without them, a version that had opened a version,
   --  a reader or a writer, would have endpoints that a new version that
   --  hasn't lacks, and the kernel would refuse to swap them (#866). The
   --  reader and writer have ended, so a call to them fails.

   Service_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   File_Id     : Rose.Objects.Capability_Identifier := 0;
   Version_Id  : Rose.Objects.Capability_Identifier := 0;
   --  The identifiers that the file and its version had in the version
   --  of the program that this one took over from, or 0.
   Taken_Over  : Boolean := False;
   --  The state was read, from an old version or a saved one.

   procedure Read_Contents
     (Stream : not null access Root_Stream_Type'Class;
      Buffer : Buffer_Index);
   --  Read the length and the contents of Buffer, as Write_Contents
   --  wrote them.

   procedure Write_Contents
     (Stream : not null access Root_Stream_Type'Class;
      Buffer : Buffer_Index;
      Length : Storage_Count);
   --  Write Length, then the first Length bytes of Buffer, in one piece.

   -----------
   -- Check --
   -----------

   procedure Check (Item : Reader_Object'Class) is
   begin
      if Item.Ended then
         raise Ended_Version;
      end if;
   end Check;

   -----------
   -- Check --
   -----------

   procedure Check (Item : Writer_Object'Class) is
   begin
      if Item.Ended then
         raise Ended_Version;
      end if;
   end Check;

   ------------
   -- Commit --
   ------------

   --  The version stays open, as a copy of what was committed.

   overriding procedure Commit (Item : in out Version_Object) is
   begin
      Copy (From => Version_Buffer, To => File_Buffer);
   end Commit;

   ----------
   -- Copy --
   ----------

   procedure Copy (From, To : Buffer_Index) is
      Length : constant Storage_Count := Lengths (From);
      Reader : Reader_Access := Readers;
   begin
      Contents (To) (1 .. Length) := Contents (From) (1 .. Length);
      Lengths (To) := Length;
      while Reader /= null loop
         if Reader.Open
           and then Reader.Buffer = To
           and then Reader.Position > Length
         then
            Reader.Position := Length;
         end if;
         Reader := Reader.Next;
      end loop;
   end Copy;

   -------------------
   -- Create_Server --
   -------------------

   --  The file is attached first, so that a fresh one gets the first
   --  identifier. A server that took over attaches each object with the
   --  identifier it had, which the caps that clients hold name.

   procedure Create_Server
     (Cap_Set_Service : Rose.Capabilities.Capability;
      Served          : out Rose.Capabilities.Capability;
      Control         : out Rose.Capabilities.Capability)
   is
      Reader : Reader_Access := Readers;
      Writer : Writer_Access := Writers;
   begin
      if not Taken_Over then
         Service_Cap := Cap_Set_Service;
      end if;

      File_Interfaces.Attach (Context, The_File'Access, File_Id);
      if The_Version.Open then
         Version_Server.Attach (Context, The_Version'Access, Version_Id);
      end if;

      while Reader /= null loop
         if Reader.Open then
            Reader_Server.Attach (Context, Reader, Reader.Identifier);
         end if;
         Reader := Reader.Next;
      end loop;

      while Writer /= null loop
         if Writer.Open then
            Writer_Server.Attach (Context, Writer, Writer.Identifier);
         end if;
         Writer := Writer.Next;
      end loop;

      Endpoint_Reader.Ended := True;
      Endpoint_Writer.Ended := True;
      Version_Server.Attach (Context, Endpoint_Version'Access);
      Reader_Server.Attach (Context, Endpoint_Reader'Access);
      Writer_Server.Attach (Context, Endpoint_Writer'Access);

      Rose.Replacement.Attach
        (Context'Access, Write_State'Access, Service_Cap);

      Served := The_File.Writable_File_Cap;
      Control := Rose.Replacement.Replaceable_Cap;
   end Create_Server;

   -----------------
   -- New_Version --
   -----------------

   --  A version starts as a copy of the file.

   overriding function New_Version
     (Item : in out File_Object)
      return Version_Class
   is
   begin
      if The_Version.Open then
         raise Constraint_Error with "the file already has a version";
      end if;

      Copy (From => File_Buffer, To => Version_Buffer);
      The_Version.Open := True;
      Version_Server.Attach (Context, The_Version'Access);
      return Versions.Client.From_Cap
        (The_Version.Transactional_Writable_File_Cap);
   end New_Version;

   -----------------
   -- New_Version --
   -----------------

   overriding function New_Version
     (Item : in out Version_Object)
      return Version_Class
   is (raise Constraint_Error with "a version has no versions");

   ----------
   -- Open --
   ----------

   function Open (Buffer : Buffer_Index) return Reader_Access is
      Reader : Reader_Access := Readers;
   begin
      while Reader /= null and then Reader.Open loop
         Reader := Reader.Next;
      end loop;

      if Reader = null then
         Reader := new Reader_Object;
         Reader.Next := Readers;
         Readers := Reader;
      end if;

      Reader.Open := True;
      Reader.Buffer := Buffer;
      Reader.Ended := False;
      Reader.Position := 0;
      Reader_Server.Attach (Context, Reader);
      return Reader;
   end Open;

   -----------------
   -- Open_Reader --
   -----------------

   --  A sequential reader is reached through the Releasable_Reader
   --  interface of the reader, which has no Seek.

   function Open_Reader
     (Buffer : Buffer_Index)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class
   is
      Reader : constant Reader_Access := Open (Buffer);
   begin
      return Rose.Interfaces.Releasable_Reader.Client.From_Cap
        (Reader.Releasable_Reader_Cap);
   end Open_Reader;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   function Open_Seekable_Reader
     (Buffer : Buffer_Index)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class
   is
      Reader : constant Reader_Access := Open (Buffer);
   begin
      return Rose.Interfaces.Seekable_Reader.Client.From_Cap
        (Reader.Seekable_Reader_Cap);
   end Open_Seekable_Reader;

   -----------------
   -- Open_Writer --
   -----------------

   function Open_Writer
     (Buffer : Buffer_Index;
      Empty  : Boolean)
      return Writer_Class
   is
      Writer : Writer_Access := Writers;
   begin
      if Empty then
         Lengths (Buffer) := 0;
         declare
            Reader : Reader_Access := Readers;
         begin
            while Reader /= null loop
               if Reader.Open and then Reader.Buffer = Buffer then
                  Reader.Position := 0;
               end if;
               Reader := Reader.Next;
            end loop;
         end;
      end if;

      while Writer /= null and then Writer.Open loop
         Writer := Writer.Next;
      end loop;

      if Writer = null then
         Writer := new Writer_Object;
         Writer.Next := Writers;
         Writers := Writer;
      end if;

      Writer.Open := True;
      Writer.Buffer := Buffer;
      Writer.Ended := False;
      Writer_Server.Attach (Context, Writer);
      return Rose.Interfaces.Releasable_Writer.Client.From_Cap
        (Writer.Releasable_Writer_Cap);
   end Open_Writer;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Reader_Object;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Length   : constant Storage_Count := Lengths (Item.Buffer);
      Position : Storage_Count renames Item.Position;
      Count    : constant Storage_Count :=
        (if Position >= Length then 0
         else Storage_Count'Min (Buffer'Length, Length - Position));
      Target   : Storage_Array (1 .. Count)
        with Import, Address => Buffer'Address;
      --  The start of Buffer, as the file's contents are stored.
   begin
      Check (Item);
      Last := Buffer'First + Stream_Element_Offset (Count) - 1;
      Target := Contents (Item.Buffer) (Position + 1 .. Position + Count);
      Position := Position + Count;
   end Read;

   -------------------
   -- Read_Contents --
   -------------------

   procedure Read_Contents
     (Stream : not null access Root_Stream_Type'Class;
      Buffer : Buffer_Index)
   is
      Length : Storage_Count;
   begin
      Storage_Count'Read (Stream, Length);
      if Length > Max_Size then
         raise Rose.Replacement.Bad_State with "file: too long";
      end if;

      declare
         Data : Stream_Element_Array (1 .. Stream_Element_Offset (Length))
           with Import, Address => Contents (Buffer)'Address;
         Next : Stream_Element_Offset := Data'First;
         Last : Stream_Element_Offset;
      begin
         while Next <= Data'Last loop
            Read (Stream.all, Data (Next .. Data'Last), Last);
            if Last < Next then
               raise Rose.Replacement.Bad_State with "file: state ends";
            end if;
            Next := Last + 1;
         end loop;
      end;

      Lengths (Buffer) := Length;
   end Read_Contents;

   ----------------
   -- Read_State --
   ----------------

   procedure Read_State
     (Stream : not null access Root_Stream_Type'Class;
      Named  : Rose.Replacement.Named_Caps)
   is
      Version : Rose.Words.Word_32;
      Count   : Natural;
   begin
      Rose.Words.Word_32'Read (Stream, Version);
      if Version /= Format then
         raise Rose.Replacement.Bad_State with "file: unknown format";
      end if;

      Rose.Objects.Capability_Identifier'Read (Stream, File_Id);
      Read_Contents (Stream, File_Buffer);
      Read_Contents (Stream, Version_Buffer);
      Boolean'Read (Stream, The_Version.Open);
      if The_Version.Open then
         Rose.Objects.Capability_Identifier'Read (Stream, Version_Id);
      end if;

      Natural'Read (Stream, Count);
      for I in 1 .. Count loop
         declare
            Reader : constant Reader_Access := new Reader_Object;
         begin
            Rose.Objects.Capability_Identifier'Read
              (Stream, Reader.Identifier);
            Buffer_Index'Read (Stream, Reader.Buffer);
            Boolean'Read (Stream, Reader.Ended);
            Storage_Count'Read (Stream, Reader.Position);
            Reader.Position :=
              Storage_Count'Min (Reader.Position, Lengths (Reader.Buffer));
            Reader.Open := True;
            Reader.Next := Readers;
            Readers := Reader;
         end;
      end loop;

      Natural'Read (Stream, Count);
      for I in 1 .. Count loop
         declare
            Writer : constant Writer_Access := new Writer_Object;
         begin
            Rose.Objects.Capability_Identifier'Read
              (Stream, Writer.Identifier);
            Buffer_Index'Read (Stream, Writer.Buffer);
            Boolean'Read (Stream, Writer.Ended);
            Writer.Open := True;
            Writer.Next := Writers;
            Writers := Writer;
         end;
      end loop;

      Service_Cap := Named.Cap (Service_Name);
      if Service_Cap = Rose.Capabilities.Null_Capability then
         raise Rose.Replacement.Bad_State with "file: no " & Service_Name;
      end if;

      Taken_Over := True;
   end Read_State;

   -------------
   -- Release --
   -------------

   --  The generated handler then detaches the reader.

   overriding procedure Release (Item : in out Reader_Object) is
   begin
      Item.Open := False;
   end Release;

   -------------
   -- Release --
   -------------

   --  What wasn't committed is dropped, and the version's readers and
   --  writers end with it: their caps still reach them until their
   --  holders release them, but they raise Ended_Version. The generated
   --  handler then detaches the version.

   overriding procedure Release (Item : in out Version_Object) is
      Reader : Reader_Access := Readers;
      Writer : Writer_Access := Writers;
   begin
      while Reader /= null loop
         if Reader.Open and then Reader.Buffer = Version_Buffer then
            Reader.Ended := True;
         end if;
         Reader := Reader.Next;
      end loop;
      while Writer /= null loop
         if Writer.Open and then Writer.Buffer = Version_Buffer then
            Writer.Ended := True;
         end if;
         Writer := Writer.Next;
      end loop;
      Item.Open := False;
   end Release;

   -------------
   -- Release --
   -------------

   --  The generated handler then detaches the writer.

   overriding procedure Release (Item : in out Writer_Object) is
   begin
      Item.Open := False;
   end Release;

   --------------
   -- Rollback --
   --------------

   overriding procedure Rollback (Item : in out Version_Object) is
   begin
      Copy (From => File_Buffer, To => Version_Buffer);
   end Rollback;

   ----------
   -- Seek --
   ----------

   --  A reader's position is capped at the end of the file (#215).

   overriding procedure Seek
     (Item              : in out Reader_Object;
      Offset_From_Start : Rose.Words.Word_64)
   is
      use type Rose.Words.Word_64;
      Length : constant Storage_Count := Lengths (Item.Buffer);
   begin
      Check (Item);
      Item.Position :=
        (if Offset_From_Start > Rose.Words.Word_64 (Length)
         then Length
         else Storage_Count (Offset_From_Start));
   end Seek;

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

   procedure Write
     (Buffer : Buffer_Index;
      Data   : Storage_Array)
   is
      Length : Storage_Count renames Lengths (Buffer);
   begin
      if Data'Length > Max_Size - Length then
         raise Constraint_Error with "the file is full";
      end if;

      Contents (Buffer) (Length + 1 .. Length + Data'Length) := Data;
      Length := Length + Data'Length;
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Writer_Object;
      Buffer : Stream_Element_Array)
   is
      Data : constant Storage_Array (1 .. Buffer'Length)
        with Import, Address => Buffer'Address;
      --  Buffer, as the file's contents are stored.
   begin
      Check (Item);
      Write (Item.Buffer, Data);
   end Write;

   --------------------
   -- Write_Contents --
   --------------------

   procedure Write_Contents
     (Stream : not null access Root_Stream_Type'Class;
      Buffer : Buffer_Index;
      Length : Storage_Count)
   is
      Data : Stream_Element_Array (1 .. Stream_Element_Offset (Length))
        with Import, Address => Contents (Buffer)'Address;
   begin
      Storage_Count'Write (Stream, Length);
      Write (Stream.all, Data);
   end Write_Contents;

   -----------------
   -- Write_State --
   -----------------

   procedure Write_State
     (Stream : not null access Root_Stream_Type'Class;
      Named  : in out Rose.Replacement.Named_Caps)
   is
      Count  : Natural := 0;
      Reader : Reader_Access := Readers;
      Writer : Writer_Access := Writers;
   begin
      Rose.Words.Word_32'Write (Stream, Format);
      Rose.Objects.Capability_Identifier'Write
        (Stream, File_Interfaces.Identifier_Of (The_File'Access));
      Write_Contents (Stream, File_Buffer, Lengths (File_Buffer));
      Write_Contents
        (Stream, Version_Buffer,
         (if The_Version.Open then Lengths (Version_Buffer) else 0));
      Boolean'Write (Stream, The_Version.Open);
      if The_Version.Open then
         Rose.Objects.Capability_Identifier'Write
           (Stream, Version_Server.Identifier_Of (The_Version'Access));
      end if;

      while Reader /= null loop
         if Reader.Open then
            Count := Count + 1;
         end if;
         Reader := Reader.Next;
      end loop;
      Natural'Write (Stream, Count);
      Reader := Readers;
      while Reader /= null loop
         if Reader.Open then
            Rose.Objects.Capability_Identifier'Write
              (Stream, Reader_Server.Identifier_Of (Reader));
            Buffer_Index'Write (Stream, Reader.Buffer);
            Boolean'Write (Stream, Reader.Ended);
            Storage_Count'Write (Stream, Reader.Position);
         end if;
         Reader := Reader.Next;
      end loop;

      Count := 0;
      while Writer /= null loop
         if Writer.Open then
            Count := Count + 1;
         end if;
         Writer := Writer.Next;
      end loop;
      Natural'Write (Stream, Count);
      Writer := Writers;
      while Writer /= null loop
         if Writer.Open then
            Rose.Objects.Capability_Identifier'Write
              (Stream, Writer_Server.Identifier_Of (Writer));
            Buffer_Index'Write (Stream, Writer.Buffer);
            Boolean'Write (Stream, Writer.Ended);
         end if;
         Writer := Writer.Next;
      end loop;

      Named.Add (Service_Name, Service_Cap);
   end Write_State;

end File_Server;
