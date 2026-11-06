with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Server;
with Rose.Words;

with Rose.Interfaces.File_Control.Server;
with Rose.Interfaces.Releasable_Reader;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Seekable_Reader;
with Rose.Interfaces.Seekable_Reader.Client;
with Rose.Interfaces.Seekable_Reader.Server;
with Rose.Interfaces.Transactional_Writable_File;
with Rose.Interfaces.Transactional_Writable_File.Client;
with Rose.Interfaces.Transactional_Writable_File.Server;
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Client;
with Rose.Interfaces.Writable_File.Server;

package body File.Server is

   use System.Storage_Elements;

   package Reader_Server renames Rose.Interfaces.Seekable_Reader.Server;
   package File_Server renames Rose.Interfaces.Writable_File.Server;
   package Version_Server renames
     Rose.Interfaces.Transactional_Writable_File.Server;
   package Versions renames Rose.Interfaces.Transactional_Writable_File;

   subtype Writable_File_Class is
     Rose.Interfaces.Writable_File.Writable_File_Object'Class;
   subtype Version_Class is
     Versions.Transactional_Writable_File_Object'Class;

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

   procedure Truncate
     (Buffer : Buffer_Index;
      Size   : Storage_Count);
   --  Shorten Buffer to Size, and move a reader past the new end back to
   --  it, as a seek past the end does (#215).

   procedure Write
     (Buffer : Buffer_Index;
      Data   : Storage_Array);
   --  Until writers are worked out (#390), a write goes at the end. A
   --  write that would go past Max_Size raises, and writes nothing.

   --  The file is an object served through Writable_File.

   type File_Object is
     limited new File_Server.Writable_File_Interface with null record;

   overriding function File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of
         (Item, Rose.Interfaces.Writable_File.File_Interface));

   overriding function Writer_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of
         (Item, Rose.Interfaces.Writable_File.Writer_Interface));

   overriding function Writable_File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of (Item));

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

   overriding procedure Write
     (Item   : in out File_Object;
      Buffer : Storage_Array);

   overriding procedure Truncate
     (Item : in out File_Object;
      Size : Storage_Count);

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

   overriding function Writer_Cap
     (Item : in out Version_Object)
      return Rose.Capabilities.Capability
   is (Version_Server.Cap_Of (Item, Versions.Writer_Interface));

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

   overriding procedure Write
     (Item   : in out Version_Object;
      Buffer : Storage_Array);

   overriding procedure Truncate
     (Item : in out Version_Object;
      Size : Storage_Count);

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
         Open     : Boolean := False;
         Buffer   : Buffer_Index := File_Buffer;
         --  What the reader reads.
         Ended    : Boolean := False;
         --  The reader read a version that has been released.
         Position : Storage_Count := 0;
         --  The number of bytes before the next one to read.
         Next     : Reader_Access;
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
      Buffer : out Storage_Array;
      Last   : out Storage_Offset);

   overriding procedure Release (Item : in out Reader_Object);

   overriding procedure Seek
     (Item              : in out Reader_Object;
      Offset_From_Start : Rose.Words.Word_64);

   function Open (Buffer : Buffer_Index) return Reader_Access;
   --  A reader at the start of Buffer, attached.

   procedure Check (Item : Reader_Object'Class);
   --  Raise Ended_Version if Item read a version that has been released.

   Ended_Version : exception;

   --  Only the directory holds File_Control, which the process publishes.
   --  Everyone else is given the file's Writable_File, or its File.

   type Control_Object is
     limited new Rose.Interfaces.File_Control.Server.File_Control_Interface
     with null record;

   overriding function File_Control_Cap
     (Item : in out Control_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.File_Control.Server.Cap_Of (Item));

   overriding function Writable
     (Item : in out Control_Object)
      return Writable_File_Class;

   Context     : Rose.Server.Server_Context;
   The_File    : aliased File_Object;
   The_Version : aliased Version_Object;
   Control     : aliased Control_Object;
   Readers     : Reader_Access;
   --  Every reader that has been opened, open or not.

   -----------
   -- Check --
   -----------

   procedure Check (Item : Reader_Object'Class) is
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

   --  The file is attached first, so that a cap with identifier 0, such
   --  as a restore notice's, reaches it.

   procedure Create_Server is
   begin
      File_Server.Attach (Context, The_File'Access);
      Rose.Interfaces.File_Control.Server.Publish (Context, Control'Access);
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

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Item   : in out Reader_Object;
      Buffer : out Storage_Array;
      Last   : out Storage_Offset)
   is
      Length   : constant Storage_Count := Lengths (Item.Buffer);
      Position : Storage_Count renames Item.Position;
      Count    : constant Storage_Count :=
        (if Position >= Length then 0
         else Storage_Count'Min (Buffer'Length, Length - Position));
   begin
      Check (Item);
      Last := Buffer'First + Count - 1;
      Buffer (Buffer'First .. Last) :=
        Contents (Item.Buffer) (Position + 1 .. Position + Count);
      Position := Position + Count;
   end Read;

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

   --  What wasn't committed is dropped, and the version's readers end
   --  with it: their caps still reach them until their holders release
   --  them, but they raise Ended_Version. The generated handler then
   --  detaches the version.

   overriding procedure Release (Item : in out Version_Object) is
      Reader : Reader_Access := Readers;
   begin
      while Reader /= null loop
         if Reader.Open and then Reader.Buffer = Version_Buffer then
            Reader.Ended := True;
         end if;
         Reader := Reader.Next;
      end loop;
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

   --------------
   -- Truncate --
   --------------

   procedure Truncate
     (Buffer : Buffer_Index;
      Size   : Storage_Count)
   is
      Reader : Reader_Access := Readers;
   begin
      if Size >= Lengths (Buffer) then
         return;
      end if;

      Lengths (Buffer) := Size;
      while Reader /= null loop
         if Reader.Open
           and then Reader.Buffer = Buffer
           and then Reader.Position > Size
         then
            Reader.Position := Size;
         end if;
         Reader := Reader.Next;
      end loop;
   end Truncate;

   --------------
   -- Truncate --
   --------------

   overriding procedure Truncate
     (Item : in out File_Object;
      Size : Storage_Count)
   is
   begin
      Truncate (File_Buffer, Size);
   end Truncate;

   --------------
   -- Truncate --
   --------------

   overriding procedure Truncate
     (Item : in out Version_Object;
      Size : Storage_Count)
   is
   begin
      Truncate (Version_Buffer, Size);
   end Truncate;

   --------------
   -- Writable --
   --------------

   overriding function Writable
     (Item : in out Control_Object)
      return Writable_File_Class
   is (Rose.Interfaces.Writable_File.Client.From_Cap
         (The_File.Writable_File_Cap));

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
     (Item   : in out File_Object;
      Buffer : Storage_Array)
   is
   begin
      Write (File_Buffer, Buffer);
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Item   : in out Version_Object;
      Buffer : Storage_Array)
   is
   begin
      Write (Version_Buffer, Buffer);
   end Write;

end File.Server;
