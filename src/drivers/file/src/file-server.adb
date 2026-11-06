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
with Rose.Interfaces.Writable_File;
with Rose.Interfaces.Writable_File.Client;
with Rose.Interfaces.Writable_File.Server;

package body File.Server is

   use System.Storage_Elements;

   package Reader_Server renames Rose.Interfaces.Seekable_Reader.Server;
   package File_Server renames Rose.Interfaces.Writable_File.Server;

   subtype Writable_File_Class is
     Rose.Interfaces.Writable_File.Writable_File_Object'Class;

   Max_Size : constant := 2 ** 16;
   --  A file holds at most 64 KiB for now (#215), in a fixed buffer. A
   --  page of it uses memory only once it is written, and space on a
   --  device only once it is written back (#305).

   --  The file and its version (#141) each have a buffer. Committing the
   --  version makes its buffer the file's, so nothing is copied, and the
   --  file's old buffer is the next version's.

   type Buffer_Index is range 1 .. 2;

   Contents : array (Buffer_Index) of Storage_Array (1 .. Max_Size);
   pragma Suppress_Initialization (Contents);
   --  It is in .bss, so it starts as zeroes. Initializing it would
   --  write every page when the process starts.

   Lengths  : array (Buffer_Index) of Storage_Count := (others => 0);
   Current  : Buffer_Index := 1;
   --  The file is Contents (Current), and its version is the other one.

   function Version_Buffer return Buffer_Index
   is (if Current = 1 then 2 else 1);

   Ended_Version : exception;
   --  Raised by an operation on a version that has been committed or
   --  discarded, or on one of its readers.

   --  The file and its version are objects served through Writable_File.
   --  Writable_File is not a resource, so a version's object stays
   --  attached once the version has ended, and its caps reach it, but
   --  every operation on it raises Ended_Version. Each version is a new
   --  object.

   type File_Object is
     limited new File_Server.Writable_File_Interface with
      record
         Is_Version : Boolean := False;
         Ended      : Boolean := False;
      end record;

   overriding function File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of
         (Item, Rose.Interfaces.Writable_File.File_Interface));

   overriding function Stream_Writer_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of
         (Item, Rose.Interfaces.Writable_File.Stream_Writer_Interface));

   overriding function Writable_File_Cap
     (Item : in out File_Object)
      return Rose.Capabilities.Capability
   is (File_Server.Cap_Of (Item));

   overriding function Size
     (Item : in out File_Object)
      return Storage_Count;

   overriding function Open_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class;

   overriding function Open_Seekable_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class;

   overriding procedure Write
     (Item   : in out File_Object;
      Buffer : Storage_Array);

   overriding procedure Truncate
     (Item : in out File_Object;
      Size : Storage_Count);

   type File_Access is access File_Object;

   function Buffer_Of (Item : File_Object'Class) return Buffer_Index;
   --  The buffer of the file or version Item. Raises Ended_Version if
   --  Item is a version that has ended.

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
         Of_Version : Boolean := False;
         --  The reader reads the version, not the file.
         Ended      : Boolean := False;
         --  The reader read a version that has been discarded.
         Position   : Storage_Count := 0;
         --  The number of bytes before the next one to read.
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
      Buffer : out Storage_Array;
      Last   : out Storage_Offset);

   overriding procedure Release (Item : in out Reader_Object);

   overriding procedure Seek
     (Item              : in out Reader_Object;
      Offset_From_Start : Rose.Words.Word_64);

   function Open (Of_Version : Boolean) return Reader_Access;
   --  A reader at the start of the file, or of its version if
   --  Of_Version, attached.

   function Reader_Buffer (Item : Reader_Object'Class) return Buffer_Index;
   --  The buffer that Item reads. Raises Ended_Version if it read a
   --  version that has been discarded.

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

   overriding function New_Version
     (Item : in out Control_Object)
      return Writable_File_Class;

   overriding procedure Commit_Version (Item : in out Control_Object);

   overriding procedure Discard_Version (Item : in out Control_Object);

   procedure End_Version;
   --  The version has been committed or discarded.

   Context  : Rose.Server.Server_Context;
   The_File : aliased File_Object;
   Control  : aliased Control_Object;
   Version  : File_Access;
   --  The file's version, if it has one.
   Readers  : Reader_Access;
   --  Every reader that has been opened, open or not.

   ---------------
   -- Buffer_Of --
   ---------------

   function Buffer_Of (Item : File_Object'Class) return Buffer_Index is
   begin
      if Item.Ended then
         raise Ended_Version;
      end if;
      return (if Item.Is_Version then Version_Buffer else Current);
   end Buffer_Of;

   --------------------
   -- Commit_Version --
   --------------------

   --  The version's readers become the file's, at the same positions,
   --  since the file now has the same contents. A reader of the file
   --  past its new end moves back to it, as for Truncate.

   overriding procedure Commit_Version (Item : in out Control_Object) is
      pragma Unreferenced (Item);
      Reader : Reader_Access := Readers;
   begin
      if Version = null then
         return;
      end if;

      Current := Version_Buffer;
      while Reader /= null loop
         if Reader.Open then
            if Reader.Of_Version then
               Reader.Of_Version := False;
            elsif Reader.Position > Lengths (Current) then
               Reader.Position := Lengths (Current);
            end if;
         end if;
         Reader := Reader.Next;
      end loop;
      End_Version;
   end Commit_Version;

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

   ---------------------
   -- Discard_Version --
   ---------------------

   --  The version's readers end with it. Their caps still reach them
   --  until their holders release them, but they raise Ended_Version.

   overriding procedure Discard_Version (Item : in out Control_Object) is
      pragma Unreferenced (Item);
      Reader : Reader_Access := Readers;
   begin
      if Version = null then
         return;
      end if;

      while Reader /= null loop
         if Reader.Open and then Reader.Of_Version then
            Reader.Ended := True;
         end if;
         Reader := Reader.Next;
      end loop;
      End_Version;
   end Discard_Version;

   -----------------
   -- End_Version --
   -----------------

   procedure End_Version is
   begin
      Version.Ended := True;
      Version := null;
   end End_Version;

   -----------------
   -- New_Version --
   -----------------

   --  A version starts as a copy of the file. The file has at most one
   --  version at a time, and asking for another gives a null cap.

   overriding function New_Version
     (Item : in out Control_Object)
      return Writable_File_Class
   is
      pragma Unreferenced (Item);
      Length : constant Storage_Count := Lengths (Current);
   begin
      if Version /= null then
         return Rose.Interfaces.Writable_File.Client.From_Cap
           (Rose.Capabilities.Null_Capability);
      end if;

      Contents (Version_Buffer) (1 .. Length) :=
        Contents (Current) (1 .. Length);
      Lengths (Version_Buffer) := Length;

      Version := new File_Object;
      Version.Is_Version := True;
      File_Server.Attach (Context, Version);
      return Rose.Interfaces.Writable_File.Client.From_Cap
        (Version.Writable_File_Cap);
   end New_Version;

   ----------
   -- Open --
   ----------

   function Open (Of_Version : Boolean) return Reader_Access is
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
      Reader.Of_Version := Of_Version;
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

   overriding function Open_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Releasable_Reader.Releasable_Reader_Object'Class
   is
      Reader : constant Reader_Access :=
        Open (Of_Version => Buffer_Of (Item) /= Current);
   begin
      return Rose.Interfaces.Releasable_Reader.Client.From_Cap
        (Reader.Releasable_Reader_Cap);
   end Open_Reader;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   overriding function Open_Seekable_Reader
     (Item : in out File_Object)
      return Rose.Interfaces.Seekable_Reader.Seekable_Reader_Object'Class
   is
      Reader : constant Reader_Access :=
        Open (Of_Version => Buffer_Of (Item) /= Current);
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
      From     : constant Buffer_Index := Reader_Buffer (Item);
      Length   : constant Storage_Count := Lengths (From);
      Position : Storage_Count renames Item.Position;
      Count    : constant Storage_Count :=
        (if Position >= Length then 0
         else Storage_Count'Min (Buffer'Length, Length - Position));
   begin
      Last := Buffer'First + Count - 1;
      Buffer (Buffer'First .. Last) :=
        Contents (From) (Position + 1 .. Position + Count);
      Position := Position + Count;
   end Read;

   -------------------
   -- Reader_Buffer --
   -------------------

   function Reader_Buffer (Item : Reader_Object'Class) return Buffer_Index is
   begin
      if Item.Ended then
         raise Ended_Version;
      end if;
      return (if Item.Of_Version then Version_Buffer else Current);
   end Reader_Buffer;

   -------------
   -- Release --
   -------------

   --  The generated handler then detaches the reader.

   overriding procedure Release (Item : in out Reader_Object) is
   begin
      Item.Open := False;
   end Release;

   ----------
   -- Seek --
   ----------

   --  A reader's position is capped at the end of the file (#215).

   overriding procedure Seek
     (Item              : in out Reader_Object;
      Offset_From_Start : Rose.Words.Word_64)
   is
      use type Rose.Words.Word_64;
      Length : constant Storage_Count := Lengths (Reader_Buffer (Item));
   begin
      Item.Position :=
        (if Offset_From_Start > Rose.Words.Word_64 (Length)
         then Length
         else Storage_Count (Offset_From_Start));
   end Seek;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Item : in out File_Object)
      return Storage_Count
   is (Lengths (Buffer_Of (Item)));

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

   --  A reader past the new end moves back to it, as a seek past the end
   --  does (#215).

   overriding procedure Truncate
     (Item : in out File_Object;
      Size : Storage_Count)
   is
      Buffer : constant Buffer_Index := Buffer_Of (Item);
      Reader : Reader_Access := Readers;
   begin
      if Size >= Lengths (Buffer) then
         return;
      end if;

      Lengths (Buffer) := Size;
      while Reader /= null loop
         if Reader.Open
           and then not Reader.Ended
           and then Reader.Of_Version = Item.Is_Version
           and then Reader.Position > Size
         then
            Reader.Position := Size;
         end if;
         Reader := Reader.Next;
      end loop;
   end Truncate;

   --------------
   -- Writable --
   --------------

   overriding function Writable
     (Item : in out Control_Object)
      return Writable_File_Class
   is
      pragma Unreferenced (Item);
   begin
      return Rose.Interfaces.Writable_File.Client.From_Cap
        (The_File.Writable_File_Cap);
   end Writable;

   -----------
   -- Write --
   -----------

   --  Until writers are worked out (#390), a write goes at the end of the
   --  file. Bytes that would go past Max_Size are dropped; the caller
   --  sees the shortfall in Size.

   overriding procedure Write
     (Item   : in out File_Object;
      Buffer : Storage_Array)
   is
      Into   : constant Buffer_Index := Buffer_Of (Item);
      Length : Storage_Count renames Lengths (Into);
      Count  : constant Storage_Count :=
        Storage_Count'Min (Buffer'Length, Max_Size - Length);
   begin
      Contents (Into) (Length + 1 .. Length + Count) :=
        Buffer (Buffer'First .. Buffer'First + Count - 1);
      Length := Length + Count;
   end Write;

end File.Server;
