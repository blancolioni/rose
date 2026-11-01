with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Interfaces.File_Control.Server;
with Rose.Interfaces.Seekable_File_Reader.Server;
with Rose.Interfaces.Writable_File.Server;

package body File.Server is

   use System.Storage_Elements;
   use type Rose.Objects.Capability_Identifier;

   Max_Size : constant := 2 ** 16;
   --  A file holds at most 64 KiB for now (#215), in a fixed buffer. A
   --  page of it uses memory only once it is written, and space on a
   --  device only once it is written back (#305).

   --  The file and its version (#141) each have a buffer. Committing the
   --  version makes its buffer the file's, so nothing is copied, and the
   --  file's old buffer is the next version's.

   type Buffer_Index is range 1 .. 2;

   Context  : Rose.Server.Server_Context;
   Contents : array (Buffer_Index) of Storage_Array (1 .. Max_Size);
   pragma Suppress_Initialization (Contents);
   --  It is in .bss, so it starts as zeroes. Initializing it would
   --  write every page when the process starts.

   Lengths  : array (Buffer_Index) of Storage_Count := (others => 0);
   Current  : Buffer_Index := 1;
   --  The file is Contents (Current), and its version is the other one.

   function Version_Buffer return Buffer_Index
   is (if Current = 1 then 2 else 1);

   --  The file and its version are reached through Writable_File caps
   --  with identifiers of their own: the file's is The_File, and each
   --  version has a new one, so the caps of a version that was committed
   --  or discarded find nothing.

   The_File   : constant Rose.Objects.Capability_Identifier := 0;
   No_Version : constant Rose.Objects.Capability_Identifier := 0;

   Version     : Rose.Objects.Capability_Identifier := No_Version;
   Version_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   --  The server's own copy of the cap that New_Version returned.

   --  Readers (ADR 0009). Each has its own position, and is reached
   --  through caps with an identifier of its own. Identifiers are never
   --  used again, so a cap to a released reader finds no reader, even
   --  when its slot holds a new one (ADR 0010, section 3).

   Max_Readers : constant := 16;

   No_Reader : constant Rose.Objects.Capability_Identifier := 0;

   type Reader_Record is
      record
         Identifier    : Rose.Objects.Capability_Identifier := No_Reader;
         Of_Version    : Boolean := False;
         --  The reader reads the version, not the file.
         Position      : Storage_Count := 0;
         --  The number of bytes before the next one to read.
         Interface_Cap : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The server's own copy of the cap that opened the reader.
      end record;

   type Reader_Array is array (1 .. Max_Readers) of Reader_Record;

   Readers         : Reader_Array;
   Next_Identifier : Rose.Objects.Capability_Identifier := 1;

   function Buffer_Of
     (Id : Rose.Objects.Capability_Identifier)
      return Natural
   is (if Id = The_File then Natural (Current)
       elsif Version /= No_Version and then Id = Version
       then Natural (Version_Buffer)
       else 0);
   --  The buffer of the file or version that Id names, or 0 if it names
   --  neither.

   function Reader_Buffer (Slot : Positive) return Buffer_Index
   is (if Readers (Slot).Of_Version then Version_Buffer else Current);

   procedure Commit_Version (Id : Rose.Objects.Capability_Identifier);

   procedure Discard_Version (Id : Rose.Objects.Capability_Identifier);

   procedure End_Version;
   --  Forget the version, and delete the server's caps for it.

   function Find (Id : Rose.Objects.Capability_Identifier) return Natural;
   --  The slot of the reader Id, or 0 if there is no such reader.

   function New_Version
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Open
     (Interface_Id : Rose.Objects.Endpoint_Id;
      Of_Version   : Boolean)
      return Rose.Capabilities.Capability;
   --  A new reader at the start of the file, or of its version if
   --  Of_Version, reached through the interface Interface_Id, or a null
   --  cap if there is no free slot.

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

   procedure Truncate
     (Id   : Rose.Objects.Capability_Identifier;
      Size : Storage_Count);

   function Writable
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   procedure Write
     (Id     : Rose.Objects.Capability_Identifier;
      Buffer : Storage_Array);

   --------------------
   -- Commit_Version --
   --------------------

   --  The version's readers become the file's, at the same positions,
   --  since the file now has the same contents. A reader of the file
   --  past its new end moves back to it, as for Truncate.

   procedure Commit_Version (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
   begin
      if Version = No_Version then
         return;
      end if;

      Current := Version_Buffer;
      for Reader of Readers loop
         if Reader.Identifier /= No_Reader then
            if Reader.Of_Version then
               Reader.Of_Version := False;
            elsif Reader.Position > Lengths (Current) then
               Reader.Position := Lengths (Current);
            end if;
         end if;
      end loop;
      End_Version;
   end Commit_Version;

   -------------------
   -- Create_Server --
   -------------------

   --  Only the directory holds File_Control, which the process publishes.
   --  Everyone else is given the file's Writable_File, or its File.
   --  Writable_File is instanced, so that the file and its version each
   --  have caps of their own.

   procedure Create_Server is
   begin
      Rose.Interfaces.Writable_File.Server.Attach_Interface
        (Server_Context       => Context,
         Size                 => Size'Access,
         Open_Reader          => Open_Reader'Access,
         Open_Seekable_Reader => Open_Seekable_Reader'Access,
         Write                => Write'Access,
         Truncate             => Truncate'Access,
         Instanced            => True);
      Rose.Interfaces.Seekable_File_Reader.Server.Attach_Interface
        (Server_Context => Context,
         Read           => Read'Access,
         Release        => Release'Access,
         Seek           => Seek'Access,
         Instanced      => True);
      Rose.Interfaces.File_Control.Server.Publish_Interface
        (Server_Context  => Context,
         Writable        => Writable'Access,
         New_Version     => New_Version'Access,
         Commit_Version  => Commit_Version'Access,
         Discard_Version => Discard_Version'Access);
   end Create_Server;

   ---------------------
   -- Discard_Version --
   ---------------------

   --  The version's readers are released, as if their holders had
   --  released them.

   procedure Discard_Version (Id : Rose.Objects.Capability_Identifier) is
      pragma Unreferenced (Id);
   begin
      if Version = No_Version then
         return;
      end if;

      for Reader of Readers loop
         if Reader.Identifier /= No_Reader and then Reader.Of_Version then
            Rose.Interfaces.Seekable_File_Reader.Server.Remove_Instance
              (Reader.Identifier);
            Rose.System_Calls.Delete_Cap (Reader.Interface_Cap);
            Reader := (others => <>);
         end if;
      end loop;
      End_Version;
   end Discard_Version;

   -----------------
   -- End_Version --
   -----------------

   procedure End_Version is
   begin
      Rose.Interfaces.Writable_File.Server.Remove_Instance (Version);
      Rose.System_Calls.Delete_Cap (Version_Cap);
      Version_Cap := Rose.Capabilities.Null_Capability;
      Version := No_Version;
   end End_Version;

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

   -----------------
   -- New_Version --
   -----------------

   --  A version starts as a copy of the file. The file has at most one
   --  version at a time.

   function New_Version
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
      Length : constant Storage_Count := Lengths (Current);
   begin
      if Version /= No_Version then
         return Rose.Capabilities.Null_Capability;
      end if;

      Contents (Version_Buffer) (1 .. Length) :=
        Contents (Current) (1 .. Length);
      Lengths (Version_Buffer) := Length;

      Version := Next_Identifier;
      Next_Identifier := Next_Identifier + 1;
      Version_Cap :=
        Rose.Server.Create_Endpoint
          (Rose.Interfaces.Writable_File.Writable_File_Interface, Version);
      return Version_Cap;
   end New_Version;

   ----------
   -- Open --
   ----------

   function Open
     (Interface_Id : Rose.Objects.Endpoint_Id;
      Of_Version   : Boolean)
      return Rose.Capabilities.Capability
   is
   begin
      for Index in Readers'Range loop
         if Readers (Index).Identifier = No_Reader then
            declare
               Reader : Reader_Record renames Readers (Index);
            begin
               Reader.Identifier := Next_Identifier;
               Reader.Of_Version := Of_Version;
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
   begin
      if Buffer_Of (Id) = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Open
        (Rose.Interfaces.Seekable_File_Reader.File_Reader_Interface,
         Of_Version => Id /= The_File);
   end Open_Reader;

   --------------------------
   -- Open_Seekable_Reader --
   --------------------------

   function Open_Seekable_Reader
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      if Buffer_Of (Id) = 0 then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Open
        (Rose.Interfaces.Seekable_File_Reader.Seekable_File_Reader_Interface,
         Of_Version => Id /= The_File);
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
         From     : constant Buffer_Index := Reader_Buffer (Slot);
         Length   : constant Storage_Count := Lengths (From);
         Position : Storage_Count renames Readers (Slot).Position;
         Count    : constant Storage_Count :=
           (if Position >= Length then 0
            else Storage_Count'Min (Buffer'Length, Length - Position));
      begin
         Last := Buffer'First + Count - 1;
         Buffer (Buffer'First .. Last) :=
           Contents (From) (Position + 1 .. Position + Count);
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
         Rose.System_Calls.Delete_Cap (Readers (Slot).Interface_Cap);
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
         declare
            Length : constant Storage_Count :=
              Lengths (Reader_Buffer (Slot));
         begin
            Readers (Slot).Position :=
              (if Offset_From_Start > Rose.Words.Word_64 (Length)
               then Length
               else Storage_Count (Offset_From_Start));
         end;
      end if;
   end Seek;

   ----------
   -- Size --
   ----------

   function Size
     (Id : Rose.Objects.Capability_Identifier)
      return Storage_Count
   is
      Buffer : constant Natural := Buffer_Of (Id);
   begin
      return (if Buffer = 0 then 0 else Lengths (Buffer_Index (Buffer)));
   end Size;

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

   procedure Truncate
     (Id   : Rose.Objects.Capability_Identifier;
      Size : Storage_Count)
   is
      Buffer : constant Natural := Buffer_Of (Id);
   begin
      if Buffer = 0 or else Size >= Lengths (Buffer_Index (Buffer)) then
         return;
      end if;

      Lengths (Buffer_Index (Buffer)) := Size;
      for Reader of Readers loop
         if Reader.Identifier /= No_Reader
           and then Reader.Of_Version = (Id /= The_File)
           and then Reader.Position > Size
         then
            Reader.Position := Size;
         end if;
      end loop;
   end Truncate;

   --------------
   -- Writable --
   --------------

   function Writable
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Rose.Server.Create_Endpoint
        (Rose.Interfaces.Writable_File.Writable_File_Interface, The_File);
   end Writable;

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
      Target : constant Natural := Buffer_Of (Id);
   begin
      if Target = 0 then
         return;
      end if;

      declare
         Into   : constant Buffer_Index := Buffer_Index (Target);
         Length : Storage_Count renames Lengths (Into);
         Count  : constant Storage_Count :=
           Storage_Count'Min (Buffer'Length, Max_Size - Length);
      begin
         Contents (Into) (Length + 1 .. Length + Count) :=
           Buffer (Buffer'First .. Buffer'First + Count - 1);
         Length := Length + Count;
      end;
   end Write;

end File.Server;
