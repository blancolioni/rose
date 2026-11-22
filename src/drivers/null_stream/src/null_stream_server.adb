with Ada.Streams;
with System.Storage_Elements;

with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Common.Tree;
with Rose.Interfaces.Common.Tree.Client;
with Rose.Interfaces.Files.Directory;
with Rose.Interfaces.Files.Directory.Client;
with Rose.Interfaces.Files.File;
with Rose.Interfaces.Files.File.Client;
with Rose.Interfaces.Files.Fixed_Directory;
with Rose.Interfaces.Files.Fixed_Directory.Client;
with Rose.Interfaces.Files.Writable_Directory;
with Rose.Interfaces.Files.Writable_Directory.Client;
with Rose.Interfaces.Files.Writable_Directory.Server;
with Rose.Interfaces.Files.Writable_File;
with Rose.Interfaces.Files.Writable_File.Client;
with Rose.Interfaces.Streams.Readable;
with Rose.Interfaces.Streams.Readable.Client;
with Rose.Interfaces.Streams.Reader;
with Rose.Interfaces.Streams.Reader.Client;
with Rose.Interfaces.Streams.Stream;
with Rose.Interfaces.Streams.Stream.Client;
with Rose.Interfaces.Streams.Stream.Server;

package body Null_Stream_Server is

   package Stream_Server renames Rose.Interfaces.Streams.Stream.Server;

   Context : aliased Rose.Server.Server_Context;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;
   --  Data and names go through the invoke buffer; without one, a read,
   --  a write or a call with a name would fail.

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

   --  The null directory (ADR 0040, section 5; #1045): a
   --  Writable_Directory that has no entries and refuses every change.
   --  Each operation that needs an entry raises, as for a name that isn't
   --  there, and so does each change.

   package Directory_Server renames
     Rose.Interfaces.Files.Writable_Directory.Server;
   package Writable renames Rose.Interfaces.Files.Writable_Directory;

   type Null_Directory is limited new
     Directory_Server.Writable_Directory_Interface with null record;

   procedure Refuse
     (Item      : in out Null_Directory'Class;
      Operation : String);
   --  Raise, naming Operation: the directory is empty, and can't change.

   overriding function Tree_Cap
     (Item : in out Null_Directory)
      return Rose.Capabilities.Capability
   is (Directory_Server.Cap_Of (Context, Item, Writable.Tree_Interface));

   overriding function Directory_Cap
     (Item : in out Null_Directory)
      return Rose.Capabilities.Capability
   is (Directory_Server.Cap_Of (Context, Item, Writable.Directory_Interface));

   overriding function Fixed_Directory_Cap
     (Item : in out Null_Directory)
      return Rose.Capabilities.Capability
   is (Directory_Server.Cap_Of
         (Context, Item, Writable.Fixed_Directory_Interface));

   overriding function Writable_Directory_Cap
     (Item : in out Null_Directory)
      return Rose.Capabilities.Capability
   is (Directory_Server.Cap_Of (Context, Item));

   overriding function Entry_Count
     (Item : in out Null_Directory)
      return Natural
   is (0);

   overriding function Find_Entry
     (Item : in out Null_Directory;
      Name : String)
      return Natural
   is (0);

   overriding function Entry_Name
     (Item  : in out Null_Directory;
      Index : Positive)
      return String;

   overriding function Entry_Kind_Of
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Entry_Kind;

   overriding function Get_Branch
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Tree_Object'Class;

   overriding function Get_Leaf
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Readable.Readable_Object'Class;

   overriding function Entry_File_Kind
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind;

   overriding function Entry_Size
     (Item  : in out Null_Directory;
      Index : Positive)
      return System.Storage_Elements.Storage_Count;

   overriding function Get_Ordinary_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.File.File_Object'Class;

   overriding function Get_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.Directory_Object'Class;

   overriding function Get_Special_Reader
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class;

   overriding function Get_Writable_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class;

   overriding function Get_Fixed_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Fixed_Directory
        .Fixed_Directory_Object'Class;

   overriding function Get_Special_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Stream.Stream_Object'Class;

   overriding function Get_Writable_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Writable.Writable_Directory_Object'Class;

   overriding function Create_File
     (Item : in out Null_Directory;
      Name : String)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class;

   overriding function Create_Directory
     (Item : in out Null_Directory;
      Name : String)
      return Writable.Writable_Directory_Object'Class;

   overriding procedure Rename
     (Item : in out Null_Directory;
      From : String;
      To   : String);

   overriding procedure Delete_File
     (Item : in out Null_Directory;
      Name : String);

   overriding procedure Delete_Directory
     (Item : in out Null_Directory;
      Name : String);

   overriding procedure Add_Special_File
     (Item : in out Null_Directory;
      Name : String;
      File : in out Rose.Interfaces.Streams.Stream.Stream_Object'Class);

   The_Directory : aliased Null_Directory;

   ----------------------
   -- Add_Special_File --
   ----------------------

   overriding procedure Add_Special_File
     (Item : in out Null_Directory;
      Name : String;
      File : in out Rose.Interfaces.Streams.Stream.Stream_Object'Class)
   is
      pragma Unreferenced (Name, File);
   begin
      Refuse (Item, "add special file");
   end Add_Special_File;

   ----------------------
   -- Create_Directory --
   ----------------------

   overriding function Create_Directory
     (Item : in out Null_Directory;
      Name : String)
      return Writable.Writable_Directory_Object'Class
   is
      pragma Unreferenced (Name);
   begin
      Refuse (Item, "create directory");
      return Rose.Interfaces.Files.Writable_Directory.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Create_Directory;

   -----------------
   -- Create_File --
   -----------------

   overriding function Create_File
     (Item : in out Null_Directory;
      Name : String)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class
   is
      pragma Unreferenced (Name);
   begin
      Refuse (Item, "create file");
      return Rose.Interfaces.Files.Writable_File.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Create_File;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Served         : out Rose.Capabilities.Capability;
      Null_Directory : out Rose.Capabilities.Capability)
   is
   begin
      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);
      Stream_Server.Attach (Context, The_Stream'Access);
      Directory_Server.Attach (Context, The_Directory'Access);
      Served := The_Stream.Stream_Cap;
      Null_Directory := The_Directory.Writable_Directory_Cap;
   end Create_Server;

   ----------------------
   -- Delete_Directory --
   ----------------------

   overriding procedure Delete_Directory
     (Item : in out Null_Directory;
      Name : String)
   is
      pragma Unreferenced (Name);
   begin
      Refuse (Item, "delete directory");
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   overriding procedure Delete_File
     (Item : in out Null_Directory;
      Name : String)
   is
      pragma Unreferenced (Name);
   begin
      Refuse (Item, "delete file");
   end Delete_File;

   ---------------------
   -- Entry_File_Kind --
   ---------------------

   overriding function Entry_File_Kind
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "entry file kind");
      return Rose.Interfaces.Files.Directory.Special_File;
   end Entry_File_Kind;

   -------------------
   -- Entry_Kind_Of --
   -------------------

   overriding function Entry_Kind_Of
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Entry_Kind
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "entry kind");
      return Rose.Interfaces.Common.Tree.Other;
   end Entry_Kind_Of;

   ----------------
   -- Entry_Name --
   ----------------

   overriding function Entry_Name
     (Item  : in out Null_Directory;
      Index : Positive)
      return String
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "entry name");
      return "";
   end Entry_Name;

   ----------------
   -- Entry_Size --
   ----------------

   overriding function Entry_Size
     (Item  : in out Null_Directory;
      Index : Positive)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "entry size");
      return 0;
   end Entry_Size;

   ----------------
   -- Get_Branch --
   ----------------

   overriding function Get_Branch
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Common.Tree.Tree_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get branch");
      return Rose.Interfaces.Common.Tree.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Branch;

   -------------------
   -- Get_Directory --
   -------------------

   overriding function Get_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Directory.Directory_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get directory");
      return Rose.Interfaces.Files.Directory.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Directory;

   -------------------------
   -- Get_Fixed_Directory --
   -------------------------

   overriding function Get_Fixed_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Fixed_Directory
        .Fixed_Directory_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get fixed directory");
      return Rose.Interfaces.Files.Fixed_Directory.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Fixed_Directory;

   --------------
   -- Get_Leaf --
   --------------

   overriding function Get_Leaf
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Readable.Readable_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get leaf");
      return Rose.Interfaces.Streams.Readable.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Leaf;

   -----------------------
   -- Get_Ordinary_File --
   -----------------------

   overriding function Get_Ordinary_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.File.File_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get ordinary file");
      return Rose.Interfaces.Files.File.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Ordinary_File;

   ----------------------
   -- Get_Special_File --
   ----------------------

   overriding function Get_Special_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Stream.Stream_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get special file");
      return Rose.Interfaces.Streams.Stream.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Special_File;

   ------------------------
   -- Get_Special_Reader --
   ------------------------

   overriding function Get_Special_Reader
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Streams.Reader.Reader_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get special reader");
      return Rose.Interfaces.Streams.Reader.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Special_Reader;

   ----------------------------
   -- Get_Writable_Directory --
   ----------------------------

   overriding function Get_Writable_Directory
     (Item  : in out Null_Directory;
      Index : Positive)
      return Writable.Writable_Directory_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get writable directory");
      return Rose.Interfaces.Files.Writable_Directory.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Writable_Directory;

   -----------------------
   -- Get_Writable_File --
   -----------------------

   overriding function Get_Writable_File
     (Item  : in out Null_Directory;
      Index : Positive)
      return Rose.Interfaces.Files.Writable_File.Writable_File_Object'Class
   is
      pragma Unreferenced (Index);
   begin
      Refuse (Item, "get writable file");
      return Rose.Interfaces.Files.Writable_File.Client.From_Cap
        (Rose.Capabilities.Null_Capability);
   end Get_Writable_File;

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

   ------------
   -- Refuse --
   ------------

   procedure Refuse
     (Item      : in out Null_Directory'Class;
      Operation : String)
   is
   begin
      if Item.Entry_Count = 0 then
         raise Constraint_Error
           with Operation & ": the null directory is empty, and can't"
             & " change";
      end if;
   end Refuse;

   ------------
   -- Rename --
   ------------

   overriding procedure Rename
     (Item : in out Null_Directory;
      From : String;
      To   : String)
   is
      pragma Unreferenced (From, To);
   begin
      Refuse (Item, "rename");
   end Rename;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

end Null_Stream_Server;
