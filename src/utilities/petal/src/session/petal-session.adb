with Ada.Streams;
with Ada.Unchecked_Deallocation;

with Ada_Text_IO;

with Rose.System_Calls;

with Rose.Interfaces.Directory;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Releasable_Writer.Client;
with Rose.Interfaces.Transactional_Writable_File.Client;
with Rose.Interfaces.Writable_Directory.Client;
with Rose.Interfaces.Writable_File.Client;

with Petal.Read;

package body Petal.Session is

   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Directory.File_Kind;

   package Directory_Clients renames Rose.Interfaces.Writable_Directory.Client;
   package File_Clients renames Rose.Interfaces.Writable_File.Client;
   package Reader_Clients renames Rose.Interfaces.Releasable_Reader.Client;
   package Writer_Clients renames Rose.Interfaces.Releasable_Writer.Client;
   package Version_Clients renames
     Rose.Interfaces.Transactional_Writable_File.Client;

   Invocation_Error : exception
     renames Rose.Interfaces.Objects.Invocation_Error;

   --  A directory's client is on the heap, because the path of the
   --  current directory is copied from level to level, and a client is
   --  limited. Closing it deletes its caps, its Directory cap among them.

   type Directory_Access is access Directory_Clients.Writable_Directory_Client;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Directory_Clients.Writable_Directory_Client, Directory_Access);

   function Open_Directory
     (Cap : Rose.Capabilities.Capability)
      return Directory_Access;
   --  A new client of the Writable_Directory interface Cap.

   Startup_File_Name : constant String := ".petalrc";

   Max_Directories : constant := 16;
   Max_Name_Length : constant := 32;

   type Directory_Entry is
      record
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Directory : Directory_Access;
      end record;

   Directories     : array (1 .. Max_Directories) of Directory_Entry;
   Directory_Count : Natural := 0;

   Session_User : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   Max_Found_Files : constant := 16;
   Found_Files     : Rose.Capabilities.Capability_Array (1 .. Max_Found_Files);
   Found_Count     : Natural := 0;
   --  The caps that Find_File has returned, for Release_Files.

   Max_Readers  : constant := 4;
   Readers      : array (1 .. Max_Readers)
     of Reader_Clients.Releasable_Reader_Client;
   Reader_Count : Natural := 0;
   --  The readers that Open_Reader has opened, for Release_Files.

   Max_Versions         : constant := 8;
   Max_File_Name_Length : constant := 64;
   --  As many versions as a directory makes at once, and its longest
   --  name.

   Versions      : array (1 .. Max_Versions)
     of Version_Clients.Transactional_Writable_File_Client;
   Version_Count : Natural := 0;
   --  The versions that New_Version made, whose interface caps the
   --  command is given.

   Redirected    : Boolean := False;
   Output_File   : Version_Clients.Transactional_Writable_File_Client;
   Output_Writer : Writer_Clients.Releasable_Writer_Client;
   Output_Name   : String (1 .. Max_File_Name_Length);
   Output_Last   : Natural := 0;
   --  The version that Redirect_Output made, which petal commits, the
   --  writer on it that the command is given, and the file's name.

   Max_Depth : constant := 16;

   type Level_Entry is
      record
         Name      : String (1 .. Max_File_Name_Length);
         Name_Last : Natural := 0;
         Directory : Directory_Access;
      end record;
   --  A directory on the path of the current directory: its name in the
   --  directory before it, and petal's client of it.

   type Level_Array is array (1 .. Max_Depth) of Level_Entry;

   Current_Root : Natural := 0;
   --  The root of the current directory, an index in Directories, or 0
   --  if there is no current directory.

   Levels : Level_Array;
   Depth  : Natural := 0;
   --  The path from the root to the current directory.

   procedure Close_Level (Item : in out Level_Entry);
   --  Delete petal's caps for the directory Item, which is no longer on
   --  the path of the current directory.

   function Current_Directory return Directory_Access;
   --  The current directory, or null if there is none.

   function Find_Directory (Name : String) return Directory_Access;
   --  The directory called Name in the table, or null.

   procedure Lookup_File
     (Name     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup);
   --  The interface cap of the ordinary file called Name in the current
   --  directory, as Find_File gives it, which the caller deletes.

   procedure Make_Version
     (Name    : String;
      Version : out Version_Clients.Transactional_Writable_File_Client;
      Result  : out Version_Lookup);
   --  A version of the file called Name in the current directory, which
   --  is created first if there is no entry Name, for New_Version and
   --  Redirect_Output. Version is opened only if Result is Version_Made.

   procedure Close_Version
     (Version : in out Version_Clients.Transactional_Writable_File_Client);
   --  Release Version, which drops what wasn't committed, and delete
   --  petal's caps for it. A version that is already gone is only closed.

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Name      : String;
      Directory : Rose.Capabilities.Capability)
   is
   begin
      if Directory_Count = Max_Directories
        or else Name'Length > Max_Name_Length
      then
         Ada_Text_IO.Put_Line ("petal: cannot name directory " & Name);
         return;
      end if;

      Directory_Count := Directory_Count + 1;
      declare
         Item : Directory_Entry renames Directories (Directory_Count);
      begin
         Item.Name_Last := Name'Length;
         Item.Name (1 .. Item.Name_Last) := Name;
         Item.Directory := Open_Directory (Directory);
      end;
   end Add_Directory;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory (Path : String) is

      type Name_Entry is
         record
            Name : String (1 .. Max_File_Name_Length);
            Last : Natural := 0;
         end record;

      Root         : Natural := Current_Root;
      Target       : array (1 .. Max_Depth) of Name_Entry;
      Target_Depth : Natural := 0;
      --  The path of the new current directory, from Root.

      Opened : Level_Array;
      --  The directories on that path that are not on the current one.

      Keep  : Natural := 0;
      First : Positive := Path'First;

      procedure Error (Message : String);

      function Target_Name (Level : Positive) return String
      is (Target (Level).Name (1 .. Target (Level).Last));

      -----------
      -- Error --
      -----------

      procedure Error (Message : String) is
      begin
         Ada_Text_IO.Put_Line ("petal: cd: " & Message);
      end Error;

   begin
      for Level in 1 .. Depth loop
         Target (Level) :=
           (Levels (Level).Name, Levels (Level).Name_Last);
      end loop;
      Target_Depth := Depth;

      --  [root] starts from the named directory root.

      if Path'Length > 0 and then Path (Path'First) = '[' then
         declare
            Close : Natural := 0;
         begin
            for I in Path'Range loop
               if Path (I) = ']' then
                  Close := I;
                  exit;
               end if;
            end loop;

            if Close = 0 then
               Error ("missing ']' in " & Path);
               return;
            end if;

            Root := 0;
            for I in 1 .. Directory_Count loop
               if Directories (I).Name (1 .. Directories (I).Name_Last)
                 = Path (Path'First + 1 .. Close - 1)
               then
                  Root := I;
                  exit;
               end if;
            end loop;

            if Root = 0 then
               Error ("no directory named " & Path (Path'First .. Close));
               return;
            end if;

            Target_Depth := 0;
            First := Close + 1;
         end;
      end if;

      if Root = 0 then
         Error ("there is no current directory");
         return;
      end if;

      --  The names, separated by "/".

      while First <= Path'Last loop
         declare
            Last : Natural := First - 1;
         begin
            while Last < Path'Last and then Path (Last + 1) /= '/' loop
               Last := Last + 1;
            end loop;

            declare
               Name : constant String := Path (First .. Last);
            begin
               if Name = "" or else Name = "." then
                  null;
               elsif Name = ".." then
                  if Target_Depth = 0 then
                     Error ("[" & Directories (Root).Name
                            (1 .. Directories (Root).Name_Last)
                            & "] has no parent");
                     return;
                  end if;
                  Target_Depth := Target_Depth - 1;
               elsif Name'Length > Max_File_Name_Length then
                  Error ("name too long: " & Name);
                  return;
               elsif Target_Depth = Max_Depth then
                  Error (Path & ": too deep");
                  return;
               else
                  Target_Depth := Target_Depth + 1;
                  Target (Target_Depth).Name (1 .. Name'Length) := Name;
                  Target (Target_Depth).Last := Name'Length;
               end if;
            end;

            First := Last + 2;
         end;
      end loop;

      --  The directories that the two paths share stay open.

      if Root = Current_Root then
         while Keep < Depth and then Keep < Target_Depth
           and then Levels (Keep + 1).Name (1 .. Levels (Keep + 1).Name_Last)
                      = Target_Name (Keep + 1)
         loop
            Keep := Keep + 1;
         end loop;
      end if;

      for Level in Keep + 1 .. Target_Depth loop
         declare
            Name   : constant String := Target_Name (Level);
            Parent : constant Directory_Access :=
              (if Level > Keep + 1 then Opened (Level - 1).Directory
               elsif Level > 1 then Levels (Level - 1).Directory
               else Directories (Root).Directory);
            Index  : Natural := 0;
            Child  : Rose.Capabilities.Capability :=
              Rose.Capabilities.Null_Capability;
         begin
            begin
               Index := Parent.Find_Entry (Name);
               if Index = 0 then
                  Error ("no directory named '" & Name & "'");
               elsif Parent.Directory_Entry_Kind (Index)
                 /= Rose.Interfaces.Directory.Directory
               then
                  Error ("'" & Name & "' is not a directory");
               else
                  declare
                     Directory : Rose.Interfaces.Writable_Directory
                       .Writable_Directory_Object'Class :=
                         Parent.Get_Writable_Directory (Index);
                  begin
                     Child := Directory.Writable_Directory_Cap;
                  end;
                  if Child = Rose.Capabilities.Null_Capability then
                     Error ("cannot open '" & Name & "'");
                  end if;
               end if;
            exception
               when Invocation_Error =>
                  Error ("cannot open '" & Name & "'");
                  Child := Rose.Capabilities.Null_Capability;
            end;

            if Child = Rose.Capabilities.Null_Capability then
               for Item of Opened (Keep + 1 .. Level - 1) loop
                  Close_Level (Item);
               end loop;
               return;
            end if;

            Opened (Level).Directory := Open_Directory (Child);
            Opened (Level).Name := Target (Level).Name;
            Opened (Level).Name_Last := Target (Level).Last;
         end;
      end loop;

      for Item of Levels (Keep + 1 .. Depth) loop
         Close_Level (Item);
      end loop;

      Levels (Keep + 1 .. Target_Depth) := Opened (Keep + 1 .. Target_Depth);
      Depth := Target_Depth;
      Current_Root := Root;
   end Change_Directory;

   -----------------
   -- Close_Level --
   -----------------

   procedure Close_Level (Item : in out Level_Entry) is
   begin
      if Item.Directory /= null then
         Directory_Clients.Close (Item.Directory.all);
         Free (Item.Directory);
      end if;
   end Close_Level;

   -------------------
   -- Close_Version --
   -------------------

   procedure Close_Version
     (Version : in out Version_Clients.Transactional_Writable_File_Client)
   is
   begin
      --  Closing a version releases it.

      Version_Clients.Close (Version);
   exception
      when Invocation_Error =>
         null;
   end Close_Version;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return Directory_Access is
   begin
      if Depth > 0 then
         return Levels (Depth).Directory;
      elsif Current_Root > 0 then
         return Directories (Current_Root).Directory;
      else
         return null;
      end if;
   end Current_Directory;

   ----------------------------
   -- Current_Directory_Name --
   ----------------------------

   function Current_Directory_Name return String is
      Result : String (1 .. Max_Name_Length + 2
                       + Max_Depth * (Max_File_Name_Length + 1));
      Last   : Natural := 0;

      procedure Add (Text : String);

      ---------
      -- Add --
      ---------

      procedure Add (Text : String) is
      begin
         Result (Last + 1 .. Last + Text'Length) := Text;
         Last := Last + Text'Length;
      end Add;

   begin
      if Current_Root = 0 then
         return "";
      end if;

      Add ("[" & Directories (Current_Root).Name
           (1 .. Directories (Current_Root).Name_Last) & "]");
      for Item of Levels (1 .. Depth) loop
         Add ("/" & Item.Name (1 .. Item.Name_Last));
      end loop;
      return Result (1 .. Last);
   end Current_Directory_Name;

   -------------------
   -- Current_Grant --
   -------------------

   procedure Current_Grant
     (Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean)
   is
      Directory : constant Directory_Access := Current_Directory;
   begin
      Grant := Rose.Capabilities.Null_Capability;

      --  The read-only grant is the cap of the directory's Directory
      --  interface, which has only the lookup operations (ADR 0003,
      --  section 1). The client asks for it the first time, and keeps it.

      if Directory /= null then
         Grant :=
           (if Writable then Directory.Writable_Directory_Cap
            else Directory.Directory_Cap);
      end if;
      Found := Grant /= Rose.Capabilities.Null_Capability;
   exception
      when Invocation_Error =>
         Grant := Rose.Capabilities.Null_Capability;
         Found := False;
   end Current_Grant;

   ---------------------
   -- End_Redirection --
   ---------------------

   procedure End_Redirection (Completed : Boolean) is
   begin
      if not Redirected then
         return;
      end if;

      --  Closing the writer releases it.

      begin
         Writer_Clients.Close (Output_Writer);
      exception
         when Invocation_Error =>
            null;
      end;

      if Completed then
         begin
            Output_File.Commit;
         exception
            when Invocation_Error =>
               Ada_Text_IO.Put_Line
                 ("petal: cannot write '"
                  & Output_Name (1 .. Output_Last) & "'");
         end;
      end if;

      Close_Version (Output_File);
      Redirected := False;
   end End_Redirection;

   --------------------
   -- Find_Directory --
   --------------------

   function Find_Directory (Name : String) return Directory_Access is
   begin
      for Item of Directories (1 .. Directory_Count) loop
         if Item.Name (1 .. Item.Name_Last) = Name then
            return Item.Directory;
         end if;
      end loop;
      return null;
   end Find_Directory;

   ---------------
   -- Find_File --
   ---------------

   procedure Find_File
     (Name     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup)
   is
   begin
      Lookup_File (Name, Writable, File, Result);
      if Result = File_Found and then Found_Count < Max_Found_Files then
         Found_Count := Found_Count + 1;
         Found_Files (Found_Count) := File;
      end if;
   end Find_File;

   ----------------
   -- Find_Grant --
   ----------------

   procedure Find_Grant
     (Name     : String;
      Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean)
   is
      Directory : constant Directory_Access := Find_Directory (Name);
   begin
      Grant := Rose.Capabilities.Null_Capability;
      if Directory /= null then
         Grant :=
           (if Writable then Directory.Writable_Directory_Cap
            else Directory.Directory_Cap);
      end if;
      Found := Grant /= Rose.Capabilities.Null_Capability;
   exception
      when Invocation_Error =>
         Grant := Rose.Capabilities.Null_Capability;
         Found := False;
   end Find_Grant;

   -----------------
   -- Lookup_File --
   -----------------

   procedure Lookup_File
     (Name     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup)
   is
      Directory : constant Directory_Access := Current_Directory;
      Index     : Natural;
   begin
      File := Rose.Capabilities.Null_Capability;

      if Directory = null then
         Result := No_Directory;
         return;
      end if;

      Index := Directory.Find_Entry (Name);
      if Index = 0 then
         Result := No_Such_File;
         return;
      elsif Directory.Directory_Entry_Kind (Index)
        /= Rose.Interfaces.Directory.Ordinary_File
      then
         Result := Not_A_File;
         return;
      end if;

      --  Only the file's interface cap: its client isn't closed, so the
      --  cap stays, for the command.

      if Writable then
         declare
            Item : Rose.Interfaces.Writable_File.Writable_File_Object'Class :=
              Directory.Get_Writable_File (Index);
         begin
            File := Item.Writable_File_Cap;
         end;
      else
         declare
            Item : Rose.Interfaces.File.File_Object'Class :=
              Directory.Get_Ordinary_File (Index);
         begin
            File := Item.File_Cap;
         end;
      end if;

      Result :=
        (if File = Rose.Capabilities.Null_Capability then No_Such_File
         else File_Found);
   exception
      when Invocation_Error =>
         File := Rose.Capabilities.Null_Capability;
         Result := No_Such_File;
   end Lookup_File;

   ------------------
   -- Make_Version --
   ------------------

   procedure Make_Version
     (Name    : String;
      Version : out Version_Clients.Transactional_Writable_File_Client;
      Result  : out Version_Lookup)
   is
      Directory : constant Directory_Access := Current_Directory;
      Index     : Natural;
   begin
      if Directory = null then
         Result := No_Version_Directory;
         return;
      end if;

      Index := Directory.Find_Entry (Name);
      if Index /= 0
        and then Directory.Directory_Entry_Kind (Index)
          /= Rose.Interfaces.Directory.Ordinary_File
      then
         Result := Not_A_Version_File;
         return;
      end if;

      declare
         Saved_Name : constant String := Name;
         --  Name is copied first: a call puts its text in the invoke
         --  buffer, where it may already be.

         function Target_File
           return Rose.Interfaces.Writable_File.Writable_File_Object'Class;
         --  The file Name, made first if there is no entry Name (ADR 0027).

         -----------------
         -- Target_File --
         -----------------

         function Target_File
           return Rose.Interfaces.Writable_File.Writable_File_Object'Class
         is
         begin
            if Index = 0 then
               return Directory.Create_File (Saved_Name);
            else
               return Directory.Get_Writable_File (Index);
            end if;
         end Target_File;

         Target : Rose.Interfaces.Writable_File.Writable_File_Object'Class :=
           Target_File;
      begin
         if Target.Writable_File_Cap = Rose.Capabilities.Null_Capability then
            Result := No_Version;
            return;
         end if;

         --  The file's New_Version (ADR 0026). Petal only needs the
         --  version, so its client of the file is closed.

         declare
            New_Version : Rose.Interfaces.Transactional_Writable_File
              .Transactional_Writable_File_Object'Class :=
                Target.New_Version;
         begin
            Version_Clients.Open
              (Version, New_Version.Transactional_Writable_File_Cap);
         end;
         File_Clients.Close (File_Clients.Writable_File_Client (Target));
      end;
      Result := Version_Made;
   exception
      when Invocation_Error =>
         Result := No_Version;
   end Make_Version;

   -----------------
   -- New_Version --
   -----------------

   procedure New_Version
     (Name   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out Version_Lookup)
   is
   begin
      File := Rose.Capabilities.Null_Capability;

      if Version_Count = Max_Versions then
         Result := No_Version;
         return;
      end if;

      --  A version starts as a copy of the file. A command that replaces
      --  it opens the version with Rewrite, and one that adds to it with
      --  Append (ADR 0009, section 4).

      Make_Version (Name, Versions (Version_Count + 1), Result);
      if Result = Version_Made then
         Version_Count := Version_Count + 1;
         File := Versions (Version_Count).Transactional_Writable_File_Cap;
      end if;
   end New_Version;

   --------------------
   -- Open_Directory --
   --------------------

   function Open_Directory
     (Cap : Rose.Capabilities.Capability)
      return Directory_Access
   is
      Result : constant Directory_Access :=
        new Directory_Clients.Writable_Directory_Client;
   begin
      Directory_Clients.Open (Result.all, Cap);
      return Result;
   end Open_Directory;

   -----------------
   -- Open_Reader --
   -----------------

   procedure Open_Reader
     (Name   : String;
      Reader : out Rose.Capabilities.Capability;
      Result : out File_Lookup)
   is
      File_Cap : Rose.Capabilities.Capability;
   begin
      Reader := Rose.Capabilities.Null_Capability;

      if Reader_Count = Max_Readers then
         Result := No_Such_File;
         return;
      end if;

      Lookup_File (Name, False, File_Cap, Result);
      if Result /= File_Found then
         return;
      end if;

      --  Petal only needs the reader, so its client of the file is
      --  closed, which deletes its caps.

      declare
         File : Rose.Interfaces.File.Client.File_Client;
      begin
         Rose.Interfaces.File.Client.Open (File, File_Cap);
         declare
            New_Reader : Rose.Interfaces.Releasable_Reader
              .Releasable_Reader_Object'Class := File.Open_Reader;
         begin
            Reader_Count := Reader_Count + 1;
            Reader_Clients.Open
              (Readers (Reader_Count), New_Reader.Releasable_Reader_Cap);
         end;
         Rose.Interfaces.File.Client.Close (File);
      exception
         when Invocation_Error =>
            Rose.Interfaces.File.Client.Close (File);
            Result := No_Such_File;
            return;
      end;

      Reader := Readers (Reader_Count).Get_Read_Cap;
   end Open_Reader;

   ---------------------
   -- Redirect_Output --
   ---------------------

   procedure Redirect_Output
     (Name   : String;
      Append : Boolean;
      Output : out Rose.Capabilities.Capability;
      Result : out Version_Lookup)
   is
   begin
      Output := Rose.Capabilities.Null_Capability;

      if Redirected or else Name'Length > Max_File_Name_Length then
         Result := No_Version;
         return;
      end if;

      Make_Version (Name, Output_File, Result);
      if Result /= Version_Made then
         return;
      end if;

      declare
         function New_Writer
           return Rose.Interfaces.Releasable_Writer
             .Releasable_Writer_Object'Class;
         --  Rewrite, which empties the version, or Append.

         ----------------
         -- New_Writer --
         ----------------

         function New_Writer
           return Rose.Interfaces.Releasable_Writer
             .Releasable_Writer_Object'Class
         is
         begin
            if Append then
               return Output_File.Append;
            else
               return Output_File.Rewrite;
            end if;
         end New_Writer;

      begin
         declare
            Writer : Rose.Interfaces.Releasable_Writer
              .Releasable_Writer_Object'Class := New_Writer;
         begin
            Writer_Clients.Open (Output_Writer, Writer.Releasable_Writer_Cap);
         end;
      exception
         when Invocation_Error =>
            Close_Version (Output_File);
            Result := No_Version;
            return;
      end;

      Output_Last := Name'Length;
      Output_Name (1 .. Output_Last) := Name;
      Redirected := True;
      Output := Output_Writer.Get_Write_Cap;
   end Redirect_Output;

   -------------------
   -- Release_Files --
   -------------------

   procedure Release_Files is
   begin
      for Cap of Found_Files (1 .. Found_Count) loop
         Rose.System_Calls.Delete_Cap (Cap);
      end loop;
      Found_Count := 0;

      --  Closing a reader releases it.

      for Reader of Readers (1 .. Reader_Count) loop
         begin
            Reader_Clients.Close (Reader);
         exception
            when Invocation_Error =>
               null;
         end;
      end loop;
      Reader_Count := 0;
   end Release_Files;

   ----------------------
   -- Release_Versions --
   ----------------------

   --  Releasing a version drops what wasn't committed, and ends the
   --  readers and writers opened from it.

   procedure Release_Versions is
   begin
      for Version of Versions (1 .. Version_Count) loop
         Close_Version (Version);
      end loop;
      Version_Count := 0;
   end Release_Versions;

   ----------------------
   -- Run_Startup_File --
   ----------------------

   procedure Run_Startup_File is
      use Ada.Streams;
      Home   : constant Directory_Access := Find_Directory (Home_Name);
      Index  : Natural;
      Buffer : Stream_Element_Array (1 .. 512);
      Last   : Stream_Element_Offset;
      Line   : String (1 .. 2000);
      Length : Natural := 0;
      Long   : Boolean := False;
      --  True while skipping the rest of a line that is too long.

      procedure End_Line;
      --  Run the line read so far, and start a new one.

      --------------
      -- End_Line --
      --------------

      procedure End_Line is
      begin
         if Long then
            Ada_Text_IO.Put_Line
              ("petal: " & Startup_File_Name & ": line too long");
         elsif Length > 0 then
            Petal.Read.Execute (Line (1 .. Length));
         end if;
         Length := 0;
         Long := False;
      end End_Line;

   begin
      if Home = null then
         return;
      end if;

      Index := Home.Find_Entry (Startup_File_Name);
      if Index = 0 then
         return;
      end if;

      --  A reader of its own, from the start of the file (ADR 0009). The
      --  file's client is closed once the reader is open, and closing
      --  the reader releases it.

      declare
         File   : Rose.Interfaces.File.File_Object'Class :=
           Home.Get_Ordinary_File (Index);
         Reader : Rose.Interfaces.Releasable_Reader
           .Releasable_Reader_Object'Class := File.Open_Reader;
      begin
         Rose.Interfaces.File.Client.Close
           (Rose.Interfaces.File.Client.File_Client (File));

         loop
            Reader.Read (Buffer, Last);
            exit when Last < Buffer'First;

            for Element of Buffer (Buffer'First .. Last) loop
               if Element = 10 then
                  End_Line;
               elsif Length = Line'Length then
                  Long := True;
               else
                  Length := Length + 1;
                  Line (Length) := Character'Val (Element);
               end if;
            end loop;
         end loop;

         --  A last line need not end with a line feed.

         End_Line;

         Reader_Clients.Close
           (Reader_Clients.Releasable_Reader_Client (Reader));
      end;
   exception
      when Invocation_Error =>
         Ada_Text_IO.Put_Line
           ("petal: cannot read " & Startup_File_Name);
   end Run_Startup_File;

   -----------
   -- Start --
   -----------

   procedure Start (User : in out Rose.Interfaces.User.User_Object'Class) is
   begin
      Session_User := User.User_Cap;

      declare
         Home : Rose.Interfaces.Writable_Directory
           .Writable_Directory_Object'Class := User.Home_Directory;
      begin
         if Home.Writable_Directory_Cap /= Rose.Capabilities.Null_Capability
         then
            Add_Directory (Home_Name, Home.Writable_Directory_Cap);
            Current_Root := Directory_Count;
         end if;
      end;
   exception
      when Invocation_Error =>
         Ada_Text_IO.Put_Line ("petal: no home directory");
   end Start;

   --------------
   -- User_Cap --
   --------------

   function User_Cap return Rose.Capabilities.Capability is
   begin
      return Session_User;
   end User_Cap;

end Petal.Session;
