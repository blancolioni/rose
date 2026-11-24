with Ada.Streams;
with Ada.Unchecked_Deallocation;

with Ada_Text_IO;

with Rose.System_Calls;

with Rose.Interfaces.Files.Directory;
with Rose.Interfaces.Files.File.Client;
with Rose.Interfaces.Files.Fixed_Directory.Client;
with Rose.Interfaces.Streams.Stream;
with Rose.Interfaces.Streams.Stream.Client;
with Rose.Interfaces.Objects;
with Rose.Interfaces.Streams.Releasable_Reader.Client;
with Rose.Interfaces.Streams.Releasable_Writer.Client;
with Rose.Interfaces.Files.Transactional_Writable_File.Client;
with Rose.Interfaces.Files.Writable_Directory.Client;
with Rose.Interfaces.Files.Writable_File.Client;

with Petal.Read;

package body Petal.Session is

   use type Rose.Capabilities.Capability;
   use type Rose.Interfaces.Files.Directory.File_Kind;

   package Directory_Clients renames Rose.Interfaces.Files.Writable_Directory
     .Client;
   package Fixed_Clients renames Rose.Interfaces.Files.Fixed_Directory
     .Client;
   package File_Clients renames Rose.Interfaces.Files.Writable_File.Client;
   package Reader_Clients renames Rose.Interfaces.Streams.Releasable_Reader
     .Client;
   package Writer_Clients renames Rose.Interfaces.Streams.Releasable_Writer
     .Client;
   package Version_Clients renames
     Rose.Interfaces.Files.Transactional_Writable_File.Client;

   Invocation_Error : exception
     renames Rose.Interfaces.Objects.Invocation_Error;

   --  A directory that petal holds: a Writable_Directory, or, in system,
   --  a Fixed_Directory (ADR 0036), whose child directories are fixed
   --  too. Only the client that matches Writable is open.

   --  A directory's client is on the heap, because the path of the
   --  current directory is copied from level to level, and a client is
   --  limited. Closing it deletes its caps, its Directory cap among them.

   type Directory_Client is limited
      record
         Writable        : Boolean := True;
         Writable_Client : Directory_Clients.Writable_Directory_Client;
         Fixed_Client    : Fixed_Clients.Fixed_Directory_Client;
      end record;

   type Directory_Access is access Directory_Client;

   procedure Free is
     new Ada.Unchecked_Deallocation (Directory_Client, Directory_Access);

   function Open_Directory
     (Cap      : Rose.Capabilities.Capability;
      Writable : Boolean)
      return Directory_Access;
   --  A new client of the Writable_Directory interface Cap, or of the
   --  Fixed_Directory interface Cap if not Writable.

   procedure Close_Directory (Directory : in out Directory_Access);
   --  Close Directory's client, which deletes petal's caps for it.

   function Entry_Index
     (Directory : Directory_Access;
      Name      : String)
      return Natural;
   --  The index of the entry Name in Directory, or 0 if there is none.

   function Entry_Kind
     (Directory : Directory_Access;
      Index     : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind;

   function Child_Directory
     (Directory : Directory_Access;
      Index     : Positive)
      return Directory_Access;
   --  A new client of the directory at Index in Directory, at the same
   --  rights, or null if Directory doesn't give it.

   function Entry_File
     (Directory : Directory_Access;
      Index     : Positive;
      Writable  : Boolean)
      return Rose.Capabilities.Capability;
   --  The interface cap of the ordinary file at Index: of its
   --  Writable_File interface if Writable, or else of its File interface.

   function Entry_Grant
     (Directory : Directory_Access;
      Index     : Positive;
      Writable  : Boolean)
      return Rose.Capabilities.Capability;
   --  The interface cap of the directory at Index, as a command is
   --  granted it: its Writable_Directory interface if Writable, which
   --  only a writable Directory has, or else its Directory interface.

   function Grant_Of
     (Directory : Directory_Access;
      Writable  : Boolean)
      return Rose.Capabilities.Capability;
   --  Petal's own grant of Directory: the cap of its Writable_Directory
   --  interface if Writable, or else of its Directory interface. The
   --  client keeps it.

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

   function Root_Name (Root : Positive) return String
   is (Directories (Root).Name (1 .. Directories (Root).Name_Last));

   procedure Add_Entry
     (Name      : String;
      Directory : Directory_Access);
   --  Name Directory in the table, or report that the table is full and
   --  close it.

   function Find_Root (Name : String) return Natural;
   --  The index in the table of the directory called Name, or 0.

   function Find_Directory (Name : String) return Directory_Access;
   --  The directory called Name in the table, or null.

   Session_User : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   Max_Found_Files : constant := 16;
   Found_Files     : Rose.Capabilities.Capability_Array (1 .. Max_Found_Files);
   Found_Count     : Natural := 0;
   --  The caps that Find_File and Find_Grant have returned, for
   --  Release_Files.

   procedure Keep_Found
     (Cap   : Rose.Capabilities.Capability;
      Found : out Boolean);
   --  Keep Cap for Release_Files. Found is False, and Cap is deleted, if
   --  there is no room for it.

   Max_Readers  : constant := 4;
   Readers      : array (1 .. Max_Readers)
     of Reader_Clients.Releasable_Reader_Client;
   Reader_Count : Natural := 0;
   --  The readers that Open_Reader has opened, for Release_Files.

   Max_Versions         : constant := 8;
   Max_File_Name_Length : constant := 64;
   --  As many versions as a directory makes at once, and its longest
   --  name.

   Max_Path_Length : constant := 256;

   Versions      : array (1 .. Max_Versions)
     of Version_Clients.Transactional_Writable_File_Client;
   Version_Count : Natural := 0;
   --  The versions that New_Version made, whose interface caps the
   --  command is given.

   Redirected    : Boolean := False;
   Output_File   : Version_Clients.Transactional_Writable_File_Client;
   Output_Writer : Writer_Clients.Releasable_Writer_Client;
   Output_Name   : String (1 .. Max_Path_Length);
   Output_Last   : Natural := 0;
   --  The version that Redirect_Output made, which petal commits, the
   --  writer on it that the command is given, and the file's path.

   Package_State_Name : constant String := "package-state";
   --  The directory in home that holds each package's state (ADR 0040).

   System_Name : constant String := "system";
   --  The system directory (ADR 0035), which petal has as a
   --  Fixed_Directory.

   --  The special files that are in use: a reader for an argument, or the
   --  output of a redirection. A special file isn't a version, so its
   --  output is never committed.

   Max_Special     : constant := 4;
   Special_Streams : array (1 .. Max_Special)
     of Rose.Interfaces.Streams.Stream.Client.Stream_Client;
   Special_Count   : Natural := 0;

   Special_Output : Boolean := False;
   Output_Stream  : Rose.Interfaces.Streams.Stream.Client.Stream_Client;

   Max_Depth : constant := 16;

   type Level_Entry is
      record
         Name      : String (1 .. Max_File_Name_Length);
         Name_Last : Natural := 0;
         Directory : Directory_Access;
      end record;
   --  A directory on a path: its name in the directory before it, and
   --  petal's client of it.

   type Level_Array is array (1 .. Max_Depth) of Level_Entry;

   Current_Root : Natural := 0;
   --  The root of the current directory, an index in Directories, or 0
   --  if there is no current directory.

   Levels : Level_Array;
   Depth  : Natural := 0;
   --  The path from the root to the current directory.

   procedure Close_Level (Item : in out Level_Entry);
   --  Delete petal's caps for the directory Item, which is no longer on
   --  a path that petal keeps.

   function Current_Directory return Directory_Access;
   --  The current directory, or null if there is none.

   --  Paths (ADR 0044).

   type Name_Entry is
      record
         Name : String (1 .. Max_File_Name_Length);
         Last : Natural := 0;
      end record;

   type Name_Array is array (1 .. Max_Depth) of Name_Entry;

   Error_Text : String (1 .. 400);
   Error_Last : Natural := 0;

   procedure Set_Error (Message : String);
   --  Keep Message for Path_Error.

   procedure Parse_Path
     (Path  : String;
      Root  : out Natural;
      Names : out Name_Array;
      Count : out Natural;
      Valid : out Boolean);
   --  The named directory that Path starts from, and the names on the
   --  path from it, with "." and ".." taken out: a path from the current
   --  directory starts with the names on its path. Valid is False, with
   --  Path_Error saying why, if Path is not a path (ADR 0044, section 5).

   procedure Walk
     (Root   : Positive;
      Names  : Name_Array;
      Count  : Natural;
      Opened : in out Level_Array;
      Keep   : out Natural;
      Target : out Directory_Access;
      Valid  : out Boolean);
   --  Open the directories Names (1 .. Count) below Root. Keep is the
   --  number of them that the current directory's path has too, which
   --  aren't opened again: Opened (Keep + 1 .. Count) are the others, and
   --  Target is the last one, or Root itself. Valid is False, with
   --  Path_Error saying why and nothing left open, if one of them isn't
   --  a directory that petal can open.

   procedure In_Directory
     (Path   : String;
      Action : not null access procedure
        (Directory : Directory_Access;
         Name      : String);
      Valid  : out Boolean);
   --  Call Action with the directory that the last name on Path is in,
   --  and that name, then close the directories opened to reach it. If
   --  Path is a named directory itself, such as //home, Directory is
   --  that directory and Name is "". Valid is False, with Path_Error
   --  saying why, and Action isn't called, if Path doesn't lead there.

   procedure Make_Version
     (Directory : Directory_Access;
      Name      : String;
      Version   : out Version_Clients.Transactional_Writable_File_Client;
      Result    : out Version_Lookup);
   --  A version of the file Name in Directory, which is created first if
   --  there is no entry Name, for New_Version and Redirect_Output.
   --  Version is opened only if Result is Version_Made.

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
      Add_Entry (Name, Open_Directory (Directory, Writable => True));
   end Add_Directory;

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Name      : String;
      Directory : Directory_Access)
   is
      Item_Directory : Directory_Access := Directory;
   begin
      if Directory_Count = Max_Directories
        or else Name'Length > Max_Name_Length
      then
         Ada_Text_IO.Put_Line ("petal: cannot name directory " & Name);
         Close_Directory (Item_Directory);
         return;
      end if;

      Directory_Count := Directory_Count + 1;
      declare
         Item : Directory_Entry renames Directories (Directory_Count);
      begin
         Item.Name_Last := Name'Length;
         Item.Name (1 .. Item.Name_Last) := Name;
         Item.Directory := Directory;
      end;
   end Add_Entry;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory (Path : String) is
      Root   : Natural;
      Names  : Name_Array;
      Count  : Natural;
      Opened : Level_Array;
      Keep   : Natural;
      Target : Directory_Access;
      Valid  : Boolean;
   begin
      Parse_Path (Path, Root, Names, Count, Valid);
      if Valid then
         Walk (Root, Names, Count, Opened, Keep, Target, Valid);
      end if;

      if not Valid then
         Ada_Text_IO.Put_Line ("petal: cd: " & Path_Error);
         return;
      end if;

      --  The directories that the two paths share stay open.

      for Item of Levels (Keep + 1 .. Depth) loop
         Close_Level (Item);
      end loop;

      Levels (Keep + 1 .. Count) := Opened (Keep + 1 .. Count);
      Depth := Count;
      Current_Root := Root;
   end Change_Directory;

   ---------------------
   -- Child_Directory --
   ---------------------

   function Child_Directory
     (Directory : Directory_Access;
      Index     : Positive)
      return Directory_Access
   is
   begin
      if Directory.Writable then
         declare
            Child : Rose.Interfaces.Files.Writable_Directory
              .Writable_Directory_Object'Class :=
                Directory.Writable_Client.Get_Writable_Directory (Index);
         begin
            if Child.Writable_Directory_Cap
              = Rose.Capabilities.Null_Capability
            then
               return null;
            end if;
            return Open_Directory (Child.Writable_Directory_Cap, True);
         end;
      else
         declare
            Child : Rose.Interfaces.Files.Fixed_Directory
              .Fixed_Directory_Object'Class :=
                Directory.Fixed_Client.Get_Fixed_Directory (Index);
         begin
            if Child.Fixed_Directory_Cap = Rose.Capabilities.Null_Capability
            then
               return null;
            end if;
            return Open_Directory (Child.Fixed_Directory_Cap, False);
         end;
      end if;
   end Child_Directory;

   ---------------------
   -- Close_Directory --
   ---------------------

   procedure Close_Directory (Directory : in out Directory_Access) is
   begin
      if Directory /= null then
         if Directory.Writable then
            Directory_Clients.Close (Directory.Writable_Client);
         else
            Fixed_Clients.Close (Directory.Fixed_Client);
         end if;
         Free (Directory);
      end if;
   end Close_Directory;

   -----------------
   -- Close_Level --
   -----------------

   procedure Close_Level (Item : in out Level_Entry) is
   begin
      Close_Directory (Item.Directory);
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

      Add ("//" & Root_Name (Current_Root));
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
      if Directory /= null then
         Grant := Grant_Of (Directory, Writable);
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

      if Special_Output then
         Rose.Interfaces.Streams.Stream.Client.Close (Output_Stream);
         Special_Output := False;
         Redirected := False;
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

   ----------------
   -- Entry_File --
   ----------------

   function Entry_File
     (Directory : Directory_Access;
      Index     : Positive;
      Writable  : Boolean)
      return Rose.Capabilities.Capability
   is
   begin
      --  Only the file's interface cap: its client isn't closed, so the
      --  cap stays, for the command.

      if Writable then
         declare
            Item : Rose.Interfaces.Files.Writable_File
              .Writable_File_Object'Class :=
                (if Directory.Writable
                 then Directory.Writable_Client.Get_Writable_File (Index)
                 else Directory.Fixed_Client.Get_Writable_File (Index));
         begin
            return Item.Writable_File_Cap;
         end;
      else
         declare
            Item : Rose.Interfaces.Files.File.File_Object'Class :=
              (if Directory.Writable
               then Directory.Writable_Client.Get_Ordinary_File (Index)
               else Directory.Fixed_Client.Get_Ordinary_File (Index));
         begin
            return Item.File_Cap;
         end;
      end if;
   end Entry_File;

   -----------------
   -- Entry_Grant --
   -----------------

   function Entry_Grant
     (Directory : Directory_Access;
      Index     : Positive;
      Writable  : Boolean)
      return Rose.Capabilities.Capability
   is
   begin
      if Writable then
         if not Directory.Writable then
            return Rose.Capabilities.Null_Capability;
         end if;

         declare
            Item : Rose.Interfaces.Files.Writable_Directory
              .Writable_Directory_Object'Class :=
                Directory.Writable_Client.Get_Writable_Directory (Index);
         begin
            return Item.Writable_Directory_Cap;
         end;
      else
         declare
            Item : Rose.Interfaces.Files.Directory.Directory_Object'Class :=
              (if Directory.Writable
               then Directory.Writable_Client.Get_Directory (Index)
               else Directory.Fixed_Client.Get_Directory (Index));
         begin
            return Item.Directory_Cap;
         end;
      end if;
   end Entry_Grant;

   -----------------
   -- Entry_Index --
   -----------------

   function Entry_Index
     (Directory : Directory_Access;
      Name      : String)
      return Natural
   is
   begin
      if Directory.Writable then
         return Directory.Writable_Client.Find_Entry (Name);
      else
         return Directory.Fixed_Client.Find_Entry (Name);
      end if;
   end Entry_Index;

   ----------------
   -- Entry_Kind --
   ----------------

   function Entry_Kind
     (Directory : Directory_Access;
      Index     : Positive)
      return Rose.Interfaces.Files.Directory.File_Kind
   is
   begin
      if Directory.Writable then
         return Directory.Writable_Client.Entry_File_Kind (Index);
      else
         return Directory.Fixed_Client.Entry_File_Kind (Index);
      end if;
   end Entry_Kind;

   --------------------
   -- Find_Directory --
   --------------------

   function Find_Directory (Name : String) return Directory_Access is
      Root : constant Natural := Find_Root (Name);
   begin
      return (if Root = 0 then null else Directories (Root).Directory);
   end Find_Directory;

   ---------------
   -- Find_File --
   ---------------

   procedure Find_File
     (Path     : String;
      Writable : Boolean;
      File     : out Rose.Capabilities.Capability;
      Result   : out File_Lookup)
   is
      Kept : Boolean;

      procedure Look (Directory : Directory_Access; Name : String);

      ----------
      -- Look --
      ----------

      procedure Look (Directory : Directory_Access; Name : String) is
         Index : constant Natural :=
           (if Name = "" then 0 else Entry_Index (Directory, Name));
      begin
         if Name = "" then
            Result := Not_A_File;
         elsif Index = 0 then
            Result := No_Such_File;
         elsif Entry_Kind (Directory, Index)
           /= Rose.Interfaces.Files.Directory.Ordinary_File
         then
            Result := Not_A_File;
         else
            File := Entry_File (Directory, Index, Writable);
            Result :=
              (if File = Rose.Capabilities.Null_Capability then No_Such_File
               else File_Found);
         end if;
      exception
         when Invocation_Error =>
            File := Rose.Capabilities.Null_Capability;
            Result := No_Such_File;
      end Look;

      Valid : Boolean;
   begin
      File := Rose.Capabilities.Null_Capability;
      Result := No_Such_File;

      In_Directory (Path, Look'Access, Valid);
      if not Valid then
         Result := Bad_Path;
      elsif Result = File_Found then
         Keep_Found (File, Kept);
         if not Kept then
            File := Rose.Capabilities.Null_Capability;
            Result := No_Such_File;
         end if;
      end if;
   end Find_File;

   ----------------
   -- Find_Grant --
   ----------------

   procedure Find_Grant
     (Path     : String;
      Writable : Boolean;
      Grant    : out Rose.Capabilities.Capability;
      Found    : out Boolean)
   is
      Kept : Boolean := False;
      --  True if Grant is a cap of its own, for Release_Files.

      procedure Look (Directory : Directory_Access; Name : String);

      ----------
      -- Look --
      ----------

      procedure Look (Directory : Directory_Access; Name : String) is
         Index : Natural;
      begin
         if Writable and then not Directory.Writable then
            Set_Error ("'" & Path & "' can't be written");
            return;
         end if;

         --  A named directory itself: petal's own grant, which it keeps.

         if Name = "" then
            Grant := Grant_Of (Directory, Writable);
            return;
         end if;

         Index := Entry_Index (Directory, Name);
         if Index = 0 then
            Set_Error ("no directory named '" & Path & "'");
         elsif Entry_Kind (Directory, Index)
           /= Rose.Interfaces.Files.Directory.Directory
         then
            Set_Error ("'" & Path & "' is not a directory");
         else
            Grant := Entry_Grant (Directory, Index, Writable);
            Kept := Grant /= Rose.Capabilities.Null_Capability;
            if not Kept then
               Set_Error ("cannot open '" & Path & "'");
            end if;
         end if;
      exception
         when Invocation_Error =>
            Grant := Rose.Capabilities.Null_Capability;
            Set_Error ("cannot open '" & Path & "'");
      end Look;

      Valid : Boolean;
   begin
      Grant := Rose.Capabilities.Null_Capability;
      In_Directory (Path, Look'Access, Valid);
      Found := Valid and then Grant /= Rose.Capabilities.Null_Capability;

      if Found and then Kept then
         Keep_Found (Grant, Found);
         if not Found then
            Grant := Rose.Capabilities.Null_Capability;
            Set_Error ("too many directories for one command");
         end if;
      end if;
   end Find_Grant;

   ---------------
   -- Find_Root --
   ---------------

   function Find_Root (Name : String) return Natural is
   begin
      for I in 1 .. Directory_Count loop
         if Root_Name (I) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Root;

   --------------
   -- Grant_Of --
   --------------

   function Grant_Of
     (Directory : Directory_Access;
      Writable  : Boolean)
      return Rose.Capabilities.Capability
   is
   begin
      --  The read-only grant is the cap of the directory's Directory
      --  interface, which has only the lookup operations (ADR 0003,
      --  section 1). The client asks for it the first time, and keeps it.

      if Writable then
         return (if Directory.Writable
                 then Directory.Writable_Client.Writable_Directory_Cap
                 else Rose.Capabilities.Null_Capability);
      elsif Directory.Writable then
         return Directory.Writable_Client.Directory_Cap;
      else
         return Directory.Fixed_Client.Directory_Cap;
      end if;
   end Grant_Of;

   ------------------
   -- In_Directory --
   ------------------

   procedure In_Directory
     (Path   : String;
      Action : not null access procedure
        (Directory : Directory_Access;
         Name      : String);
      Valid  : out Boolean)
   is
      Root   : Natural;
      Names  : Name_Array;
      Count  : Natural;
      Opened : Level_Array;
      Keep   : Natural;
      Target : Directory_Access;
      Last   : Natural;
   begin
      Parse_Path (Path, Root, Names, Count, Valid);
      if not Valid then
         return;
      end if;

      Last := (if Count = 0 then 0 else Count - 1);
      Walk (Root, Names, Last, Opened, Keep, Target, Valid);
      if not Valid then
         return;
      end if;

      begin
         if Count = 0 then
            Action (Target, "");
         else
            Action (Target, Names (Count).Name (1 .. Names (Count).Last));
         end if;
      exception
         when others =>
            for Item of Opened (Keep + 1 .. Last) loop
               Close_Level (Item);
            end loop;
            raise;
      end;

      for Item of Opened (Keep + 1 .. Last) loop
         Close_Level (Item);
      end loop;
   end In_Directory;

   ----------------
   -- Keep_Found --
   ----------------

   procedure Keep_Found
     (Cap   : Rose.Capabilities.Capability;
      Found : out Boolean)
   is
   begin
      Found := Found_Count < Max_Found_Files;
      if Found then
         Found_Count := Found_Count + 1;
         Found_Files (Found_Count) := Cap;
      else
         Rose.System_Calls.Delete_Cap (Cap);
      end if;
   end Keep_Found;

   ------------------
   -- Make_Version --
   ------------------

   procedure Make_Version
     (Directory : Directory_Access;
      Name      : String;
      Version   : out Version_Clients.Transactional_Writable_File_Client;
      Result    : out Version_Lookup)
   is
      Index : Natural;
   begin
      if Name = "" then
         Result := Not_A_Version_File;
         return;
      end if;

      Index := Entry_Index (Directory, Name);
      if Index /= 0
        and then Entry_Kind (Directory, Index)
          /= Rose.Interfaces.Files.Directory.Ordinary_File
      then
         Result := Not_A_Version_File;
         return;
      end if;

      --  A file can't be created in system (ADR 0036).

      if Index = 0 and then not Directory.Writable then
         Result := No_Version;
         return;
      end if;

      declare
         Saved_Name : constant String := Name;
         --  Name is copied first: a call puts its text in the invoke
         --  buffer, where it may already be.

         function Target_File
           return Rose.Interfaces.Files.Writable_File
             .Writable_File_Object'Class;
         --  The file Name, made first if there is no entry Name (ADR 0027).

         -----------------
         -- Target_File --
         -----------------

         function Target_File
           return Rose.Interfaces.Files.Writable_File
             .Writable_File_Object'Class
         is
         begin
            if Index = 0 then
               return Directory.Writable_Client.Create_File (Saved_Name);
            elsif Directory.Writable then
               return Directory.Writable_Client.Get_Writable_File (Index);
            else
               return Directory.Fixed_Client.Get_Writable_File (Index);
            end if;
         end Target_File;

         Target : Rose.Interfaces.Files.Writable_File
           .Writable_File_Object'Class :=
           Target_File;
      begin
         if Target.Writable_File_Cap = Rose.Capabilities.Null_Capability then
            Result := No_Version;
            return;
         end if;

         --  The file's New_Version (ADR 0026). Petal only needs the
         --  version, so its client of the file is closed.

         declare
            New_Version : Rose.Interfaces.Files.Transactional_Writable_File
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
     (Path   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out Version_Lookup)
   is
      procedure Make (Directory : Directory_Access; Name : String);

      ----------
      -- Make --
      ----------

      procedure Make (Directory : Directory_Access; Name : String) is
      begin
         Make_Version
           (Directory, Name, Versions (Version_Count + 1), Result);
      end Make;

      Valid : Boolean;
   begin
      File := Rose.Capabilities.Null_Capability;
      Result := No_Version;

      if Version_Count = Max_Versions then
         return;
      end if;

      --  A version starts as a copy of the file. A command that replaces
      --  it opens the version with Rewrite, and one that adds to it with
      --  Append (ADR 0009, section 4).

      In_Directory (Path, Make'Access, Valid);
      if not Valid then
         Result := Bad_Path;
      elsif Result = Version_Made then
         Version_Count := Version_Count + 1;
         File := Versions (Version_Count).Transactional_Writable_File_Cap;
      end if;
   end New_Version;

   --------------------
   -- Open_Directory --
   --------------------

   function Open_Directory
     (Cap      : Rose.Capabilities.Capability;
      Writable : Boolean)
      return Directory_Access
   is
      Result : constant Directory_Access := new Directory_Client;
   begin
      Result.Writable := Writable;
      if Writable then
         Directory_Clients.Open (Result.Writable_Client, Cap);
      else
         Fixed_Clients.Open (Result.Fixed_Client, Cap);
      end if;
      return Result;
   end Open_Directory;

   -----------------
   -- Open_Reader --
   -----------------

   procedure Open_Reader
     (Path   : String;
      Reader : out Rose.Capabilities.Capability;
      Result : out File_Lookup)
   is
      procedure Look (Directory : Directory_Access; Name : String);

      ----------
      -- Look --
      ----------

      procedure Look (Directory : Directory_Access; Name : String) is
         Index : constant Natural :=
           (if Name = "" then 0 else Entry_Index (Directory, Name));
         Kind  : Rose.Interfaces.Files.Directory.File_Kind;
      begin
         if Name = "" then
            Result := Not_A_File;
            return;
         elsif Index = 0 then
            Result := No_Such_File;
            return;
         end if;

         Kind := Entry_Kind (Directory, Index);

         --  A special file gives the reader of its Stream, which isn't
         --  released afterwards, since petal didn't open it (ADR 0035,
         --  section 5).

         if Kind = Rose.Interfaces.Files.Directory.Special_File then
            if Special_Count = Max_Special then
               Result := No_Such_File;
               return;
            end if;

            declare
               Special : Rose.Interfaces.Streams.Stream
                 .Stream_Object'Class :=
                   (if Directory.Writable
                    then Directory.Writable_Client.Get_Special_File (Index)
                    else Directory.Fixed_Client.Get_Special_File (Index));
            begin
               if Special.Stream_Cap = Rose.Capabilities.Null_Capability then
                  Result := No_Such_File;
                  return;
               end if;
               Special_Count := Special_Count + 1;
               Rose.Interfaces.Streams.Stream.Client.Open
                 (Special_Streams (Special_Count), Special.Stream_Cap);
            end;
            Reader := Special_Streams (Special_Count).Get_Read_Cap;
            Result := File_Found;
            return;
         end if;

         if Kind /= Rose.Interfaces.Files.Directory.Ordinary_File then
            Result := Not_A_File;
            return;
         end if;

         --  Petal only needs the reader, so its client of the file is
         --  closed, which deletes its caps.

         declare
            File : Rose.Interfaces.Files.File.Client.File_Client;
         begin
            Rose.Interfaces.Files.File.Client.Open
              (File, Entry_File (Directory, Index, Writable => False));
            declare
               New_Reader : Rose.Interfaces.Streams.Releasable_Reader
                 .Releasable_Reader_Object'Class := File.Open_Reader;
            begin
               Reader_Count := Reader_Count + 1;
               Reader_Clients.Open
                 (Readers (Reader_Count), New_Reader.Releasable_Reader_Cap);
            end;
            Rose.Interfaces.Files.File.Client.Close (File);
         exception
            when Invocation_Error =>
               Rose.Interfaces.Files.File.Client.Close (File);
               Result := No_Such_File;
               return;
         end;

         Reader := Readers (Reader_Count).Get_Read_Cap;
         Result := File_Found;
      exception
         when Invocation_Error =>
            Reader := Rose.Capabilities.Null_Capability;
            Result := No_Such_File;
      end Look;

      Valid : Boolean;
   begin
      Reader := Rose.Capabilities.Null_Capability;
      Result := No_Such_File;

      if Reader_Count = Max_Readers then
         return;
      end if;

      In_Directory (Path, Look'Access, Valid);
      if not Valid then
         Result := Bad_Path;
      end if;
   end Open_Reader;

   -------------------
   -- Package_State --
   -------------------

   function Package_State
     (Package_Name : String)
      return Rose.Capabilities.Capability
   is
      Home      : constant Directory_Access := Find_Directory (Home_Name);
      Directory : String (1 .. Package_Name'Length);
   begin
      if Home = null or else not Home.Writable
        or else Package_Name'Length = 0
        or else Found_Count = Max_Found_Files
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      for I in Package_Name'Range loop
         declare
            Ch : constant Character := Package_Name (I);
         begin
            Directory (I - Package_Name'First + 1) :=
              (if Ch = '.' then '-'
               elsif Ch in 'A' .. 'Z'
               then Character'Val (Character'Pos (Ch) + 32)
               else Ch);
         end;
      end loop;

      --  Create_Directory gives the directory that is there already, so
      --  the first launch makes both, and later ones find them. Making a
      --  directory needs no commit.

      declare
         Parent : Rose.Interfaces.Files.Writable_Directory
           .Writable_Directory_Object'Class :=
             Home.Writable_Client.Create_Directory (Package_State_Name);
      begin
         declare
            Child : Rose.Interfaces.Files.Writable_Directory
              .Writable_Directory_Object'Class :=
                Parent.Create_Directory (Directory);
            Cap   : constant Rose.Capabilities.Capability :=
              Child.Writable_Directory_Cap;
         begin
            Rose.System_Calls.Delete_Cap (Parent.Writable_Directory_Cap);
            if Cap /= Rose.Capabilities.Null_Capability then
               Found_Count := Found_Count + 1;
               Found_Files (Found_Count) := Cap;
            end if;
            return Cap;
         end;
      end;
   exception
      when Invocation_Error =>
         return Rose.Capabilities.Null_Capability;
   end Package_State;

   ----------------
   -- Parse_Path --
   ----------------

   procedure Parse_Path
     (Path  : String;
      Root  : out Natural;
      Names : out Name_Array;
      Count : out Natural;
      Valid : out Boolean)
   is
      First : Positive := Path'First;
   begin
      Root := Current_Root;
      Count := 0;
      Valid := False;

      if Path = "" then
         Set_Error ("no path");
         return;
      end if;

      --  A ":" before the first "/" would be a scheme (ADR 0044, section
      --  5), and petal knows none.

      for Ch of Path loop
         exit when Ch = '/';
         if Ch = ':' then
            Set_Error ("'" & Path & "': petal knows no scheme");
            return;
         end if;
      end loop;

      if Path'Length >= 2
        and then Path (Path'First .. Path'First + 1) = "//"
      then

         --  //name: the named directory, which is the authority.

         declare
            Slash : Natural := Path'Last + 1;
         begin
            for I in Path'First + 2 .. Path'Last loop
               if Path (I) = '/' then
                  Slash := I;
                  exit;
               end if;
            end loop;

            Root := Find_Root (Path (Path'First + 2 .. Slash - 1));
            if Root = 0 then
               Set_Error
                 ("no directory named '" & Path (Path'First .. Slash - 1)
                  & "'");
               return;
            end if;
            First := Slash + 1;
         end;

      elsif Root = 0 then
         Set_Error ("there is no current directory");
         return;

      elsif Path (Path'First) = '/' then

         --  /path: the root of the current directory.

         First := Path'First + 1;

      else

         --  A relative path starts with the current directory's path.

         for Level in 1 .. Depth loop
            Names (Level) := (Levels (Level).Name, Levels (Level).Name_Last);
         end loop;
         Count := Depth;
      end if;

      --  The names, separated by "/". A "/" at the end is allowed, but
      --  an empty name between two is not (ADR 0044, section 5).

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
               if Name = "" then
                  Set_Error ("'" & Path & "': a name is missing");
                  return;
               elsif Name = "." then
                  null;
               elsif Name = ".." then
                  if Count = 0 then
                     Set_Error ("'" & Path & "': //" & Root_Name (Root)
                                & " has no parent");
                     return;
                  end if;
                  Count := Count - 1;
               elsif Name'Length > Max_File_Name_Length then
                  Set_Error ("name too long: " & Name);
                  return;
               elsif Count = Max_Depth then
                  Set_Error ("'" & Path & "': too deep");
                  return;
               else
                  Count := Count + 1;
                  Names (Count).Name (1 .. Name'Length) := Name;
                  Names (Count).Last := Name'Length;
               end if;
            end;

            First := Last + 2;
         end;
      end loop;

      Valid := True;
   end Parse_Path;

   ----------------
   -- Path_Error --
   ----------------

   function Path_Error return String is (Error_Text (1 .. Error_Last));

   ---------------------
   -- Redirect_Output --
   ---------------------

   procedure Redirect_Output
     (Path   : String;
      Append : Boolean;
      Output : out Rose.Capabilities.Capability;
      Result : out Version_Lookup)
   is
      procedure Open (Directory : Directory_Access; Name : String);

      ----------
      -- Open --
      ----------

      procedure Open (Directory : Directory_Access; Name : String) is
         Index : constant Natural :=
           (if Name = "" then 0 else Entry_Index (Directory, Name));
      begin

         --  A redirection to a special file writes to its Stream, and
         --  never replaces it (ADR 0035, section 5).

         if Index /= 0
           and then Entry_Kind (Directory, Index)
             = Rose.Interfaces.Files.Directory.Special_File
         then
            declare
               Special : Rose.Interfaces.Streams.Stream
                 .Stream_Object'Class :=
                   (if Directory.Writable
                    then Directory.Writable_Client.Get_Special_File (Index)
                    else Directory.Fixed_Client.Get_Special_File (Index));
            begin
               if Special.Stream_Cap = Rose.Capabilities.Null_Capability then
                  Result := No_Version;
                  return;
               end if;
               Rose.Interfaces.Streams.Stream.Client.Open
                 (Output_Stream, Special.Stream_Cap);
            end;
            Special_Output := True;
            Output := Output_Stream.Get_Write_Cap;
            Result := Version_Made;
            return;
         end if;

         Make_Version (Directory, Name, Output_File, Result);
      exception
         when Invocation_Error =>
            Result := No_Version;
      end Open;

      Valid : Boolean;
   begin
      Output := Rose.Capabilities.Null_Capability;
      Result := No_Version;

      if Redirected or else Path'Length > Max_Path_Length then
         return;
      end if;

      In_Directory (Path, Open'Access, Valid);
      if not Valid then
         Result := Bad_Path;
         return;
      elsif Result /= Version_Made then
         return;
      end if;

      Output_Last := Path'Length;
      Output_Name (1 .. Output_Last) := Path;
      Redirected := True;

      if Special_Output then
         return;
      end if;

      declare
         function New_Writer
           return Rose.Interfaces.Streams.Releasable_Writer
             .Releasable_Writer_Object'Class;
         --  Rewrite, which empties the version, or Append.

         ----------------
         -- New_Writer --
         ----------------

         function New_Writer
           return Rose.Interfaces.Streams.Releasable_Writer
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
            Writer : Rose.Interfaces.Streams.Releasable_Writer
              .Releasable_Writer_Object'Class := New_Writer;
         begin
            Writer_Clients.Open (Output_Writer, Writer.Releasable_Writer_Cap);
         end;
      exception
         when Invocation_Error =>
            Close_Version (Output_File);
            Redirected := False;
            Result := No_Version;
            return;
      end;

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

      --  Closing a special file's client deletes petal's caps for it,
      --  and leaves the stream as it is.

      for Special of Special_Streams (1 .. Special_Count) loop
         Rose.Interfaces.Streams.Stream.Client.Close (Special);
      end loop;
      Special_Count := 0;
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
      if Home = null or else not Home.Writable then
         return;
      end if;

      Index := Home.Writable_Client.Find_Entry (Startup_File_Name);
      if Index = 0 then
         return;
      end if;

      --  A reader of its own, from the start of the file (ADR 0009). The
      --  file's client is closed once the reader is open, and closing
      --  the reader releases it.

      declare
         File   : Rose.Interfaces.Files.File.File_Object'Class :=
           Home.Writable_Client.Get_Ordinary_File (Index);
         Reader : Rose.Interfaces.Streams.Releasable_Reader
           .Releasable_Reader_Object'Class := File.Open_Reader;
      begin
         Rose.Interfaces.Files.File.Client.Close
           (Rose.Interfaces.Files.File.Client.File_Client (File));

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

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error (Message : String) is
   begin
      Error_Last := Natural'Min (Message'Length, Error_Text'Length);
      Error_Text (1 .. Error_Last) :=
        Message (Message'First .. Message'First + Error_Last - 1);
   end Set_Error;

   ----------------
   -- Set_System --
   ----------------

   procedure Set_System (Directory : Rose.Capabilities.Capability) is
   begin
      if Directory /= Rose.Capabilities.Null_Capability then
         Add_Entry
           (System_Name, Open_Directory (Directory, Writable => False));
      end if;
   end Set_System;

   -----------
   -- Start --
   -----------

   procedure Start (User : in out Rose.Interfaces.Users.User
     .User_Object'Class) is
   begin
      Session_User := User.User_Cap;

      declare
         Home : Rose.Interfaces.Files.Writable_Directory
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

   ----------
   -- Walk --
   ----------

   procedure Walk
     (Root   : Positive;
      Names  : Name_Array;
      Count  : Natural;
      Opened : in out Level_Array;
      Keep   : out Natural;
      Target : out Directory_Access;
      Valid  : out Boolean)
   is
      function Name (Level : Positive) return String
      is (Names (Level).Name (1 .. Names (Level).Last));
   begin
      Keep := 0;
      Valid := False;

      if Root = Current_Root then
         while Keep < Depth and then Keep < Count
           and then Levels (Keep + 1).Name (1 .. Levels (Keep + 1).Name_Last)
                      = Name (Keep + 1)
         loop
            Keep := Keep + 1;
         end loop;
      end if;

      Target :=
        (if Keep > 0 then Levels (Keep).Directory
         else Directories (Root).Directory);

      for Level in Keep + 1 .. Count loop
         declare
            Index : Natural := 0;
            Child : Directory_Access;
         begin
            begin
               Index := Entry_Index (Target, Name (Level));
               if Index = 0 then
                  Set_Error ("no directory named '" & Name (Level) & "'");
               elsif Entry_Kind (Target, Index)
                 /= Rose.Interfaces.Files.Directory.Directory
               then
                  Set_Error ("'" & Name (Level) & "' is not a directory");
               else
                  Child := Child_Directory (Target, Index);
                  if Child = null then
                     Set_Error ("cannot open '" & Name (Level) & "'");
                  end if;
               end if;
            exception
               when Invocation_Error =>
                  Set_Error ("cannot open '" & Name (Level) & "'");
                  Child := null;
            end;

            if Child = null then
               for Item of Opened (Keep + 1 .. Level - 1) loop
                  Close_Level (Item);
               end loop;
               Target := null;
               return;
            end if;

            Opened (Level) :=
              (Names (Level).Name, Names (Level).Last, Child);
            Target := Child;
         end;
      end loop;

      Valid := True;
   end Walk;

end Petal.Session;
