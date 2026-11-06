with System.Storage_Elements;

with Ada_Text_IO;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.Releasable_Reader.Client;
with Rose.Interfaces.Transactional_Writable_File.Client.Conversions;
with Rose.Interfaces.Writable_Directory.Client.Conversions;
with Rose.Interfaces.Writable_File.Client;

with Petal.Read;

package body Petal.Session is

   use Rose.Interfaces.Writable_Directory.Client;
   use type Rose.Capabilities.Capability;

   package Version_Clients renames
     Rose.Interfaces.Transactional_Writable_File.Client;

   --  Petal's argument cap set: the caps it uses here.


   Startup_File_Name : constant String := ".petalrc";

   Max_Directories : constant := 16;
   Max_Name_Length : constant := 32;

   type Directory_Entry is
      record
         Name      : String (1 .. Max_Name_Length);
         Name_Last : Natural := 0;
         Directory : Writable_Directory_Client;
         Grant     : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         --  The read-only grant of Directory, once it has been made.
      end record;

   Directories     : array (1 .. Max_Directories) of Directory_Entry;
   Directory_Count : Natural := 0;

   Session_User : Rose.Interfaces.User.Client.User_Client;
   Have_User    : Boolean := False;

   Max_Found_Files : constant := 16;
   Found_Files     : Rose.Capabilities.Capability_Array (1 .. Max_Found_Files);
   Found_Count     : Natural := 0;
   --  The caps that Find_File has returned, for Release_Files.

   Max_Versions         : constant := 8;
   Max_File_Name_Length : constant := 64;
   --  As many as a directory makes at once, with its longest name.

   type Version_Entry is
      record
         Version   : Version_Clients.Transactional_Writable_File_Client;
         File      : Rose.Interfaces.Writable_File.Client
           .Writable_File_Client;
         Name      : String (1 .. Max_File_Name_Length);
         Name_Last : Natural := 0;
         Pending   : Boolean := False;
      end record;
   --  A version that New_Version made, and petal's client of its
   --  Writable_File, whose interface cap the command is given.

   Versions      : array (1 .. Max_Versions) of Version_Entry;
   Version_Count : Natural := 0;

   Max_Depth : constant := 16;

   type Level_Entry is
      record
         Name      : String (1 .. Max_File_Name_Length);
         Name_Last : Natural := 0;
         Directory : Writable_Directory_Client;
         Grant     : Rose.Interfaces.Directory.Client.Directory_Client;
         Has_Grant : Boolean := False;
      end record;
   --  A directory on the path of the current directory: its name in the
   --  directory before it, petal's client of it, and its read-only
   --  grant, once it has been made.

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

   procedure Close_Version (Item : in out Version_Entry);
   --  Release the version Item, which drops what wasn't committed, and
   --  delete petal's caps for it.

   procedure Current_Directory
     (Directory : out Writable_Directory_Client;
      Found     : out Boolean);
   --  The current directory, if there is one.

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Name      : String;
      Directory : Writable_Directory_Client)
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
         Item.Directory := Directory;
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
            use type Rose.Interfaces.Directory.File_Kind;
            Name   : constant String := Target_Name (Level);
            Parent : constant Writable_Directory_Client :=
              (if Level > Keep + 1 then Opened (Level - 1).Directory
               elsif Level > 1 then Levels (Level - 1).Directory
               else Directories (Root).Directory);
            Index  : constant Natural := Find_Entry (Parent, Name);
            Params : aliased Rose.Invocation.Invocation_Record;
            Failed : Boolean := False;
         begin
            if Index = 0 then
               Error ("no directory named '" & Name & "'");
               Failed := True;
            elsif Directory_Entry_Kind (Parent, Index)
              /= Rose.Interfaces.Directory.Directory
            then
               Error ("'" & Name & "' is not a directory");
               Failed := True;
            else

               --  The call is made here, not through
               --  Get_Writable_Directory, which opens a client of the
               --  result even when the directory refuses.

               Rose.System_Calls.Initialize_Send
                 (Params, Get_Get_Writable_Directory_Cap (Parent));
               Rose.System_Calls.Send_Word (Params, Index);
               Rose.System_Calls.Receive_Caps (Params, 1);
               Rose.System_Calls.Invoke_Capability (Params);

               if Params.Control.Flags (Rose.Invocation.Error)
                 or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
               then
                  Error ("cannot open '" & Name & "'");
                  Failed := True;
               end if;
            end if;

            if Failed then
               for Item of Opened (Keep + 1 .. Level - 1) loop
                  Close_Level (Item);
               end loop;
               return;
            end if;

            Open (Opened (Level).Directory, Params.Caps (0));
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
      if Item.Has_Grant then
         Rose.System_Calls.Delete_Cap
           (Rose.Interfaces.Directory.Client.Get_Interface_Cap (Item.Grant));
         Rose.Interfaces.Directory.Client.Close (Item.Grant);
         Item.Has_Grant := False;
      end if;
      Rose.System_Calls.Delete_Cap (Get_Interface_Cap (Item.Directory));
      Close (Item.Directory);
   end Close_Level;

   -------------------
   -- Close_Version --
   -------------------

   procedure Close_Version (Item : in out Version_Entry) is
   begin
      Version_Clients.Release (Item.Version);
      Rose.Interfaces.Writable_File.Client.Close (Item.File);
      Version_Clients.Close (Item.Version);
      Item.Pending := False;
   end Close_Version;

   ---------------------
   -- Commit_Versions --
   ---------------------

   procedure Commit_Versions (Failed : out Natural) is
   begin
      Failed := 0;
      for Index in 1 .. Version_Count loop
         Version_Clients.Commit (Versions (Index).Version);
         if Version_Clients.Has_Error then
            Failed := Index;
            return;
         end if;
         Close_Version (Versions (Index));
      end loop;
      Version_Count := 0;
   end Commit_Versions;

   -----------------------
   -- Current_Directory --
   -----------------------

   procedure Current_Directory
     (Directory : out Writable_Directory_Client;
      Found     : out Boolean)
   is
   begin
      if Depth > 0 then
         Directory := Levels (Depth).Directory;
         Found := True;
      elsif Current_Root > 0 then
         Directory := Directories (Current_Root).Directory;
         Found := True;
      else
         Found := False;
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
   begin
      if Depth = 0 then
         if Current_Root = 0 then
            Grant := Rose.Capabilities.Null_Capability;
            Found := False;
         else
            Find_Grant
              (Directories (Current_Root).Name
                 (1 .. Directories (Current_Root).Name_Last),
               Writable, Grant, Found);
         end if;
         return;
      end if;

      declare
         Item : Level_Entry renames Levels (Depth);
      begin
         if Writable then
            Grant := Get_Interface_Cap (Item.Directory);
         else
            if not Item.Has_Grant then
               Item.Grant :=
                 Rose.Interfaces.Writable_Directory.Client.Conversions
                   .To_Directory (Item.Directory);
               Item.Has_Grant := True;
            end if;
            Grant :=
              Rose.Interfaces.Directory.Client.Get_Interface_Cap (Item.Grant);
         end if;
         Found := Grant /= Rose.Capabilities.Null_Capability;
      end;
   end Current_Grant;

   ----------------------
   -- Discard_Versions --
   ----------------------

   procedure Discard_Versions is
   begin
      for Item of Versions (1 .. Version_Count) loop
         if Item.Pending then
            Close_Version (Item);
         end if;
      end loop;
      Version_Count := 0;
   end Discard_Versions;

   --------------------
   -- Find_Directory --
   --------------------

   procedure Find_Directory
     (Name      : String;
      Directory : out Writable_Directory_Client;
      Found     : out Boolean)
   is
   begin
      for Item of Directories (1 .. Directory_Count) loop
         if Item.Name (1 .. Item.Name_Last) = Name then
            Directory := Item.Directory;
            Found := True;
            return;
         end if;
      end loop;
      Found := False;
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
      use type Rose.Interfaces.Directory.File_Kind;
      Directory      : Writable_Directory_Client;
      Have_Directory : Boolean;
      Index          : Natural;
      Params         : aliased Rose.Invocation.Invocation_Record;
   begin
      File := Rose.Capabilities.Null_Capability;

      Current_Directory (Directory, Have_Directory);
      if not Have_Directory then
         Result := No_Directory;
         return;
      end if;

      Index := Find_Entry (Directory, Name);
      if Index = 0 then
         Result := No_Such_File;
         return;
      elsif Directory_Entry_Kind (Directory, Index)
        /= Rose.Interfaces.Directory.Ordinary_File
      then
         Result := Not_A_File;
         return;
      end if;

      --  Only the file's interface cap: Get_Ordinary_File would also open
      --  a client of it, with a cap for each of its subprograms, which
      --  petal does not need.

      Rose.System_Calls.Initialize_Send
        (Params,
         (if Writable then Get_Get_Writable_File_Cap (Directory)
          else Get_Get_Ordinary_File_Cap (Directory)));
      Rose.System_Calls.Send_Word (Params, Index);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         Result := No_Such_File;
         return;
      end if;

      File := Params.Caps (0);
      if Found_Count < Max_Found_Files then
         Found_Count := Found_Count + 1;
         Found_Files (Found_Count) := File;
      end if;
      Result := File_Found;
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
   begin
      for Item of Directories (1 .. Directory_Count) loop
         if Item.Name (1 .. Item.Name_Last) = Name then
            if Writable then
               Grant := Get_Interface_Cap (Item.Directory);
            else
               if Item.Grant = Rose.Capabilities.Null_Capability then
                  Item.Grant :=
                    Rose.Interfaces.Directory.Client.Get_Interface_Cap
                      (Rose.Interfaces.Writable_Directory.Client.Conversions
                       .To_Directory (Item.Directory));
               end if;
               Grant := Item.Grant;
            end if;
            Found := Grant /= Rose.Capabilities.Null_Capability;
            return;
         end if;
      end loop;
      Grant := Rose.Capabilities.Null_Capability;
      Found := False;
   end Find_Grant;

   --------------
   -- Get_User --
   --------------

   procedure Get_User
     (User  : out Rose.Interfaces.User.Client.User_Client;
      Found : out Boolean)
   is
   begin
      User := Session_User;
      Found := Have_User;
   end Get_User;

   -----------------
   -- New_Version --
   -----------------

   procedure New_Version
     (Name   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out Version_Lookup)
   is
      use type Rose.Interfaces.Directory.File_Kind;
      Directory      : Writable_Directory_Client;
      Have_Directory : Boolean;
      Index          : Natural;
      Params         : aliased Rose.Invocation.Invocation_Record;
   begin
      File := Rose.Capabilities.Null_Capability;

      Current_Directory (Directory, Have_Directory);
      if not Have_Directory then
         Result := No_Version_Directory;
         return;
      end if;

      Index := Find_Entry (Directory, Name);
      if Index /= 0
        and then Directory_Entry_Kind (Directory, Index)
          /= Rose.Interfaces.Directory.Ordinary_File
      then
         Result := Not_A_Version_File;
         return;
      elsif Version_Count = Max_Versions
        or else Name'Length > Max_File_Name_Length
      then
         Result := No_Version;
         return;
      end if;

      --  The calls that can fail are made here, not through the clients'
      --  functions, which open a client of the result even when the
      --  call fails. Name is copied first: Send_Text puts it in the
      --  invoke buffer, where it may already be.

      declare
         Saved_Name : constant String := Name;
         Target     : Rose.Interfaces.Writable_File.Client
           .Writable_File_Client;
         Version    : Rose.Capabilities.Capability;
      begin
         Rose.System_Calls.Initialize_Send
           (Params,
            (if Index = 0 then Get_Create_File_Cap (Directory)
             else Get_Get_Writable_File_Cap (Directory)));
         if Index = 0 then
            Rose.System_Calls.Send_Text (Params, Saved_Name);
         else
            Rose.System_Calls.Send_Word (Params, Index);
         end if;
         Rose.System_Calls.Receive_Caps (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);

         if Params.Control.Flags (Rose.Invocation.Error)
           or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
         then
            Result := No_Version;
            return;
         end if;

         --  The file's New_Version (ADR 0026).

         Rose.Interfaces.Writable_File.Client.Open
           (Target, Params.Caps (0));
         Rose.System_Calls.Initialize_Send
           (Params,
            Rose.Interfaces.Writable_File.Client.Get_New_Version_Cap
              (Target));
         Rose.System_Calls.Receive_Caps (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);

         if Params.Control.Flags (Rose.Invocation.Error)
           or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
         then
            Rose.Interfaces.Writable_File.Client.Close (Target);
            Result := No_Version;
            return;
         end if;

         Version := Params.Caps (0);
         Rose.Interfaces.Writable_File.Client.Close (Target);

         Version_Count := Version_Count + 1;
         declare
            Item : Version_Entry renames Versions (Version_Count);
         begin
            Version_Clients.Open (Item.Version, Version);
            Item.File := Version_Clients.Conversions.To_Writable_File
              (Item.Version);
            Item.Name (1 .. Saved_Name'Length) := Saved_Name;
            Item.Name_Last := Saved_Name'Length;
            Item.Pending := True;

            --  A version starts as a copy of the file. The command
            --  replaces the file (ADR 0012, section 4); appending (#338)
            --  would keep the copy.

            Rose.Interfaces.Writable_File.Client.Truncate (Item.File, 0);
            File := Rose.Interfaces.Writable_File.Client.Get_Interface_Cap
              (Item.File);
         end;
      end;
      Result := Version_Made;
   end New_Version;

   -------------------
   -- Release_Files --
   -------------------

   procedure Release_Files is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      for Cap of Found_Files (1 .. Found_Count) loop
         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end loop;
      Found_Count := 0;
   end Release_Files;

   ----------------------
   -- Run_Startup_File --
   ----------------------

   procedure Run_Startup_File is
      use System.Storage_Elements;
      Home   : Writable_Directory_Client;
      Found  : Boolean;
      Index  : Natural;
      Reader : Rose.Interfaces.Releasable_Reader.Client
        .Releasable_Reader_Client;
      Buffer : Storage_Array (1 .. 512);
      Last   : Storage_Count;
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
      Find_Directory (Home_Name, Home, Found);
      if not Found then
         return;
      end if;

      Index := Find_Entry (Home, Startup_File_Name);
      if Index = 0 then
         return;
      end if;

      --  A reader of its own, from the start of the file (ADR 0009).

      declare
         File : Rose.Interfaces.File.Client.File_Client :=
           Get_Ordinary_File (Home, Index);
      begin
         Reader := Rose.Interfaces.File.Client.Open_Reader (File);
         Rose.Interfaces.File.Client.Close (File);
      end;

      loop
         Rose.Interfaces.Releasable_Reader.Client.Read (Reader, Buffer, Last);
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

      --  Closing the reader releases it.

      Rose.Interfaces.Releasable_Reader.Client.Close (Reader);
   end Run_Startup_File;

   -----------
   -- Start --
   -----------

   procedure Start (User : Rose.Interfaces.User.Client.User_Client) is
      Home : constant Writable_Directory_Client :=
        Rose.Interfaces.User.Client.Home_Directory (User);
   begin
      Session_User := User;
      Have_User := True;
      if Get_Interface_Cap (Home) /= Rose.Capabilities.Null_Capability then
         Add_Directory (Home_Name, Home);
         Current_Root := Directory_Count;
      end if;
   end Start;

   ------------------
   -- Version_Name --
   ------------------

   function Version_Name (Index : Positive) return String is
   begin
      return Versions (Index).Name (1 .. Versions (Index).Name_Last);
   end Version_Name;

end Petal.Session;
