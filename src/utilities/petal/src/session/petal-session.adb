with System.Storage_Elements;

with Ada_Text_IO;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Version.Client;
with Rose.Interfaces.File_Reader.Client;
with Rose.Interfaces.Writable_Directory.Client.Conversions;
with Rose.Interfaces.Writable_File.Client;

with Petal.Read;

package body Petal.Session is

   use Rose.Interfaces.Writable_Directory.Client;
   use type Rose.Capabilities.Capability;

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
         Version   : Rose.Interfaces.File_Version.Client.File_Version_Client;
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

   procedure Close_Version (Item : in out Version_Entry);
   --  Delete petal's caps for the version Item, which has been committed
   --  or discarded.

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

   -------------------
   -- Close_Version --
   -------------------

   procedure Close_Version (Item : in out Version_Entry) is
   begin
      Rose.Interfaces.Writable_File.Client.Close (Item.File);
      Rose.Interfaces.File_Version.Client.Close (Item.Version);
      Item.Pending := False;
   end Close_Version;

   ---------------------
   -- Commit_Versions --
   ---------------------

   procedure Commit_Versions (Failed : out Natural) is
   begin
      Failed := 0;
      for Index in 1 .. Version_Count loop
         Rose.Interfaces.File_Version.Client.Commit
           (Versions (Index).Version);
         if Rose.Interfaces.File_Version.Client.Has_Error then
            Failed := Index;
            return;
         end if;
         Close_Version (Versions (Index));
      end loop;
      Version_Count := 0;
   end Commit_Versions;

   ----------------------
   -- Discard_Versions --
   ----------------------

   procedure Discard_Versions is
   begin
      for Item of Versions (1 .. Version_Count) loop
         if Item.Pending then
            Rose.Interfaces.File_Version.Client.Discard (Item.Version);
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
      Home      : Writable_Directory_Client;
      Have_Home : Boolean;
      Index     : Natural;
      Params    : aliased Rose.Invocation.Invocation_Record;
   begin
      File := Rose.Capabilities.Null_Capability;

      Find_Directory (Home_Name, Home, Have_Home);
      if not Have_Home then
         Result := No_Directory;
         return;
      end if;

      Index := Find_Entry (Home, Name);
      if Index = 0 then
         Result := No_Such_File;
         return;
      elsif Directory_Entry_Kind (Home, Index)
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
         (if Writable then Get_Get_Writable_File_Cap (Home)
          else Get_Get_Ordinary_File_Cap (Home)));
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
      Home      : Writable_Directory_Client;
      Have_Home : Boolean;
      Index     : Natural;
      Params    : aliased Rose.Invocation.Invocation_Record;
   begin
      File := Rose.Capabilities.Null_Capability;

      Find_Directory (Home_Name, Home, Have_Home);
      if not Have_Home then
         Result := No_Version_Directory;
         return;
      end if;

      Index := Find_Entry (Home, Name);
      if Index /= 0
        and then Directory_Entry_Kind (Home, Index)
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

      --  The call is made here, not through New_File_Version, which
      --  opens a client of the result even when the directory refuses.

      Rose.System_Calls.Initialize_Send
        (Params, Get_New_File_Version_Cap (Home));
      Rose.System_Calls.Send_Text (Params, Name);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         Result := No_Version;
         return;
      end if;

      Version_Count := Version_Count + 1;
      declare
         Item : Version_Entry renames Versions (Version_Count);
      begin
         Rose.Interfaces.File_Version.Client.Open
           (Item.Version, Params.Caps (0));
         Item.File :=
           Rose.Interfaces.File_Version.Client.Writable (Item.Version);
         Item.Name (1 .. Name'Length) := Name;
         Item.Name_Last := Name'Length;
         Item.Pending := True;

         --  A version starts as a copy of the file. The command replaces
         --  the file (ADR 0012, section 4); appending (#338) would keep
         --  the copy.

         Rose.Interfaces.Writable_File.Client.Truncate (Item.File, 0);
         File := Rose.Interfaces.Writable_File.Client.Get_Interface_Cap
           (Item.File);
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
      Reader : Rose.Interfaces.File_Reader.Client.File_Reader_Client;
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
         Rose.Interfaces.File_Reader.Client.Read (Reader, Buffer, Last);
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

      Rose.Interfaces.File_Reader.Client.Close (Reader);
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
