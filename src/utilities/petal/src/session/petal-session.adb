with System.Storage_Elements;

with Ada_Text_IO;

with Rose.Capabilities.Standard;
with Rose.Invocation;
with Rose.System_Calls.Client;
with Rose.Words;

with Rose.Interfaces.Cap_Set;
with Rose.Interfaces.Directory.Client;
with Rose.Interfaces.File.Client;
with Rose.Interfaces.File_Reader.Client;
with Rose.Interfaces.Writable_Directory.Client.Conversions;

with Petal.Read;

package body Petal.Session is

   use Rose.Interfaces.Writable_Directory.Client;
   use type Rose.Capabilities.Capability;

   --  Petal's argument cap set: the caps it uses here.

   Home_Index : constant := 4;
   --  The current_directory slot, which holds the user's home directory,
   --  a Writable_Directory.
   User_Index : constant := 12;
   --  The user's User interface (interfaces/user.idl).

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
     (Name   : String;
      File   : out Rose.Capabilities.Capability;
      Result : out File_Lookup)
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
        (Params, Get_Get_Ordinary_File_Cap (Home));
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
     (Name  : String;
      Grant : out Rose.Capabilities.Capability;
      Found : out Boolean)
   is
   begin
      for Item of Directories (1 .. Directory_Count) loop
         if Item.Name (1 .. Item.Name_Last) = Name then
            if Item.Grant = Rose.Capabilities.Null_Capability then
               Item.Grant :=
                 Rose.Interfaces.Directory.Client.Get_Interface_Cap
                   (Rose.Interfaces.Writable_Directory.Client.Conversions
                    .To_Directory (Item.Directory));
            end if;
            Grant := Item.Grant;
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

   procedure Start is
      Params  : aliased Rose.Invocation.Invocation_Record;
      Get_Cap : Rose.Capabilities.Capability;

      function Argument_Cap
        (Index : Rose.Words.Word)
         return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability (Get_Cap, (1 => Index)));

   begin
      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Standard.Argument_Cap_Set);
      Rose.System_Calls.Receive_Caps
        (Params, Rose.Interfaces.Cap_Set.Subprogram_Count);
      Rose.System_Calls.Invoke_Capability (Params);
      Get_Cap := Params.Caps (Rose.Interfaces.Cap_Set.Get_Cap_Index);

      --  A generated client invokes whatever cap it is opened with, and
      --  invoking a null cap fails with Invalid_Capability (#239), so
      --  missing caps are not opened.

      declare
         Home_Cap : constant Rose.Capabilities.Capability :=
           Argument_Cap (Home_Index);
         User_Cap : constant Rose.Capabilities.Capability :=
           Argument_Cap (User_Index);
         Home     : Writable_Directory_Client;
      begin
         if Home_Cap /= Rose.Capabilities.Null_Capability then
            Open (Home, Home_Cap);
            Add_Directory (Home_Name, Home);
         end if;

         if User_Cap /= Rose.Capabilities.Null_Capability then
            Rose.Interfaces.User.Client.Open (Session_User, User_Cap);
            Have_User := True;
         end if;
      end;
   end Start;

end Petal.Session;
