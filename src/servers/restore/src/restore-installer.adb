with System.Storage_Elements;

with Rose.Limits;
with Rose.Words;

with Rose.Console_IO;

with Rose.Invocation;
with Rose.System_Calls;

with Rose.Interfaces.Files.Directory.Client;
with Rose.Interfaces.Processes.Exec.Client;
with Rose.Interfaces.Files.File.Client;
with Rose.Interfaces.Processes.Launch.Client;
with Rose.Interfaces.Commands.Map.Client;
with Rose.Interfaces.Streams.Releasable_Reader.Client.Conversions;
with Rose.Interfaces.Processes.Process_Control;
with Rose.Interfaces.Streams.Reader.Client;
with Rose.Interfaces.Screens.Terminal.Client;

with Restore.Checkpoints;
with Restore.Command_Line;
with Restore.Replacing;

package body Restore.Installer is

   function Skip_Test_Program
     (Name  : String;
      Flags : Install_Flag_Array)
      return Boolean;
   --  True if Name is a program only for the tests, and this isn't a
   --  test install (#1009), in which case it says so on the console.

   type Array_Of_Capabilities is
     array (Positive range <>) of Rose.Capabilities.Capability;

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities);

   procedure Open_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities);
   --  The caps of the first Interface_Caps'Length subprograms of the
   --  interface whose cap is Cap, in the order of their index constants,
   --  asked for a page at a time (idle#101). Get_Interface is for a cap
   --  that isn't an interface's: it takes every cap of the reply.

   function Get_Public_Interface_From_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   function Get_Ancestor_Interface
     (Interface_Cap : Rose.Capabilities.Capability;
      Ancestor      : Positive)
      return Rose.Capabilities.Capability;
   --  The interface cap of an ancestor of the interface of Interface_Cap,
   --  numbered as the generated server numbers them.

   Reader_Ancestor : constant := 1;
   Writer_Ancestor : constant := 2;
   --  The ancestors of Terminal (interfaces/terminal.idl).

   Clock_Ancestor         : constant := 1;
   --  The ancestor of Clock_Admin (interfaces/clock_admin.idl).

   Keyboard_Interface_Cap : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Screen_Surface_Cap     : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Input_Stream_Cap       : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Terminal_Cap           : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   Console_Writer_Cap     : Rose.Capabilities.Capability :=
                              Rose.Capabilities.Null_Capability;
   --  The Terminal's Writer, for init and for Requires_Console.

   Invoke_Buffer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;

   List_Path     : constant String := "/rose/install/list";
   Max_List_Size : constant := 16 * 1024;
   Max_Line      : constant := 200;
   Max_Name      : constant := 100;

   List_Text : System.Storage_Elements.Storage_Array (1 .. Max_List_Size);
   --  The install list (#1100), read whole before anything is installed,
   --  so that its reader is released first.

   List_Transfer : System.Storage_Elements.Storage_Array
     (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;
   --  The invocation buffer while the list is read, which each part of
   --  it arrives in, as in Restore.Boot_Sets, rather than whatever
   --  buffer Rose.Console_IO left.

   procedure Read_List
     (Install_Directory : Rose.Interfaces.Files.Directory.Client
        .Directory_Client;
      Last              : out System.Storage_Elements.Storage_Count;
      Success           : out Boolean);
   --  Read the list in Install_Directory into List_Text (1 .. Last).
   --  Success is False, and the console says why, if there is no list,
   --  or it is too big, or it can't be read.

   procedure Find_Flag
     (Word  : String;
      Flag  : out Install_Flag;
      Found : out Boolean);
   --  The flag that Word names in the list: its name in lower case.
   --  Is_Driver has none, because the category sets it.

   function Is_Space (Ch : Character) return Boolean
   is (Ch = ' '
       or else Ch = Character'Val (9)
       or else Ch = Character'Val (13));
   --  Spaces and tabs separate the fields of a line of the list, and a
   --  carriage return before a line feed is ignored.

   --  procedure Install_From_Directory
   --    (Directory_Entry : Rose.Directories.Directory_Entry_Type);
   --
   --  procedure Install_With_Caps
   --    (Cap_File_Entry : Rose.Directories.Directory_Entry_Type);

   procedure Install_Executable
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Root_Path : String;
      Category  : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array);
   --  Install Name from Directory, which is Root_Path/Category. The
   --  path is only for messages.

   type Program_Handler is access
     procedure (Directory : Rose.Interfaces.Files.Directory.Client
       .Directory_Client;
                Root_Path : String;
                Category  : String;
                Name      : String;
                Action    : Install_Action;
                Flags     : Install_Flag_Array);

   procedure For_Each_Program
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client;
      Handle      : Program_Handler;
      Found       : out Boolean);
   --  Call Handle for each program that the install list names, in its
   --  order, with its category's directory in /rose/install. Found is
   --  False if there is no /rose/install, or no list in it that can be
   --  read.

   Exec_Client : Rose.Interfaces.Processes.Exec.Client.Exec_Client;
   Map_Client  : Rose.Interfaces.Commands.Map.Client.Map_Client;
   --  The restored exec and command registry, for Update.

   Not_Updated : Natural := 0;
   --  The programs that Update couldn't install again, which keep their
   --  installed versions.

   procedure Report_Not_Updated;
   --  Say how many programs Update couldn't install again, on restore's
   --  console and on the restored persistent console, which is the one
   --  left on the screen (#1109).

   procedure Update_Executable
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Root_Path : String;
      Category  : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array);
   --  Install Name from Directory again in the restored exec, if it is a
   --  saved program, and add it to the restored registry.

   procedure Open_Directory
     (Parent    : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Name      : String;
      Directory : out Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Found     : out Boolean);
   --  Directory is the directory Name in Parent. Found is False if
   --  Parent has no directory called Name.

   function Find_File
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Name      : String)
      return Natural;
   --  The index of the ordinary file Name in Directory, or 0 if there
   --  is none.

   --  The installer has no cap to delete caps, so every cap it gets
   --  stays in its cap table, which holds 4,095 (#667). Directory
   --  clients cost 8 caps each, so it opens /rose/install once, and a
   --  category's directory only when the category changes, rather than
   --  looking up each program's path with Rose.Directories.

   procedure Append
     (To   : in out String;
      Last : in out Natural;
      S    : String);

   procedure Send_Console (Console_Cap : Rose.Capabilities.Capability);
   --  Give init the persistent console's Writer interface, so
   --  that it replaces the boot console before anything else is
   --  installed.

   procedure Send_Clock (Admin_Cap : Rose.Capabilities.Capability);
   --  Give init the clock's Clock_Admin interface, and its Clock
   --  ancestor. Nothing is sent if there is no Clock_Admin, because a
   --  message with no caps is the restore server's last.

   procedure Send_Screen (Surface_Cap : Rose.Capabilities.Capability);
   --  Give init the screen driver's surface (#582). Nothing is sent if
   --  there is none, as for the clock.

   procedure Send_Cap_Sets (Constructor_Cap : Rose.Capabilities.Capability);

   procedure Send_Copy_Test
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client);
   --  Send init the five images of its test of exec's Update copying an
   --  installed image's matching pages (#1110), one call each, from the
   --  files copy_test, copy_test_a and copy_test_b in Directory, which
   --  the build makes from replaceable_test: copy_test, then each of the
   --  others twice. Init updates the program copy_test with each. If a
   --  file is missing, nothing is sent, and init doesn't pass the test.

   procedure Send_Forward_Test
     (Directory  : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Exec_Index : Positive);
   --  Send init replaceable_test's image again, from Directory, for its
   --  test of exec's Update (#902). Init updates the program with it, and
   --  then launches it through the first version's Launch interface,
   --  which a retired version's launch caps do: they launch the newest
   --  version.
   --  Give init the Constructor of the userland cap set server (#825).
   --  Nothing is sent if there is none, as for the clock.

   ------------
   -- Append --
   ------------

   procedure Append
     (To   : in out String;
      Last : in out Natural;
      S    : String)
   is
   begin
      for Ch of S loop
         Last := Last + 1;
         To (Last) := Ch;
      end loop;
   end Append;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Name      : String)
      return Natural
   is
      use Rose.Interfaces.Files.Directory.Client;
      use type Rose.Interfaces.Files.Directory.File_Kind;
      Index : constant Natural := Find_Entry (Directory, Name);
   begin
      if Index /= 0
        and then Entry_File_Kind (Directory, Index)
        = Rose.Interfaces.Files.Directory.Ordinary_File
      then
         return Index;
      else
         return 0;
      end if;
   end Find_File;

   ---------------
   -- Find_Flag --
   ---------------

   procedure Find_Flag
     (Word  : String;
      Flag  : out Install_Flag;
      Found : out Boolean)
   is
      Matches : Boolean;
   begin
      Flag := Install_Flag'First;
      Found := False;

      --  A case, so that a new flag can't be left without a name.

      for Candidate in Install_Flag loop
         case Candidate is
            when Provides_Keyboard_Interface =>
               Matches := Word = "provides_keyboard_interface";
            when Requires_Keyboard_Interface =>
               Matches := Word = "requires_keyboard_interface";
            when Provides_Input_Stream =>
               Matches := Word = "provides_input_stream";
            when Requires_Input_Stream =>
               Matches := Word = "requires_input_stream";
            when Provides_Console =>
               Matches := Word = "provides_console";
            when Is_Driver =>
               Matches := False;
            when Provides_Clock =>
               Matches := Word = "provides_clock";
            when Requires_Framebuffer =>
               Matches := Word = "requires_framebuffer";
            when Provides_Screen =>
               Matches := Word = "provides_screen";
            when Requires_No_Framebuffer =>
               Matches := Word = "requires_no_framebuffer";
            when Requires_Screen =>
               Matches := Word = "requires_screen";
            when Provides_Cap_Sets =>
               Matches := Word = "provides_cap_sets";
            when Requires_Console =>
               Matches := Word = "requires_console";
            when Test_Only =>
               Matches := Word = "test_only";
         end case;

         if Matches then
            Flag := Candidate;
            Found := True;
            return;
         end if;
      end loop;
   end Find_Flag;

   ----------------------
   -- For_Each_Program --
   ----------------------

   procedure For_Each_Program
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client;
      Handle      : Program_Handler;
      Found       : out Boolean)
   is
      use Rose.Interfaces.Files.Directory.Client;
      use type System.Storage_Elements.Storage_Offset;
      Install_Path       : constant String := "/rose/install";
      Install_Directory  : Directory_Client;
      Category_Directory : Directory_Client;
      Category_Name      : String (1 .. 100);
      Category_Last      : Natural := 0;
      Have_Category      : Boolean := False;
      List_Last          : System.Storage_Elements.Storage_Count;

      procedure Process_Executable
        (Name     : String;
         Category : String;
         Action   : Install_Action;
         Flags    : Install_Flag_Array);

      procedure Process_Line
        (Line        : String;
         Line_Number : Positive);
      --  Install the program that Line of the list names, unless it is
      --  blank or only a comment, or refuse it.

      procedure Refuse
        (Line_Number : Positive;
         Reason      : String;
         Detail      : String := "");
      --  Say that line Line_Number of the list is refused, and why:
      --  Reason, then Detail, which is the field at fault, if any. They
      --  are separate so that nothing is concatenated, which would make
      --  the stack unbounded.

      ------------------------
      -- Process_Executable --
      ------------------------

      procedure Process_Executable
        (Name     : String;
         Category : String;
         Action   : Install_Action;
         Flags    : Install_Flag_Array)
      is
         Install_Flags : Install_Flag_Array := Flags;
      begin
         Install_Flags (Is_Driver) := Category = "drivers";

         if not Have_Category
           or else Category_Name (1 .. Category_Last) /= Category
         then
            Open_Directory
              (Install_Directory, Category, Category_Directory,
               Have_Category);

            if not Have_Category then
               Rose.Console_IO.Put ("install: cannot find directory ");
               Rose.Console_IO.Put (Install_Path);
               Rose.Console_IO.Put ("/");
               Rose.Console_IO.Put (Category);
               Rose.Console_IO.New_Line;
               return;
            end if;

            --  A name too long to remember is opened every time.

            if Category'Length <= Category_Name'Length then
               Category_Last := Category'Length;
               Category_Name (1 .. Category_Last) := Category;
            else
               Have_Category := False;
            end if;
         end if;

         Handle
           (Directory => Category_Directory,
            Root_Path => Install_Path,
            Category  => Category,
            Name      => Name,
            Action    => Action,
            Flags     => Install_Flags);
      end Process_Executable;

      ------------------
      -- Process_Line --
      ------------------

      procedure Process_Line
        (Line        : String;
         Line_Number : Positive)
      is
         type Field is
            record
               First : Positive;
               Last  : Natural;
            end record;

         Max_Fields : constant := 3 + Install_Flag_Array'Length;

         Fields : array (1 .. Max_Fields) of Field;
         Count  : Natural := 0;
         Stop   : Natural := Line'Last;
         Next   : Positive := Line'First;
         Action : Install_Action;
         Flags  : Install_Flag_Array := (others => False);
      begin
         for I in Line'Range loop
            if Line (I) = '#' then
               Stop := I - 1;
               exit;
            end if;
         end loop;

         while Next <= Stop loop
            if Is_Space (Line (Next)) then
               Next := Next + 1;
            else
               if Count = Max_Fields then
                  Refuse (Line_Number, "too many fields");
                  return;
               end if;
               Count := Count + 1;
               Fields (Count).First := Next;
               while Next <= Stop and then not Is_Space (Line (Next)) loop
                  Next := Next + 1;
               end loop;
               Fields (Count).Last := Next - 1;
            end if;
         end loop;

         if Count = 0 then
            return;
         elsif Count < 3 then
            Refuse (Line_Number,
                    "a line needs a name, a category and an action");
            return;
         end if;

         declare
            Name     : String renames
                         Line (Fields (1).First .. Fields (1).Last);
            Category : String renames
                         Line (Fields (2).First .. Fields (2).Last);
            Act      : String renames
                         Line (Fields (3).First .. Fields (3).Last);
         begin
            if Name'Length > Max_Name then
               Refuse (Line_Number, "the name is too long");
               return;
            elsif Name (Name'First) = '.' then
               Refuse (Line_Number, "a name can't start with .");
               return;
            end if;

            for Ch of Name loop
               if Ch = '/' then
                  Refuse (Line_Number, "a name can't have / in it");
                  return;
               end if;
            end loop;

            if Category /= "bin" and then Category /= "drivers" then
               Refuse (Line_Number, "unknown category", Category);
               return;
            end if;

            if Act = "launch" then
               Action := Launch;
            elsif Act = "save" then
               Action := Save;
            else
               Refuse (Line_Number, "unknown action", Act);
               return;
            end if;

            for I in 4 .. Count loop
               declare
                  Word  : String renames
                            Line (Fields (I).First .. Fields (I).Last);
                  Flag  : Install_Flag;
                  Known : Boolean;
               begin
                  Find_Flag (Word, Flag, Known);
                  if not Known then
                     Refuse (Line_Number, "unknown flag", Word);
                     return;
                  end if;
                  Flags (Flag) := True;
               end;
            end loop;

            Process_Executable (Name, Category, Action, Flags);
         end;
      end Process_Line;

      ------------
      -- Refuse --
      ------------

      procedure Refuse
        (Line_Number : Positive;
         Reason      : String;
         Detail      : String := "")
      is
      begin
         Rose.Console_IO.Put ("install: ");
         Rose.Console_IO.Put (List_Path);
         Rose.Console_IO.Put (" line ");
         Rose.Console_IO.Put (Num => Line_Number);
         Rose.Console_IO.Put (": ");
         Rose.Console_IO.Put (Reason);
         if Detail /= "" then
            Rose.Console_IO.Put (" ");
            Rose.Console_IO.Put (Detail);
         end if;
         Rose.Console_IO.Put_Line (": refused");
      end Refuse;

   begin

      declare
         Rose_Directory : Directory_Client;
      begin
         Open_Directory
           (Rose.Interfaces.Files.File_System.Client
             .Root_Directory (File_System),
            "rose", Rose_Directory, Found);
         if Found then
            Open_Directory
              (Rose_Directory, "install", Install_Directory, Found);
         end if;
      end;

      if not Found then
         Rose.Console_IO.Put_Line
           ("install: cannot find directory /rose/install");
         return;
      end if;

      Read_List (Install_Directory, List_Last, Found);
      if not Found then
         return;
      end if;

      declare
         Line        : String (1 .. Max_Line) := (others => ' ');
         Line_Length : Natural := 0;
         Too_Long    : Boolean := False;
         Line_Number : Natural := 0;
      begin

         --  One past the end of the list ends a last line that has no
         --  line feed.

         for Index in 1 .. List_Last + 1 loop
            declare
               Ch : constant Character :=
                      (if Index > List_Last
                       then Character'Val (10)
                       else Character'Val (List_Text (Index)));
            begin
               if Ch = Character'Val (10) then
                  Line_Number := Line_Number + 1;
                  if Too_Long then
                     Refuse (Line_Number, "the line is too long");
                  else
                     Process_Line (Line (1 .. Line_Length), Line_Number);
                  end if;
                  Line_Length := 0;
                  Too_Long := False;
               elsif Line_Length = Line'Last then
                  Too_Long := True;
               else
                  Line_Length := Line_Length + 1;
                  Line (Line_Length) := Ch;
               end if;
            end;
         end loop;
      end;
   end For_Each_Program;

   ----------------------------
   -- Get_Ancestor_Interface --
   ----------------------------

   function Get_Ancestor_Interface
     (Interface_Cap : Rose.Capabilities.Capability;
      Ancestor      : Positive)
      return Rose.Capabilities.Capability
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Interface_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word (Ancestor));
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return Rose.Capabilities.Null_Capability;
      else
         return Params.Caps (0);
      end if;
   end Get_Ancestor_Interface;

   -------------------
   -- Get_Interface --
   -------------------

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Block            => True,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Recv_Cap :=
        Capability_Index (Interface_Caps'Length - 1);

      Rose.System_Calls.Invoke_Capability (Params);

      Interface_Caps := (others => 0);

      if not (Params.Control.Flags (Error)
              or else not Params.Control.Flags (Send_Caps))
      then
         declare
            Last : Natural := Interface_Caps'First - 1;
         begin
            for Index in 0 .. Params.Control.Last_Sent_Cap loop
               Last := Last + 1;
               exit when Last > Interface_Caps'Length;
               Interface_Caps (Last) := Params.Caps (Index);
            end loop;
         end;
      end if;
   end Get_Interface;

   ---------------------------------------
   -- Get_Public_Interface_From_Process --
   ---------------------------------------

   function Get_Public_Interface_From_Process
     (Process_Cap : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities;
      Caps          : Array_Of_Capabilities
        (1 .. Rose.Interfaces.Processes.Process_Control.Subprogram_Count);
      Public_Cap    : Rose.Capabilities.Capability;
      Interface_Cap : Rose.Capabilities.Capability :=
                        Rose.Capabilities.Null_Capability;
   begin
      --  The caps of the Process_Control interface that elf's launch
      --  returns (#99).
      Open_Interface (Process_Cap, Caps);

      Public_Cap :=
        Caps (Rose.Interfaces.Processes.Process_Control
                .Published_Interface_Index + 1);

      if Public_Cap /= Null_Capability then
         Get_Interface (Public_Cap, Caps);
         Interface_Cap := Caps (1);
      end if;

      return Interface_Cap;
   end Get_Public_Interface_From_Process;

   -------------
   -- Install --
   -------------

   procedure Install
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client)
   is
      Found : Boolean;
   begin
      For_Each_Program
        (File_System, Install_Executable'Access, Found);
      if not Found then
         return;
      end if;

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Cap (Params, Keyboard_Interface_Cap);
         Rose.System_Calls.Send_Cap (Params, Input_Stream_Cap);
         Rose.System_Calls.Send_Cap (Params, Terminal_Cap);
         Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
         Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(99));

         Rose.System_Calls.Invoke_Capability (Params);
      end;

   end Install;

   ------------------------
   -- Install_Executable --
   ------------------------

   procedure Install_Executable
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Root_Path : String;
      Category  : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array)
   is
      use Rose.Interfaces.Files.Directory.Client;
      use Rose.Interfaces.Streams.Releasable_Reader.Client;
      Caps_Reader    : Releasable_Reader_Client;
      Binary_Reader  : Releasable_Reader_Client;
      --  Released once exec has read them, which frees their slots in
      --  isofs (ADR 0009). Restore has no cap to delete caps, so they
      --  are not closed.
      Exec_Path      : String (1 .. 200);
      Exec_Path_Last : Natural := 0;
      Cap_Path       : String (1 .. 200);
      Cap_Path_Last  : Natural := 0;
      Exec_Index     : Natural;
      Caps_Index     : Natural;
      Flag_Word      : Natural := 0;
      Flag_Bit       : Positive := 1;
   begin
      if Skip_Test_Program (Name, Flags) then
         return;
      end if;

      for Flag of Flags loop
         if Flag then
            Flag_Word := Flag_Word + Flag_Bit;
         end if;
         Flag_Bit := Flag_Bit * 2;
      end loop;

      Append (Exec_Path, Exec_Path_Last, Root_Path);
      Append (Exec_Path, Exec_Path_Last, "/");
      if Category /= "" then
         Append (Exec_Path, Exec_Path_Last, Category);
         Append (Exec_Path, Exec_Path_Last, "/");
      end if;
      Append (Exec_Path, Exec_Path_Last, Name);
      Cap_Path := Exec_Path;
      Cap_Path_Last := Exec_Path_Last;
      Append (Cap_Path, Cap_Path_Last, ".caps");

      Exec_Index := Find_File (Directory, Name);

      if Exec_Index = 0 then
         Rose.Console_IO.Put ("install: cannot find ");
         Rose.Console_IO.Put (Exec_Path (1 .. Exec_Path_Last));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Caps_Index :=
        Find_File
          (Directory,
           Cap_Path (Cap_Path_Last - Name'Length - 4 .. Cap_Path_Last));

      if Caps_Index = 0 then
         Rose.Console_IO.Put ("install: cannot find ");
         Rose.Console_IO.Put (Cap_Path (1 .. Cap_Path_Last));
         Rose.Console_IO.New_Line;
         return;
      end if;

      Caps_Reader :=
        Rose.Interfaces.Files.File.Client.Open_Reader
          (Get_Ordinary_File (Directory, Caps_Index));
      Binary_Reader :=
        Rose.Interfaces.Files.File.Client.Open_Reader
          (Get_Ordinary_File (Directory, Exec_Index));

      Rose.Console_IO.Put ("restore: installing: ");
      Rose.Console_IO.Put (Name);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Flush;

      declare
         use System.Storage_Elements;
         Params : aliased Rose.Invocation.Invocation_Record;
         Last   : Storage_Count := 0;
      begin

         Invoke_Buffer := (others => 0);
         for Ch of Name loop
            Last := Last + 1;
            Invoke_Buffer (Last) := Character'Pos (Ch);
         end loop;

         Rose.System_Calls.Initialize_Send (Params, Init_Cap);
         Rose.System_Calls.Send_Cap
           (Params,
            Rose.Interfaces.Streams.Reader.Client.Get_Interface_Cap
              (Conversions.To_Reader (Caps_Reader)));
         Rose.System_Calls.Send_Cap
           (Params,
            Rose.Interfaces.Streams.Reader.Client.Get_Interface_Cap
              (Conversions.To_Reader (Binary_Reader)));
         Rose.System_Calls.Send_Word
           (Params,
            Natural (Entry_Size (Directory, Exec_Index)));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word'(Install_Action'Pos (Action)));
         Rose.System_Calls.Send_Word
           (Params, Rose.Words.Word (Flag_Word));

         Rose.System_Calls.Send_Buffer
           (Params, Last, Invoke_Buffer'Address, False);

         if Flags (Requires_Keyboard_Interface) then
            Rose.System_Calls.Send_Cap (Params, Keyboard_Interface_Cap);
         elsif Flags (Requires_Screen) then
            Rose.System_Calls.Send_Cap (Params, Screen_Surface_Cap);
         elsif Flags (Requires_Console) then
            Rose.System_Calls.Send_Cap (Params, Console_Writer_Cap);
         end if;

         --  Init replies with the Launch cap for a launch, and with the
         --  command map's Add cap and the Launch cap for a save (#895).

         Rose.System_Calls.Receive_Caps (Params, 2);
         Rose.System_Calls.Invoke_Capability (Params);
         if Params.Control.Flags (Rose.Invocation.Error) then
            Rose.Console_IO.Put_Line ("FAIL");
         elsif Flags (Requires_Framebuffer)
           and then not Params.Control.Flags (Rose.Invocation.Send_Caps)
         then
            Rose.Console_IO.Put_Line ("skipped: no framebuffer");
         elsif Flags (Requires_No_Framebuffer)
           and then not Params.Control.Flags (Rose.Invocation.Send_Caps)
         then
            Rose.Console_IO.Put_Line ("skipped: framebuffer");
         else
            Rose.Console_IO.Put_Line ("done");
            if Params.Control.Flags (Rose.Invocation.Send_Caps) then
               declare
                  Reply : aliased Rose.Invocation.Invocation_Record;
               begin
                  case Action is
                     when Launch =>
                        --  Launch takes the bank to launch against; a
                        --  null bank is exec's own. It used to send the
                        --  name instead, and no bank, which exec read as
                        --  null only because the slot held 0 (#892).

                        Rose.System_Calls.Initialize_Send
                          (Reply, Params.Caps (0));
                        Rose.System_Calls.Send_Cap
                          (Reply, Rose.Capabilities.Null_Capability);
                        Rose.System_Calls.Receive_Caps (Reply, 1);
                        Rose.System_Calls.Invoke_Capability (Reply);

                        if Flags (Provides_Keyboard_Interface) then
                           Keyboard_Interface_Cap :=
                             Get_Public_Interface_From_Process
                               (Rose.Invocation.Sent_Cap (Reply, 0));
                        end if;

                        if Flags (Provides_Clock) then
                           Send_Clock
                             (Get_Public_Interface_From_Process
                                (Rose.Invocation.Sent_Cap (Reply, 0)));
                        end if;

                        if Flags (Provides_Cap_Sets) then
                           Send_Cap_Sets
                             (Get_Public_Interface_From_Process
                                (Rose.Invocation.Sent_Cap (Reply, 0)));
                        end if;

                        if Flags (Provides_Screen) then
                           Screen_Surface_Cap :=
                             Get_Public_Interface_From_Process
                               (Rose.Invocation.Sent_Cap (Reply, 0));
                           Send_Screen (Screen_Surface_Cap);
                        end if;

                        if Flags (Provides_Console) then

                           --  The console is a Terminal (#454). Init gets
                           --  its Writer as the console, its
                           --  Reader as standard input, and the
                           --  Terminal itself.

                           Terminal_Cap :=
                             Get_Public_Interface_From_Process
                               (Rose.Invocation.Sent_Cap (Reply, 0));
                           Console_Writer_Cap :=
                             Get_Ancestor_Interface
                               (Terminal_Cap, Writer_Ancestor);
                           Send_Console (Console_Writer_Cap);
                           if Flags (Provides_Input_Stream) then
                              Input_Stream_Cap :=
                                Get_Ancestor_Interface
                                  (Terminal_Cap, Reader_Ancestor);
                           end if;

                        elsif Flags (Provides_Input_Stream) then
                           Input_Stream_Cap :=
                             Get_Public_Interface_From_Process
                               (Rose.Invocation.Sent_Cap (Reply, 0));
                        end if;

                     when Save =>
                        Rose.System_Calls.Initialize_Send
                          (Reply, Params.Caps (0));
                        Rose.System_Calls.Send_Text (Reply, Name);
                        Rose.System_Calls.Send_Cap (Reply, Params.Caps (1));
                        Rose.System_Calls.Invoke_Capability (Reply);

                        if Name = "replaceable_test" then
                           Send_Forward_Test (Directory, Exec_Index);
                           Send_Copy_Test (Directory);
                        end if;
                  end case;
               end;
            end if;
         end if;
      end;

      Release (Caps_Reader);
      Release (Binary_Reader);
   end Install_Executable;

   --------------------
   -- Open_Directory --
   --------------------

   procedure Open_Directory
     (Parent    : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Name      : String;
      Directory : out Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Found     : out Boolean)
   is
      use Rose.Interfaces.Files.Directory.Client;
      use type Rose.Interfaces.Files.Directory.File_Kind;
      Index : constant Natural := Find_Entry (Parent, Name);
   begin
      Found := Index /= 0
        and then Entry_File_Kind (Parent, Index)
        = Rose.Interfaces.Files.Directory.Directory;
      if Found then
         Directory := Get_Directory (Parent, Index);
      end if;
   end Open_Directory;

   --------------------
   -- Open_Interface --
   --------------------

   procedure Open_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities)
   is
      Caps : Rose.Capabilities.Capability_Array (Interface_Caps'Range);
   begin
      Rose.System_Calls.Get_Interface_Caps (Cap, Caps);
      Interface_Caps := Array_Of_Capabilities (Caps);
   end Open_Interface;

   ---------------
   -- Read_List --
   ---------------

   procedure Read_List
     (Install_Directory : Rose.Interfaces.Files.Directory.Client
        .Directory_Client;
      Last              : out System.Storage_Elements.Storage_Count;
      Success           : out Boolean)
   is
      use System.Storage_Elements;
      use Rose.Interfaces.Files.Directory.Client;
      use Rose.Interfaces.Streams.Releasable_Reader.Client;
      Index         : constant Natural :=
                        Find_File (Install_Directory, "list");
      Reader        : Releasable_Reader_Client;
      Saved_Address : System.Address;
      Saved_Length  : Storage_Count;
      Failed        : Boolean := False;
   begin
      Last := 0;
      Success := False;

      if Index = 0 then
         Rose.Console_IO.Put_Line ("install: cannot find " & List_Path);
         return;
      end if;

      if Entry_Size (Install_Directory, Index) > Max_List_Size then
         Rose.Console_IO.Put_Line
           ("install: " & List_Path & " is bigger than 16 KiB");
         return;
      end if;

      Reader :=
        Rose.Interfaces.Files.File.Client.Open_Reader
          (Get_Ordinary_File (Install_Directory, Index));

      Saved_Address := Rose.System_Calls.Buffer_Address;
      Saved_Length := Rose.System_Calls.Buffer_Length;
      Rose.System_Calls.Use_Buffer
        (List_Transfer'Address, List_Transfer'Length);

      while Last < List_Text'Last loop
         declare
            Chunk_Last : constant Storage_Count :=
                           Storage_Count'Min
                             (Last + List_Transfer'Length, List_Text'Last);
            Count      : Storage_Offset;
         begin
            Read (Reader, List_Text (Last + 1 .. Chunk_Last), Count);
            if Rose.Interfaces.Streams.Releasable_Reader.Client.Has_Error
              or else Count < 0
              or else Count > Chunk_Last - Last
            then
               Failed := True;
               exit;
            end if;
            exit when Count = 0;
            Last := Last + Count;
         end;
      end loop;

      Rose.System_Calls.Use_Buffer (Saved_Address, Saved_Length);
      Release (Reader);

      if Failed then
         Rose.Console_IO.Put_Line ("install: cannot read " & List_Path);
         Last := 0;
         return;
      end if;

      Success := True;
   end Read_List;

   ------------------------
   -- Report_Not_Updated --
   ------------------------

   procedure Report_Not_Updated is
      use System.Storage_Elements;
      use type Rose.Capabilities.Capability;
      Text     : String (1 .. 100);
      Data     : Storage_Array (1 .. Text'Length);
      Last     : Natural := 0;
      Digits_N : String (1 .. 10);
      First    : Positive := Digits_N'Last + 1;
      N        : Natural := Not_Updated;
      Terminal : Rose.Interfaces.Screens.Terminal.Client.Terminal_Client;
      Cap      : Rose.Capabilities.Capability;
   begin
      if Not_Updated = 0 then
         return;
      end if;

      loop
         First := First - 1;
         Digits_N (First) := Character'Val (Character'Pos ('0') + N mod 10);
         N := N / 10;
         exit when N = 0;
      end loop;

      Append (Text, Last, "restore: ");
      Append (Text, Last, Digits_N (First .. Digits_N'Last));
      Append (Text, Last,
              (if Not_Updated = 1 then " program" else " programs"));
      Append (Text, Last,
              " not installed again; installed versions kept");

      Rose.Console_IO.Put_Line (Text (1 .. Last));

      Cap := Restore.Checkpoints.Find_Server
        (Rose.Interfaces.Screens.Terminal.Terminal_Interface);
      if Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Last := Last + 1;
      Text (Last) := Character'Val (10);

      for I in 1 .. Last loop
         Data (Storage_Offset (I)) := Character'Pos (Text (I));
      end loop;
      Rose.Interfaces.Screens.Terminal.Client.Open (Terminal, Cap);
      Rose.Interfaces.Screens.Terminal.Client.Write
        (Terminal, Data (1 .. Storage_Offset (Last)));
      Rose.Interfaces.Screens.Terminal.Client.Close (Terminal);
   end Report_Not_Updated;

   -------------------
   -- Send_Cap_Sets --
   -------------------

   procedure Send_Cap_Sets
     (Constructor_Cap : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Constructor_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line ("restore: no cap set constructor");
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Constructor_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(95));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Cap_Sets;

   ----------------
   -- Send_Clock --
   ----------------

   procedure Send_Clock (Admin_Cap : Rose.Capabilities.Capability) is
      use type Rose.Capabilities.Capability;
      Clock_Cap : constant Rose.Capabilities.Capability :=
                    (if Admin_Cap = Rose.Capabilities.Null_Capability
                     then Rose.Capabilities.Null_Capability
                     else Get_Ancestor_Interface (Admin_Cap, Clock_Ancestor));
      Params    : aliased Rose.Invocation.Invocation_Record;
   begin
      if Clock_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line ("restore: no clock interface");
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Admin_Cap);
      Rose.System_Calls.Send_Cap (Params, Clock_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(97));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Clock;

   ------------------
   -- Send_Console --
   ------------------

   procedure Send_Console (Console_Cap : Rose.Capabilities.Capability) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Console_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(98));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Console;

   --------------------
   -- Send_Copy_Test --
   --------------------

   procedure Send_Copy_Test
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client)
   is
      use Rose.Interfaces.Files.Directory.Client;
      use Rose.Interfaces.Streams.Releasable_Reader.Client;

      type Image_Name is (Base, Changed_End, Shorter);
      Images  : constant array (Image_Name) of Natural :=
        (Base        => Find_File (Directory, "copy_test"),
         Changed_End => Find_File (Directory, "copy_test_a"),
         Shorter     => Find_File (Directory, "copy_test_b"));
      Steps   : constant array (Rose.Words.Word range 1 .. 5) of Image_Name :=
        (Base, Changed_End, Changed_End, Shorter, Shorter);
   begin
      if (for some Index of Images => Index = 0) then
         Rose.Console_IO.Put_Line
           ("restore: exec copy test: an image is missing");
         return;
      end if;

      for Step in Steps'Range loop
         declare
            Index         : constant Positive := Images (Steps (Step));
            Binary_Reader : constant Releasable_Reader_Client :=
              Rose.Interfaces.Files.File.Client.Open_Reader
                (Get_Ordinary_File (Directory, Index));
            Params        : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Init_Cap);
            Rose.System_Calls.Send_Cap
              (Params,
               Rose.Interfaces.Streams.Reader.Client.Get_Interface_Cap
                 (Conversions.To_Reader (Binary_Reader)));
            Rose.System_Calls.Send_Word
              (Params, Natural (Entry_Size (Directory, Index)));
            Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(93));
            Rose.System_Calls.Send_Word (Params, Step);
            Rose.System_Calls.Invoke_Capability (Params);

            --  Init has updated copy_test from the reader by the time it
            --  replies.

            Release (Binary_Reader);
         end;
      end loop;
   end Send_Copy_Test;

   -----------------------
   -- Send_Forward_Test --
   -----------------------

   procedure Send_Forward_Test
     (Directory  : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Exec_Index : Positive)
   is
      use Rose.Interfaces.Files.Directory.Client;
      use Rose.Interfaces.Streams.Releasable_Reader.Client;
      Binary_Reader : constant Releasable_Reader_Client :=
        Rose.Interfaces.Files.File.Client.Open_Reader
          (Get_Ordinary_File (Directory, Exec_Index));
      Params        : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap
        (Params,
         Rose.Interfaces.Streams.Reader.Client.Get_Interface_Cap
           (Conversions.To_Reader (Binary_Reader)));
      Rose.System_Calls.Send_Word
        (Params, Natural (Entry_Size (Directory, Exec_Index)));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(94));
      Rose.System_Calls.Invoke_Capability (Params);

      --  Init has updated the program from the reader, and run its
      --  test, by the time it replies.

      Release (Binary_Reader);
   end Send_Forward_Test;

   -----------------
   -- Send_Screen --
   -----------------

   procedure Send_Screen (Surface_Cap : Rose.Capabilities.Capability) is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Surface_Cap = Rose.Capabilities.Null_Capability then
         Rose.Console_IO.Put_Line ("restore: no screen surface");
         return;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Init_Cap);
      Rose.System_Calls.Send_Cap (Params, Surface_Cap);
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(0));
      Rose.System_Calls.Send_Word (Params, Rose.Words.Word'(96));
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Screen;

   -----------------------
   -- Skip_Test_Program --
   -----------------------

   Test_Install_Known : Boolean := False;
   Test_Install       : Boolean := False;
   --  Whether the kernel's command line has self-tests, once it is read.

   function Skip_Test_Program
     (Name  : String;
      Flags : Install_Flag_Array)
      return Boolean
   is
   begin
      if not Flags (Test_Only) then
         return False;
      end if;

      if not Test_Install_Known then
         Test_Install := Restore.Command_Line.Has_Argument ("self-tests");
         Test_Install_Known := True;
      end if;

      if Test_Install then
         return False;
      end if;

      Rose.Console_IO.Put ("restore: installing: ");
      Rose.Console_IO.Put (Name);
      Rose.Console_IO.Put_Line (": skipped: not a test install");
      return True;
   end Skip_Test_Program;

   ------------
   -- Update --
   ------------

   procedure Update
     (File_System : Rose.Interfaces.Files.File_System.Client
       .File_System_Client)
   is
      use Rose.Capabilities;
      Exec_Cap : constant Capability :=
                   Restore.Checkpoints.Find_Server
                     (Rose.Interfaces.Processes.Exec.Exec_Interface);
      Map_Cap  : constant Capability :=
                   Restore.Checkpoints.Find_Server
                     (Rose.Interfaces.Commands.Map.Map_Interface);
      Found    : Boolean;
   begin
      if Exec_Cap = Null_Capability or else Map_Cap = Null_Capability then
         Rose.Console_IO.Put_Line
           ("restore: cannot find the restored exec and command registry, "
            & "so the commands are unchanged");
         return;
      end if;

      Rose.Interfaces.Processes.Exec.Client.Open (Exec_Client, Exec_Cap);
      Rose.Interfaces.Commands.Map.Client.Open (Map_Client, Map_Cap);

      --  An exec installed before Exec.Update has no cap for it.

      if Rose.Interfaces.Processes.Exec.Client.Get_Update_Cap (Exec_Client)
        = Null_Capability
      then
         Rose.Console_IO.Put_Line
           ("restore: the restored exec can't update programs, so the "
            & "commands are unchanged");
         return;
      end if;

      Rose.Console_IO.Put_Line ("restore: installing the commands again");
      For_Each_Program
        (File_System, Update_Executable'Access, Found);
      Report_Not_Updated;

      --  Then the restored servers that can take over from themselves,
      --  with the programs just installed (ADR 0031 section 5, #830).

      Restore.Replacing.Replace_Servers;
   end Update;

   -----------------------
   -- Update_Executable --
   -----------------------

   procedure Update_Executable
     (Directory : Rose.Interfaces.Files.Directory.Client.Directory_Client;
      Root_Path : String;
      Category  : String;
      Name      : String;
      Action    : Install_Action;
      Flags     : Install_Flag_Array)
   is
      pragma Unreferenced (Root_Path, Category);
      use Rose.Interfaces.Files.Directory.Client;
      use Rose.Interfaces.Streams.Releasable_Reader.Client;
      use type Rose.Capabilities.Capability;
      Exec_Index : Natural;
      Reader     : Releasable_Reader_Client;
   begin
      if Action /= Save or else Skip_Test_Program (Name, Flags) then
         return;
      end if;

      Exec_Index := Find_File (Directory, Name);
      if Exec_Index = 0 then
         Rose.Console_IO.Put ("restore: cannot find ");
         Rose.Console_IO.Put_Line (Name);
         return;
      end if;

      Reader :=
        Rose.Interfaces.Files.File.Client.Open_Reader
          (Get_Ordinary_File (Directory, Exec_Index));

      declare
         Launch : constant Rose.Interfaces.Processes.Launch.Client
           .Launch_Client :=
                    Rose.Interfaces.Processes.Exec.Client.Update
                      (Exec_Client, Conversions.To_Reader (Reader), Name,
                       Rose.Capabilities.Null_Capability);
      begin
         if Rose.Interfaces.Processes.Exec.Client.Has_Error
           or else Rose.Interfaces.Processes.Launch.Client
             .Get_Interface_Cap (Launch)
           = Rose.Capabilities.Null_Capability
         then
            Rose.Console_IO.Put ("restore: cannot install ");
            Rose.Console_IO.Put (Name);
            Rose.Console_IO.Put_Line (" again");
            Not_Updated := Not_Updated + 1;
         else
            Rose.Interfaces.Commands.Map.Client.Add (Map_Client, Name, Launch);
            if Rose.Interfaces.Commands.Map.Client.Has_Error then
               Rose.Console_IO.Put ("restore: cannot add ");
               Rose.Console_IO.Put (Name);
               Rose.Console_IO.Put_Line (" to the registry");
            end if;
            Restore.Replacing.Add_Program
              (Name, Rose.Interfaces.Processes.Launch.Client
                .Get_Interface_Cap (Launch));
         end if;
      end;

      Release (Reader);
   end Update_Executable;

end Restore.Installer;
