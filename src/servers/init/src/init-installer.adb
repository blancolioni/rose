with System.Storage_Elements;

with Rose.Init_Capabilities;
with Rose.Limits;
with Rose.Objects;

with Rose.Interfaces.Processes.Starting_Process.Client;
with Rose.Interfaces.Storage.Region.Client;
with Rose.Interfaces.Streams.Reader.Client;

with Rose.Invocation;
with Rose.System_Calls;

with Init.Boot;
with Init.Grants;

package body Init.Installer is

   Grant_Text : String (1 .. 1024);
   --  A program's cap requests, which Grant_Caps reads here rather than on
   --  the stack, which is a page before mem runs (#1004).

   Exec_Region_Length : constant := 64 * 2 ** 20;
   --  The storage for the images of the programs that exec installs.
   --  They came to more than 8 MB, with debugging information, once the
   --  console became a terminal (#454), and to 15 MB of 16 with 26
   --  programs (#669).  The store only gives a page space on a device
   --  when it is written, so a larger region costs only page ids.

   Buffer : System.Storage_Elements.Storage_Array (1 .. Rose.Limits.Page_Size)
     with Alignment => 4096;

   function Reserve_Storage
     (Storage_Cap : Rose.Capabilities.Capability;
      Size        : Rose.Words.Word_64)
      return Rose.Interfaces.Storage.Region.Client.Region_Client;

   procedure Copy_Stream
     (From : Rose.Interfaces.Streams.Reader.Client.Reader_Client;
      To   : Rose.Interfaces.Storage.Region.Client.Region_Client);

   Max_Granted_Caps : constant := 24;

   type Granted_Caps is
      record
         Caps  : Init.Calls.Array_Of_Capabilities (1 .. Max_Granted_Caps) :=
                   (others => 0);
         Count : Natural := 0;
      end record;
   --  The caps a program starts with, in order. Init has no secondary
   --  stack, so they are built up here and passed as Caps (1 .. Count).

   procedure Add
     (To  : in out Granted_Caps;
      Cap : Rose.Capabilities.Capability);
   --  Add Cap to To, if there is room.

   procedure Grant_Caps
     (From      : Rose.Interfaces.Streams.Reader.Client.Reader_Client;
      To        : in out Granted_Caps;
      Is_Driver : Boolean);
   --  From is the program's cap requests: one to a line, each a name and
   --  up to two numbers, with comments from '#'.  Add a cap to To for
   --  each request that init's policy allows, in order (#115).  Any
   --  program may have create-endpoint.  Only drivers may have the
   --  others, and not hardware that another program has.

   function Launch_Library
     (Launch_Cap : Rose.Capabilities.Capability;
      Name       : String;
      Image      : Rose.Capabilities.Capability;
      Bank_Cap   : Rose.Capabilities.Capability;
      Caps       : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability;
   --  Launch Image through the ELF server's Launch, Launch_Cap, give the
   --  new process Caps, one at a time (#234), and start it. The result
   --  is its Process_Control cap, or null if the launch failed.

   function Install_Cap_Set
     (Caps : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability;
   --  A boot cap set (#826) of Caps, for exec's Install and Update, or
   --  null if there are none. Exec copies the caps out of it, and init's
   --  cap to it is deleted once the call is made.

   procedure Add_Text
     (Params : in out Rose.Invocation.Invocation_Record;
      Text   : String);

   ---------
   -- Add --
   ---------

   procedure Add
     (To  : in out Granted_Caps;
      Cap : Rose.Capabilities.Capability)
   is
   begin
      if To.Count < Max_Granted_Caps then
         To.Count := To.Count + 1;
         To.Caps (To.Count) := Cap;
      end if;
   end Add;

   --------------
   -- Add_Text --
   --------------

   procedure Add_Text
     (Params : in out Rose.Invocation.Invocation_Record;
      Text   : String)
   is
      use System.Storage_Elements;
      Last : Storage_Offset := Buffer'First - 1;
   begin
      Buffer := (others => 0);
      for Ch of Text loop
         Last := Last + 1;
         Buffer (Last) := Character'Pos (Ch);
      end loop;

      Params.Control.Flags (Rose.Invocation.Send_Buffer) := True;
      Params.Buffer_Address := Buffer'Address;
      Params.Buffer_Length := Last;
   end Add_Text;

   -----------------
   -- Copy_Stream --
   -----------------

   procedure Copy_Stream
     (From : Rose.Interfaces.Streams.Reader.Client.Reader_Client;
      To   : Rose.Interfaces.Storage.Region.Client.Region_Client)
   is
      use System.Storage_Elements;
      use Rose.Interfaces.Streams.Reader.Client;
      use Rose.Interfaces.Storage.Region.Client;
      use type Rose.Objects.Object_Id;
      Last   : Storage_Count;
      Page_Base : Rose.Objects.Object_Id;
      Page_Bound : Rose.Objects.Object_Id;
      Next_Page : Rose.Objects.Object_Id;
   begin
      Get_Range
        (To, Page_Base, Page_Bound);
      Next_Page := Page_Base;

      loop
         Read (From, Buffer, Last);
         exit when Last = 0;
         Buffer (Last + 1 .. Buffer'Last) := (others => 0);
         Put (To, Next_Page, Buffer);
         Next_Page := Next_Page + 1;
      end loop;
   end Copy_Stream;

   ----------------
   -- Grant_Caps --
   ----------------

   procedure Grant_Caps
     (From      : Rose.Interfaces.Streams.Reader.Client.Reader_Client;
      To        : in out Granted_Caps;
      Is_Driver : Boolean)
   is
      use Rose.Interfaces.Streams.Reader.Client;
      use System.Storage_Elements;
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      package Init_Caps renames Rose.Init_Capabilities;

      Owner : constant Init.Grants.Owner_Id := Init.Grants.Next_Owner;
      Text  : String renames Grant_Text;
      Last  : Natural := 0;
      Index : Positive := 1;

      procedure Request (Line : String);
      --  Grant the cap that Line asks for, if the policy allows it

      -------------
      -- Request --
      -------------

      procedure Request (Line : String) is
         Name      : String (1 .. 32);
         Name_Last : Natural := 0;
         Values    : array (1 .. 2) of Rose.Words.Word := (others => 0);
         Count     : Natural := 0;
         Position  : Positive := Line'First;
         Bad       : Boolean := False;
         Cap       : Rose.Capabilities.Capability :=
                       Rose.Capabilities.Null_Capability;

         function Is_Space (Ch : Character) return Boolean
         is (Ch = ' ' or else Ch = Character'Val (9));

         procedure Skip_Spaces;

         -----------------
         -- Skip_Spaces --
         -----------------

         procedure Skip_Spaces is
         begin
            while Position <= Line'Last
              and then Is_Space (Line (Position))
            loop
               Position := Position + 1;
            end loop;
         end Skip_Spaces;

      begin
         Skip_Spaces;

         while Position <= Line'Last
           and then not Is_Space (Line (Position))
         loop
            if Name_Last < Name'Last then
               Name_Last := Name_Last + 1;
               Name (Name_Last) := Line (Position);
            end if;
            Position := Position + 1;
         end loop;

         if Name_Last = 0 then
            return;
         end if;

         loop
            Skip_Spaces;
            exit when Position > Line'Last;
            if Count = Values'Last then
               Bad := True;
               exit;
            end if;
            Count := Count + 1;
            while Position <= Line'Last
              and then not Is_Space (Line (Position))
            loop
               if Line (Position) in '0' .. '9'
                 and then Values (Count) < 2 ** 24
               then
                  Values (Count) :=
                    Values (Count) * 10
                    + Character'Pos (Line (Position)) - 48;
               else
                  Bad := True;
               end if;
               Position := Position + 1;
            end loop;
         end loop;

         declare
            N : String renames Name (1 .. Name_Last);

            function Port
              (Single, Ranged : Rose.Words.Word)
               return Rose.Capabilities.Capability
            is (if Count = 1
                then Init.Grants.Ports
                  (Owner, Single, Init_Caps.Port_Size_8,
                   Values (1), Values (1))
                else Init.Grants.Ports
                  (Owner, Ranged, Init_Caps.Port_Size_8,
                   Values (1), Values (2)));

         begin
            if Bad then
               null;
            elsif N = "create-endpoint" and then Count = 0 then
               Cap := Init_Caps.Create_Endpoint_Cap;
            elsif N = "create-signal" and then Count = 0 then
               Cap := Init_Caps.Create_Signal_Cap;
            elsif not Is_Driver then
               null;
            elsif N = "delete-endpoint" and then Count = 0 then
               Cap := Init_Caps.Delete_Cap;
            elsif N = "rescind-endpoint" and then Count = 0 then
               Cap := Init_Caps.Rescind_Cap;
            elsif N = "get-ticks" and then Count = 0 then
               Cap := Init_Caps.Get_Ticks_64_Cap;
            elsif N = "register-interrupt" and then Count = 1 then
               Cap := Init.Grants.Interrupt (Owner, Values (1));
            elsif N = "device-page" and then Count = 1 then
               Cap := Init.Grants.Device_Page (Owner, Values (1));
            elsif N = "framebuffer" and then Count = 0 then
               Cap := Init.Grants.Framebuffer (Owner);
            elsif N = "bank" and then Count = 2 then
               Cap := Init.Grants.Bank (Values (1), Values (2));
            elsif N = "boot-tag" and then Count = 1
              and then Values (1) in 1 | 8
            then

               --  The framebuffer (8), for the screen driver, and the
               --  kernel's command line (1), which tells the consoles
               --  whether to copy to COM2 (#904).

               Cap := Init.Grants.Boot_Tag (Values (1));
            elsif N = "port-out-8" and then Count in 1 .. 2 then
               Cap := Port (Init_Caps.Port_Out_Endpoint,
                            Init_Caps.Port_Out_Range_Endpoint);
            elsif N = "port-in-8" and then Count in 1 .. 2 then
               Cap := Port (Init_Caps.Port_In_Endpoint,
                            Init_Caps.Port_In_Range_Endpoint);
            end if;

            if Cap = Rose.Capabilities.Null_Capability
              or else To.Count = Max_Granted_Caps
            then
               Init.Calls.Send_String ("init: refused cap request: ");
               Init.Calls.Send_String (Line);
               Init.Calls.Send_String ((1 => Character'Val (10)));
            else
               Add (To, Cap);
            end if;
         end;
      end Request;

   begin
      loop
         declare
            Buffer : Storage_Array (1 .. 256);
            Count  : Storage_Count;
         begin
            Read (From, Buffer, Count);
            exit when Has_Error or else Count = 0;
            for I in 1 .. Count loop
               exit when Last = Text'Last;
               Last := Last + 1;
               Text (Last) := Character'Val (Buffer (I));
            end loop;
         end;
      end loop;

      while Index <= Last loop
         declare
            Line_Start : constant Positive := Index;
            Line_Last  : Natural;
         begin
            while Index <= Last
              and then Text (Index) /= Character'Val (10)
            loop
               Index := Index + 1;
            end loop;
            Line_Last := Index - 1;
            Index := Index + 1;

            for I in Line_Start .. Line_Last loop
               if Text (I) = '#' or else Text (I) = Character'Val (13) then
                  Line_Last := I - 1;
                  exit;
               end if;
            end loop;

            Request (Text (Line_Start .. Line_Last));
         end;
      end loop;
   end Grant_Caps;

   ---------------------
   -- Install_Cap_Set --
   ---------------------

   function Install_Cap_Set
     (Caps : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
   begin
      if Caps'Length = 0 then
         return Rose.Capabilities.Null_Capability;
      else
         return Init.Calls.Create_Cap_Set_With
           (Init.Boot.Cap_Set_Cap, Caps);
      end if;
   end Install_Cap_Set;

   -----------------------------
   -- Install_Command_Library --
   -----------------------------

   function Install_Command_Library
     (Bank_Cap        : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Storage.Region.Client;
      use Rose.Interfaces.Streams.Reader.Client;
      Region      : constant Region_Client :=
                      Reserve_Storage
                        (Reserve_Cap,
                         Rose.Words.Word_64 (Binary_Length));
      Exec_Reader : Reader_Client;
      Cap_Reader  : Reader_Client;
      Caps        : Granted_Caps;
      Exec_Region : constant Region_Client :=
                      Reserve_Storage
                        (Reserve_Cap, Rose.Words.Word_64 (Exec_Region_Length));
   begin

      Open (Exec_Reader, Binary_Stream);
      Copy_Stream (Exec_Reader, Region);

      Open (Cap_Reader, Cap_Stream);
      Grant_Caps (Cap_Reader, Caps, Is_Driver => True);

      Add (Caps, Standard_Output);
      Add (Caps, Launch_Cap);
      Add (Caps, Get_Interface_Cap (Exec_Region));
      Add (Caps, Bank_Cap);

      return Launch_Library
        (Launch_Cap, "command", Get_Interface_Cap (Region), Bank_Cap,
         Caps.Caps (1 .. Caps.Count));
   end Install_Command_Library;

   --------------------------
   -- Install_Exec_Library --
   --------------------------

   function Install_Exec_Library
     (Bank_Cap        : Rose.Capabilities.Capability;
      Reserve_Cap     : Rose.Capabilities.Capability;
      Launch_Cap      : Rose.Capabilities.Capability;
      Launch_Command_Cap : Rose.Capabilities.Capability;
      Trace_Cap       : Rose.Capabilities.Capability;
      Cap_Stream      : Rose.Capabilities.Capability;
      Standard_Output : Rose.Capabilities.Capability;
      Binary_Stream   : Rose.Capabilities.Capability;
      Binary_Length   : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Storage.Region.Client;
      use Rose.Interfaces.Streams.Reader.Client;
      Region : constant Region_Client :=
                     Reserve_Storage
                       (Reserve_Cap,
                        Rose.Words.Word_64 (Binary_Length));
      Exec_Reader : Reader_Client;
      Cap_Reader  : Reader_Client;
      Caps        : Granted_Caps;
      Exec_Region : constant Region_Client :=
                      Reserve_Storage
                        (Reserve_Cap, Rose.Words.Word_64 (Exec_Region_Length));
   begin

      Open (Exec_Reader, Binary_Stream);
      Copy_Stream (Exec_Reader, Region);

      Open (Cap_Reader, Cap_Stream);
      Grant_Caps (Cap_Reader, Caps, Is_Driver => True);

      --  Exec's caps after the ones it asks for are in Exec (exec.ads).

      Add (Caps, Standard_Output);
      Add (Caps, Launch_Cap);
      Add (Caps, Get_Interface_Cap (Exec_Region));
      Add (Caps, Bank_Cap);
      Add (Caps, Launch_Command_Cap);
      Add (Caps, Trace_Cap);

      return Launch_Library
        (Launch_Cap, "exec", Get_Interface_Cap (Region), Bank_Cap,
         Caps.Caps (1 .. Caps.Count));
   end Install_Exec_Library;

   ------------------------
   -- Install_Executable --
   ------------------------

   function Install_Executable
     (Install_Cap   : Rose.Capabilities.Capability;
      Cap_Stream    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Binary_Length : Rose.Words.Word;
      Name          : String;
      Is_Driver     : Boolean;
      Extra_Caps    : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Binary_Length);
      use Rose.Interfaces.Streams.Reader.Client;
      Client  : Reader_Client;
      Caps    : Granted_Caps;
      Params  : aliased Rose.Invocation.Invocation_Record;
      Cap_Set : Rose.Capabilities.Capability;
   begin
      Open (Client, Cap_Stream);
      Grant_Caps (Client, Caps, Is_Driver);
      for Cap of Extra_Caps loop
         Add (Caps, Cap);
      end loop;
      Cap_Set := Install_Cap_Set (Caps.Caps (1 .. Caps.Count));

      Rose.System_Calls.Initialize_Send (Params, Install_Cap);
      Rose.System_Calls.Send_Cap (Params, Binary_Stream);
      Add_Text (Params, Name);
      Rose.System_Calls.Send_Cap (Params, Cap_Set);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.System_Calls.Delete_Cap (Cap_Set);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return 0;
      else
         return Params.Caps (0);
      end if;

   end Install_Executable;

   --------------------
   -- Launch_Library --
   --------------------

   function Launch_Library
     (Launch_Cap : Rose.Capabilities.Capability;
      Name       : String;
      Image      : Rose.Capabilities.Capability;
      Bank_Cap   : Rose.Capabilities.Capability;
      Caps       : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Processes.Starting_Process.Client;
      use type Rose.Capabilities.Capability;
      Params       : aliased Rose.Invocation.Invocation_Record;
      Starting_Cap : Rose.Capabilities.Capability;
      Starting     : Starting_Process_Client;
      Result       : Rose.Capabilities.Capability;
   begin
      Rose.System_Calls.Initialize_Send (Params, Launch_Cap);
      Add_Text (Params, Name);
      Rose.System_Calls.Send_Cap (Params, Image);
      Rose.System_Calls.Send_Cap (Params, Bank_Cap);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
        or else Params.Caps (0) = Rose.Capabilities.Null_Capability
      then
         return 0;
      end if;

      Starting_Cap := Params.Caps (0);
      Open (Starting, Starting_Cap);
      for Cap of Caps loop
         Add_Cap (Starting, Cap);
      end loop;
      Result := Start (Starting);
      Close (Starting);
      Rose.System_Calls.Delete_Cap (Starting_Cap);
      return Result;
   end Launch_Library;

   ---------------------
   -- Reserve_Storage --
   ---------------------

   function Reserve_Storage
     (Storage_Cap : Rose.Capabilities.Capability;
      Size        : Rose.Words.Word_64)
      return Rose.Interfaces.Storage.Region.Client.Region_Client
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Storage_Cap);
      Rose.System_Calls.Send_Word (Params, Size);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);


      return Result : Rose.Interfaces.Storage.Region.Client.Region_Client do
         if Params.Control.Flags (Rose.Invocation.Send_Caps) then
            Rose.Interfaces.Storage.Region.Client.Open
              (Result, Params.Caps (0));
         end if;
      end return;

   end Reserve_Storage;

   -----------------------
   -- Update_Executable --
   -----------------------

   function Update_Executable
     (Update_Cap    : Rose.Capabilities.Capability;
      Binary_Stream : Rose.Capabilities.Capability;
      Name          : String;
      Install_Caps  : Init.Calls.Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
      Params  : aliased Rose.Invocation.Invocation_Record;
      Cap_Set : constant Rose.Capabilities.Capability :=
                  Install_Cap_Set (Install_Caps);
   begin
      Rose.System_Calls.Initialize_Send (Params, Update_Cap);
      Rose.System_Calls.Send_Cap (Params, Binary_Stream);
      Add_Text (Params, Name);
      Rose.System_Calls.Send_Cap (Params, Cap_Set);
      Rose.System_Calls.Receive_Caps (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.System_Calls.Delete_Cap (Cap_Set);

      if Params.Control.Flags (Rose.Invocation.Error)
        or else not Params.Control.Flags (Rose.Invocation.Send_Caps)
      then
         return 0;
      else
         return Params.Caps (0);
      end if;
   end Update_Executable;

end Init.Installer;
