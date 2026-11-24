with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Console_IO;
with Rose.Trace;
with Rose.System_Calls.Client;
with Rose.System_Calls.Server;

with Rose.Interfaces.Commands.Command_Endpoint;
with Rose.Interfaces.Processes.Executable.Server;
with Rose.Interfaces.Processes.Starting_Process.Server;

with Rose.Interfaces.Processes.Kernel_Process.Client;
with Rose.Interfaces.Processes.Memory.Client;
with Rose.Interfaces.Processes.Process.Client;
with Rose.Interfaces.Processes.Process_Control.Client;
with Rose.Interfaces.Storage.Region_Access.Client;
with Rose.Interfaces.Storage.Bank.Client;

with Elf.Loader;

package body Elf.Server is

   Server_Context : Rose.Server.Server_Context;
   Memory_Client  : Rose.Interfaces.Processes.Memory.Client.Memory_Client;

   Last_Control   : Rose.Interfaces.Processes.Process_Control.Client
     .Process_Control_Client;
   Last_Command   : Rose.Capabilities.Capability := 0;
   Have_Last      : Boolean := False;
   --  What the last launch returned: the new process's Process_Control
   --  cap, which is all that its launcher gets (#99), or a command's
   --  endpoint cap. They are kept until the next launch, because the
   --  reply to the launch sends them.

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability)
     renames Rose.System_Calls.Delete_Cap;

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   Max_Starting : constant := 8;
   type Starting_Index is range 1 .. Max_Starting;

   type Starting_Record is
      record
         In_Use         : Boolean := False;
         Command        : Boolean := False;
         Interface_Cap  : Rose.Capabilities.Capability := 0;
         --  Elf's copy of the Starting_Process cap that Launch returned,
         --  rescinded when the process starts.
         Start_Cap      : Rose.Capabilities.Capability := 0;
         Initial_Cap    : Rose.Capabilities.Capability := 0;
         Command_Cap    : Rose.Capabilities.Capability := 0;
         Process_Client : Rose.Interfaces.Processes.Process.Client
           .Process_Client;
         Start          : Rose.Words.Word := 0;
      end record;

   Starting : array (Starting_Index) of Starting_Record;
   --  The processes that have been launched but not yet started (#234).
   --  The identifier of a Starting_Process cap is its index here.

   function Launch
     (Id        : Rose.Objects.Capability_Identifier;
      Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Transient : Boolean)
      return Rose.Capabilities.Capability;

   function Launch_Command
     (Id        : Rose.Objects.Capability_Identifier;
      Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Transient : Boolean)
      return Rose.Capabilities.Capability;

   function Launch_Process
     (Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Command   : Boolean;
      Transient : Boolean)
      return Rose.Capabilities.Capability;
   --  Create a process and load it, but do not start it. If Transient is
   --  True, it is never saved in a checkpoint (#524). The result is a
   --  Starting_Process cap, through which the caller gives it its caps
   --  and starts it. If Command is True, it is a command (ADR 0002): the
   --  process gets the receive cap of a command endpoint in its second
   --  cap, after its Process interface, and Start gives the endpoint cap.
   --  Otherwise Start gives its Process_Control interface.

   procedure Add_Cap
     (Id  : Rose.Objects.Capability_Identifier;
      Cap : Rose.Capabilities.Capability);

   function Start
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Is_Starting
     (Id : Rose.Objects.Capability_Identifier)
      return Boolean
   is (Id in 1 .. Max_Starting
       and then Starting (Starting_Index (Id)).In_Use);

   procedure Create_Process
     (Name         : String;
      Transient    : Boolean;
      Process      : out Rose.Capabilities.Capability;
      Start_Cap    : out Rose.Capabilities.Capability;
      Initial_Cap  : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability);

   procedure Start_Process
     (Cap   : Rose.Capabilities.Capability;
      Start : Rose.Words.Word);

   procedure Encode_Name
     (Params : in out Rose.Invocation.Invocation_Record;
      Name   : String;
      Flags  : Rose.Words.Word_32);

   -------------
   -- Add_Cap --
   -------------

   procedure Add_Cap
     (Id  : Rose.Objects.Capability_Identifier;
      Cap : Rose.Capabilities.Capability)
   is
   begin
      --  The cap takes the process's next slot. Elf's copy of it, from
      --  the call, is deleted either way.

      if Is_Starting (Id) then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send
              (Params, Starting (Starting_Index (Id)).Initial_Cap);
            Rose.System_Calls.Send_Cap (Params, Cap);
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end if;

      Delete_Cap (Cap);
   end Add_Cap;

   --------------------
   -- Create_Process --
   --------------------

   procedure Create_Process
     (Name         : String;
      Transient    : Boolean;
      Process      : out Rose.Capabilities.Capability;
      Start_Cap    : out Rose.Capabilities.Capability;
      Initial_Cap  : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;

   begin
      Rose.System_Calls.Initialize_Send (Params, Create_Process_Cap);
      Encode_Name (Params, Name, Flags => Boolean'Pos (Transient));
      Rose.System_Calls.Receive_Caps (Params, 4);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error) then
         Process := Params.Caps (0);
         Start_Cap := Params.Caps (1);
         Initial_Cap := Params.Caps (2);
         Endpoint_Cap := Params.Caps (3);
      else
         Process := 0;
         Start_Cap := 0;
         Initial_Cap := 0;
         Endpoint_Cap := 0;
      end if;
   end Create_Process;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      Next_Cap_Index : Positive := 1;

      procedure Next (Cap : out Rose.Capabilities.Capability);

      ----------
      -- Next --
      ----------

      procedure Next (Cap : out Rose.Capabilities.Capability) is
      begin
         Cap :=
           Rose.System_Calls.Client.Get_Capability
             (Get_Cap_From_Set, (1 => Rose.Words.Word (Next_Cap_Index)));
         Next_Cap_Index := Next_Cap_Index + 1;
      end Next;

   begin

      Rose.System_Calls.Use_Buffer
        (Invoke_Buffer'Address, Invoke_Buffer'Last);

      Next (Delete_Endpoint_Cap);
      Next (Rescind_Endpoint_Cap);

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap,
         Delete_Cap      => Delete_Endpoint_Cap,
         Rescind_Cap     => Rescind_Endpoint_Cap);

      Next (Console_Cap);
      Next (Memory_Cap);
      Next (Create_Process_Cap);

      declare
         Trace_Cap : Rose.Capabilities.Capability;
      begin
         Next (Trace_Cap);
         Rose.Trace.Set_Trace_Cap (Trace_Cap);

         --  Nothing reads the cap set again (#826).

         Rose.System_Calls.Delete_Cap (Get_Cap_From_Set);
      end;

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Processes.Executable.Server.Create_Server
        (Server_Context => Server_Context,
         Launch         => Launch'Access,
         Launch_Command => Launch_Command'Access);
      Rose.Interfaces.Processes.Starting_Process.Server.Attach_Interface
        (Server_Context => Server_Context,
         Add_Cap        => Add_Cap'Access,
         Start          => Start'Access,
         Instanced      => True);
      Rose.Interfaces.Processes.Memory.Client.Open (Memory_Client, Memory_Cap);
   end Create_Server;

   -----------------
   -- Encode_Name --
   -----------------

   procedure Encode_Name
     (Params : in out Rose.Invocation.Invocation_Record;
      Name   : String;
      Flags  : Rose.Words.Word_32)
   is
      use type Rose.Invocation.Parameter_Word_Index;
      use type Rose.Words.Word;
      W          : Rose.Words.Word_32 := 0;
      Shift      : Natural := 0;
   begin

      --  The kernel's Create_Process takes the flags in word 0, and the
      --  name in the words after it (#524).

      Params.Control.Flags (Rose.Invocation.Send_Words) := True;
      Params.Data (0) := Flags;
      Params.Control.Last_Sent_Word := 0;
      if Name'Length > 0 then
         Params.Control.Last_Sent_Word := 1;
         for Ch of Name loop
            W := W + Character'Pos (Ch) * 2 ** Shift;
            Shift := Shift + 8;
            if Shift = 32 then
               Shift := 0;
               Params.Data (Params.Control.Last_Sent_Word) := W;
               Params.Control.Last_Sent_Word :=
                 Params.Control.Last_Sent_Word + 1;
               W := 0;
            end if;
         end loop;

         if Shift = 0 then
            Params.Control.Last_Sent_Word :=
              Params.Control.Last_Sent_Word - 1;
         else
            Params.Data (Params.Control.Last_Sent_Word) := W;
         end if;
      end if;
   end Encode_Name;

   ------------
   -- Launch --
   ------------

   function Launch
     (Id        : Rose.Objects.Capability_Identifier;
      Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Transient : Boolean)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Launch_Process
        (Name, Image, Bank, Command => False, Transient => Transient);
   end Launch;

   --------------------
   -- Launch_Command --
   --------------------

   function Launch_Command
     (Id        : Rose.Objects.Capability_Identifier;
      Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Transient : Boolean)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Launch_Process
        (Name, Image, Bank, Command => True, Transient => Transient);
   end Launch_Command;

   --------------------
   -- Launch_Process --
   --------------------

   function Launch_Process
     (Name      : String;
      Image     : Rose.Capabilities.Capability;
      Bank      : Rose.Capabilities.Capability;
      Command   : Boolean;
      Transient : Boolean)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;

      Index          : Starting_Index := Starting_Index'First;
      Found          : Boolean := False;
      Kernel_Process : Rose.Capabilities.Capability;
      Endpoint_Cap   : Rose.Capabilities.Capability;
      Kernel_Client  : Rose.Interfaces.Processes.Kernel_Process
        .Client.Kernel_Process_Client;
      Region         : Rose.Interfaces.Storage.Region_Access.Client
                         .Region_Access_Client;
      Bank_Store     : Rose.Interfaces.Storage.Bank.Client.Bank_Client;
      Process_Bank   : Rose.Interfaces.Storage.Bank.Client.Bank_Client;
      --  The process's own bank (ADR 0015, #750), which mem makes as a
      --  sub-bank of Bank_Store, the bank it is launched against.
      Success        : Boolean;

      procedure Clean_Up;
      --  Close and delete what the launch no longer needs.

      procedure Fail;
      --  Give up the launch, after Create_Process, and end the process
      --  that it made, so that its kernel entry, and mem's, are free
      --  again (#915).

      --------------
      -- Clean_Up --
      --------------

      procedure Clean_Up is
      begin
         Rose.Interfaces.Storage.Region_Access.Client.Close (Region);
         Rose.Interfaces.Storage.Bank.Client.Close (Bank_Store);
         Rose.Interfaces.Storage.Bank.Client.Close (Process_Bank);
         Rose.Interfaces.Processes.Kernel_Process.Client.Close (Kernel_Client);
         Delete_Cap (Endpoint_Cap);
      end Clean_Up;

      ----------
      -- Fail --
      ----------

      procedure Fail is
         use Rose.Interfaces.Processes.Process.Client;
         Item : Starting_Record renames Starting (Index);
      begin
         --  Once mem has the process, ending it there ends the kernel's
         --  process too, and its bank. Before then, only the kernel has
         --  it.

         if Get_Interface_Cap (Item.Process_Client) /= 0 then
            Exit_Process (Item.Process_Client, 1);
         else
            Rose.Interfaces.Processes.Kernel_Process.Client.Destroy
              (Kernel_Client);
         end if;

         Close (Item.Process_Client);
         Clean_Up;
         Delete_Cap (Item.Start_Cap);
         Delete_Cap (Item.Initial_Cap);
         Delete_Cap (Item.Command_Cap);
         Item := (others => <>);
      end Fail;

   begin
      for I in Starting'Range loop
         if not Starting (I).In_Use then
            Index := I;
            Found := True;
            exit;
         end if;
      end loop;

      Rose.Trace.Event ("load ", Name);

      if not Found then
         Rose.Console_IO.Put ("elf: too many processes starting: ");
         Rose.Console_IO.Put_Line (Name);
         return 0;
      end if;

      Rose.Interfaces.Storage.Region_Access.Client.Open (Region, Image);
      Rose.Interfaces.Storage.Bank.Client.Open (Bank_Store, Bank);
      Rose.Trace.Event ("regions opened");

      declare
         Item : Starting_Record renames Starting (Index);
      begin
         Create_Process
           (Name, Transient, Kernel_Process, Item.Start_Cap,
            Item.Initial_Cap, Endpoint_Cap);

         --  The kernel refuses when its process table is full (#724). The
         --  launch fails, and nothing else has been opened yet.

         if Kernel_Process = 0 then
            Rose.Console_IO.Put ("elf: cannot create process ");
            Rose.Console_IO.Put_Line (Name);
            Rose.Interfaces.Storage.Region_Access.Client.Close (Region);
            Rose.Interfaces.Storage.Bank.Client.Close (Bank_Store);
            Item := (others => <>);
            return 0;
         end if;

         Item.In_Use := True;
         Item.Command := Command;

         Rose.Interfaces.Processes.Kernel_Process.Client.Open
           (Kernel_Client, Kernel_Process);
         Rose.Trace.Event ("process created");
         Item.Process_Client :=
           Rose.Interfaces.Processes.Memory.Client.New_Process
             (Item      => Memory_Client,
              Process   => Kernel_Client,
              Bank      => Bank_Store,
              Transient => Transient);

         --  Mem gives a null cap when it has no room for another process
         --  (#915).

         if Rose.Interfaces.Processes.Process.Client.Get_Interface_Cap
           (Item.Process_Client) = 0
         then
            Rose.Console_IO.Put ("elf: no room in mem for ");
            Rose.Console_IO.Put_Line (Name);
            Fail;
            return 0;
         end if;

         Process_Bank := Rose.Interfaces.Processes.Process.Client
           .Bank (Item.Process_Client);
         Rose.Trace.Event ("mem process");

         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Item.Initial_Cap);
            Rose.System_Calls.Send_Cap
              (Params,
               Rose.Interfaces.Processes.Process.Client.Get_Interface_Cap
                 (Item.Process_Client));
            Rose.System_Calls.Invoke_Capability (Params);
         end;

         --  The receive cap of a command endpoint follows the Process
         --  interface (see Rose.Capabilities.Standard), so it is created
         --  before the caller adds the other caps.

         if Command then
            declare
               Params : aliased Rose.Invocation.Invocation_Record;
            begin
               Rose.System_Calls.Initialize_Send (Params, Endpoint_Cap);
               Rose.System_Calls.Send_Endpoint
                 (Params, Rose.Interfaces.Commands.Command_Endpoint
                   .Launch_Endpoint);
               Rose.System_Calls.Receive_Caps (Params, 1);
               Rose.System_Calls.Invoke_Capability (Params);
               if not Params.Control.Flags (Rose.Invocation.Error) then
                  Item.Command_Cap := Params.Caps (0);
               end if;
            end;

            Rose.Trace.Event ("command endpoint");
            if Item.Command_Cap = 0 then
               Rose.Console_IO.Put_Line
                 ("elf: failed to create command endpoint");
               Fail;
               return 0;
            end if;
         end if;

         --  Without a bank of its own, as before mem has the store's
         --  Process_Banks, the process is charged to the launch's bank.

         if Rose.Interfaces.Storage.Bank.Client
           .Get_Interface_Cap (Process_Bank) /= 0
         then
            Elf.Loader.Load_Elf_Image
              (Item.Process_Client, Process_Bank, Region, Item.Start,
               Success);
         else
            Elf.Loader.Load_Elf_Image
              (Item.Process_Client, Bank_Store, Region, Item.Start,
               Success);
         end if;
         Rose.Trace.Event ("image loaded");

         if not Success then
            Rose.Console_IO.Put_Line ("elf: failed to load process");
            Fail;
            return 0;
         end if;

         Clean_Up;

         Item.Interface_Cap :=
           Rose.System_Calls.Server.Create_Endpoint
             (Create_Cap  => Create_Endpoint_Cap,
              Endpoint_Id =>
                Rose.Interfaces.Processes.Starting_Process
                  .Starting_Process_Interface,
              Identifier  => Rose.Objects.Capability_Identifier (Index));
         return Item.Interface_Cap;
      end;
   end Launch_Process;

   -----------
   -- Start --
   -----------

   function Start
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Processes.Process.Client;
   begin
      if Have_Last then
         Rose.Interfaces.Processes.Process_Control.Client.Close
           (Last_Control);
         Have_Last := False;
      end if;
      Delete_Cap (Last_Command);
      Last_Command := 0;

      if not Is_Starting (Id) then
         return 0;
      end if;

      declare
         Item : Starting_Record renames Starting (Starting_Index (Id));
      begin
         Start_Process (Item.Start_Cap, Item.Start);
         Rose.Trace.Event ("started");

         --  The caller's Starting_Process cap is no use from now on.

         Rose.System_Calls.Server.Rescind_Cap (Item.Interface_Cap);
         Delete_Cap (Item.Interface_Cap);
         Delete_Cap (Item.Start_Cap);
         Delete_Cap (Item.Initial_Cap);

         if Item.Command then
            Close (Item.Process_Client);
            Last_Command := Item.Command_Cap;
         else

            --  The launcher gets the process's Process_Control cap, not
            --  its own Process cap, which elf has just used to load it
            --  (#99).

            Last_Control := Control_Cap (Item.Process_Client);
            Close (Item.Process_Client);
            Have_Last := True;
         end if;

         Item := (others => <>);
      end;

      if Have_Last then
         return Rose.Interfaces.Processes.Process_Control.Client
           .Get_Interface_Cap (Last_Control);
      else
         return Last_Command;
      end if;
   end Start;

   -------------------
   -- Start_Process --
   -------------------

   procedure Start_Process
     (Cap   : Rose.Capabilities.Capability;
      Start : Rose.Words.Word)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Word (Params, Start);
      Rose.System_Calls.Invoke_Capability (Params);
   end Start_Process;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Console_IO.Put_Line ("elf: starting server");
      Rose.Server.Start_Server (Server_Context);
   end Start_Server;

end Elf.Server;
