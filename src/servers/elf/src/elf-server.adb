with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Console_IO;
with Rose.Trace;
with Rose.System_Calls.Client;

with Rose.Interfaces.Command_Endpoint;
with Rose.Interfaces.Executable.Server;

with Rose.Interfaces.Kernel_Process.Client;
with Rose.Interfaces.Memory.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage.Client;

with Elf.Loader;

package body Elf.Server is

   Server_Context : Rose.Server.Server_Context;
   Memory_Client  : Rose.Interfaces.Memory.Client.Memory_Client;

   Last_Process   : Rose.Interfaces.Process.Client.Process_Client;
   Last_Command   : Rose.Capabilities.Capability := 0;
   Have_Last      : Boolean := False;
   --  What the last launch returned: the new process's Process interface,
   --  or a command's endpoint cap. They are kept until the next launch,
   --  because the reply to the launch sends them.

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability);

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   function Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Name  : String;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   function Launch_Command
     (Id    : Rose.Objects.Capability_Identifier;
      Name  : String;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   function Launch_Process
     (Name    : String;
      Image   : Rose.Capabilities.Capability;
      Store   : Rose.Capabilities.Capability;
      Caps    : Rose.Capabilities.Capability_Array;
      Command : Boolean)
      return Rose.Capabilities.Capability;
   --  Create, load and start a process. If Command is True, it is a
   --  command (ADR 0002): the process gets the receive cap of a command
   --  endpoint in its second cap, after its Process interface, and the
   --  result is the endpoint cap. Otherwise the result is the Process
   --  interface.

   procedure Create_Process
     (Name         : String;
      Process      : out Rose.Capabilities.Capability;
      Start_Cap    : out Rose.Capabilities.Capability;
      Initial_Cap  : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability);

   procedure Start_Process
     (Cap   : Rose.Capabilities.Capability;
      Start : Rose.Words.Word);

   procedure Encode_Name
     (Params : in out Rose.Invocation.Invocation_Record;
      Name   : String);

   --------------------
   -- Create_Process --
   --------------------

   procedure Create_Process
     (Name         : String;
      Process      : out Rose.Capabilities.Capability;
      Start_Cap    : out Rose.Capabilities.Capability;
      Initial_Cap  : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;

   begin
      Rose.System_Calls.Initialize_Send (Params, Create_Process_Cap);
      Encode_Name (Params, Name);
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
      end;

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Executable.Server.Create_Server
        (Server_Context => Server_Context,
         Launch         => Launch'Access,
         Launch_Command => Launch_Command'Access);
      Rose.Interfaces.Memory.Client.Open (Memory_Client, Memory_Cap);
   end Create_Server;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability) is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cap /= 0 then
         Rose.System_Calls.Initialize_Send
           (Params, Rose.System_Calls.Delete_Cap_Capability);
         Rose.System_Calls.Send_Cap (Params, Cap);
         Rose.System_Calls.Invoke_Capability (Params);
      end if;
   end Delete_Cap;

   -----------------
   -- Encode_Name --
   -----------------

   procedure Encode_Name
     (Params : in out Rose.Invocation.Invocation_Record;
      Name   : String)
   is
      use type Rose.Invocation.Parameter_Word_Index;
      use type Rose.Words.Word;
      Have_Words : constant Boolean := Name'Length > 0;
      W          : Rose.Words.Word_32 := 0;
      Shift      : Natural := 0;
   begin
      Params.Control.Flags (Rose.Invocation.Send_Words) := Have_Words;
      if Have_Words then
         Params.Control.Last_Sent_Word := 0;
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
     (Id    : Rose.Objects.Capability_Identifier;
      Name  : String;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Launch_Process (Name, Image, Store, Caps, Command => False);
   end Launch;

   --------------------
   -- Launch_Command --
   --------------------

   function Launch_Command
     (Id    : Rose.Objects.Capability_Identifier;
      Name  : String;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Launch_Process (Name, Image, Store, Caps, Command => True);
   end Launch_Command;

   --------------------
   -- Launch_Process --
   --------------------

   function Launch_Process
     (Name    : String;
      Image   : Rose.Capabilities.Capability;
      Store   : Rose.Capabilities.Capability;
      Caps    : Rose.Capabilities.Capability_Array;
      Command : Boolean)
      return Rose.Capabilities.Capability
   is
      use Rose.Interfaces.Process.Client;
      use type Rose.Capabilities.Capability;

      Kernel_Process : Rose.Capabilities.Capability;
      Start_Cap      : Rose.Capabilities.Capability;
      Initial_Cap    : Rose.Capabilities.Capability;
      Endpoint_Cap   : Rose.Capabilities.Capability;
      Command_Cap    : Rose.Capabilities.Capability := 0;
      Kernel_Client  : Rose.Interfaces.Kernel_Process
        .Client.Kernel_Process_Client;
      Process_Client : Rose.Interfaces.Process.Client.Process_Client;
      Region         : Rose.Interfaces.Region.Client.Region_Client;
      Storage        : Rose.Interfaces.Storage.Client.Storage_Client;
      Start          : Rose.Words.Word;
      Success        : Boolean;

      procedure Clean_Up;
      --  Delete the caps that this launch used, apart from its result.

      --------------
      -- Clean_Up --
      --------------

      procedure Clean_Up is
      begin
         Rose.Interfaces.Region.Client.Close (Region);
         Rose.Interfaces.Storage.Client.Close (Storage);
         Rose.Interfaces.Kernel_Process.Client.Close (Kernel_Client);
         Delete_Cap (Start_Cap);
         Delete_Cap (Initial_Cap);
         Delete_Cap (Endpoint_Cap);
         for Cap of Caps loop
            Delete_Cap (Cap);
         end loop;
      end Clean_Up;

   begin
      if Have_Last then
         Rose.Interfaces.Process.Client.Close (Last_Process);
         Have_Last := False;
      end if;
      Delete_Cap (Last_Command);
      Last_Command := 0;

      Rose.Trace.Event ("load " & Name);
      Rose.Interfaces.Region.Client.Open (Region, Image);
      Rose.Interfaces.Storage.Client.Open (Storage, Store);
      Rose.Trace.Event ("regions opened");

      Create_Process
        (Name, Kernel_Process, Start_Cap, Initial_Cap, Endpoint_Cap);
      Rose.Interfaces.Kernel_Process.Client.Open
        (Kernel_Client, Kernel_Process);
      Rose.Trace.Event ("process created");
      Process_Client :=
        Rose.Interfaces.Memory.Client.New_Process
          (Item    => Memory_Client,
           Process => Kernel_Client);
      Rose.Trace.Event ("mem process");

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Initial_Cap);
         Rose.System_Calls.Send_Cap
           (Params,
            Get_Interface_Cap (Process_Client));
         Rose.System_Calls.Invoke_Capability (Params);
      end;

      --  The receive cap of a command endpoint follows the Process
      --  interface (see Rose.Capabilities.Standard), so it is created
      --  before the other caps are sent.

      if Command then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Endpoint_Cap);
            Rose.System_Calls.Send_Endpoint
              (Params, Rose.Interfaces.Command_Endpoint.Launch_Endpoint);
            Rose.System_Calls.Receive_Caps (Params, 1);
            Rose.System_Calls.Invoke_Capability (Params);
            if not Params.Control.Flags (Rose.Invocation.Error) then
               Command_Cap := Params.Caps (0);
            end if;
         end;

         Rose.Trace.Event ("command endpoint");
         if Command_Cap = 0 then
            Rose.Console_IO.Put_Line
              ("elf: failed to create command endpoint");
            Rose.Interfaces.Process.Client.Close (Process_Client);
            Clean_Up;
            return 0;
         end if;
      end if;

      if Caps'Length > 0 then
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Initial_Cap);
            for Cap of Caps loop
               Rose.System_Calls.Send_Cap (Params, Cap);
            end loop;
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end if;

      Elf.Loader.Load_Elf_Image
        (Process_Client, Storage, Region, Start, Success);
      Rose.Trace.Event ("image loaded");
      if Success then
         Start_Process (Start_Cap, Start);
         Rose.Trace.Event ("started");
      else
         Rose.Console_IO.Put_Line ("elf: failed to load process");
      end if;

      Clean_Up;
      Rose.Trace.Event ("cleaned up");

      if not Success then
         Rose.Interfaces.Process.Client.Close (Process_Client);
         Delete_Cap (Command_Cap);
         return 0;
      elsif Command then
         Rose.Interfaces.Process.Client.Close (Process_Client);
         Last_Command := Command_Cap;
         return Command_Cap;
      else
         Last_Process := Process_Client;
         Have_Last := True;
         return Get_Interface_Cap (Process_Client);
      end if;
   end Launch_Process;

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
