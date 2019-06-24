with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

with Rose.Interfaces.Executable.Server;

with Rose.Interfaces.Memory.Client;
with Rose.Interfaces.Process.Client;
with Rose.Interfaces.Process_Memory.Client;
with Rose.Interfaces.Region.Client;
with Rose.Interfaces.Storage.Client;

with Elf.Loader;

package body Elf.Server is

   Server_Context : Rose.Server.Server_Context;
   Memory_Client  : Rose.Interfaces.Memory.Client.Memory_Client;

   function Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Objects.Object_Id;

   procedure Create_Process
     (Caps      : Rose.Capabilities.Capability_Array;
      Process   : out Rose.Capabilities.Capability;
      Start_Cap : out Rose.Capabilities.Capability);

   function Start_Process
     (Cap   : Rose.Capabilities.Capability;
      Start : Rose.Words.Word)
      return Rose.Objects.Object_Id;

   --------------------
   -- Create_Process --
   --------------------

   procedure Create_Process
     (Caps      : Rose.Capabilities.Capability_Array;
      Process   : out Rose.Capabilities.Capability;
      Start_Cap : out Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Create_Process_Cap);
      for Cap of Caps loop
         Rose.System_Calls.Send_Cap (Params, Cap);
      end loop;
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error) then
         Process := Params.Caps (0);
         Start_Cap := Params.Caps (1);
      else
         Process := 0;
         Start_Cap := 0;
      end if;
   end Create_Process;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
      procedure Next (Cap : out Rose.Capabilities.Capability);

      ----------
      -- Next --
      ----------

      procedure Next (Cap : out Rose.Capabilities.Capability) is
      begin
         Cap := Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      end Next;

   begin
      Next (Delete_Endpoint_Cap);
      Next (Rescind_Endpoint_Cap);
      Next (Console_Cap);
      Next (Memory_Cap);
      Next (Create_Process_Cap);

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Executable.Server.Create_Server
        (Server_Context => Server_Context,
         Launch         => Launch'Access);
      Rose.Interfaces.Memory.Client.Open (Memory_Client, Memory_Cap);
   end Create_Server;

   ------------
   -- Launch --
   ------------

   function Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Objects.Object_Id
   is
      pragma Unreferenced (Id);
      use Rose.Interfaces.Process_Memory.Client;
      Process   : Rose.Capabilities.Capability;
      Start_Cap : Rose.Capabilities.Capability;
      Process_Client : Rose.Interfaces.Process.Client.Process_Client;
      Process_Memory : Process_Memory_Client;
      Region         : Rose.Interfaces.Region.Client.Region_Client;
      Storage        : Rose.Interfaces.Storage.Client.Storage_Client;
      Start          : Rose.Words.Word;
      Success        : Boolean;
   begin
      Create_Process (Caps, Process, Start_Cap);
      Rose.Interfaces.Process.Client.Open (Process_Client, Process);

      Process_Memory :=
        Rose.Interfaces.Memory.Client.New_Process
          (Item       => Memory_Client,
           Process    => Process_Client);

      Rose.Interfaces.Region.Client.Open (Region, Image);
      Rose.Interfaces.Storage.Client.Open (Storage, Store);
      Elf.Loader.Load_Elf_Image
        (Process_Memory, Storage, Region, Start, Success);
      if Success then
         return Start_Process (Start_Cap, Start);
      else
         Rose.Console_IO.Put_Line ("elf: failed to load process");
         return 0;
      end if;
   end Launch;

   -------------------
   -- Start_Process --
   -------------------

   function Start_Process
     (Cap   : Rose.Capabilities.Capability;
      Start : Rose.Words.Word)
      return Rose.Objects.Object_Id
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Send_Word (Params, Start);
      Rose.System_Calls.Invoke_Capability (Params);
      return Rose.Objects.Object_Id
        (Rose.System_Calls.Get_Word_64
           (Params, 0));
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
