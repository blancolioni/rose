with System.Storage_Elements;

with Rose.Invocation;
with Rose.Objects;
with Rose.Server;
with Rose.System_Calls;
with Rose.Words;

with Rose.Console_IO;
with Rose.System_Calls.Client;

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

   Invoke_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   function Launch
     (Id    : Rose.Objects.Capability_Identifier;
      Name  : String;
      Image : Rose.Capabilities.Capability;
      Store : Rose.Capabilities.Capability;
      Caps  : Rose.Capabilities.Capability_Array)
      return Rose.Capabilities.Capability;

   procedure Create_Process
     (Name        : String;
      Process     : out Rose.Capabilities.Capability;
      Start_Cap   : out Rose.Capabilities.Capability;
      Initial_Cap : out Rose.Capabilities.Capability);

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
     (Name        : String;
      Process     : out Rose.Capabilities.Capability;
      Start_Cap   : out Rose.Capabilities.Capability;
      Initial_Cap : out Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;

   begin
      Rose.System_Calls.Initialize_Send (Params, Create_Process_Cap);
      Encode_Name (Params, Name);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error) then
         Process := Params.Caps (0);
         Start_Cap := Params.Caps (1);
         Initial_Cap := Params.Caps (2);
      else
         Process := 0;
         Start_Cap := 0;
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

      Rose.Console_IO.Open (Console_Cap);

      Rose.Interfaces.Executable.Server.Create_Server
        (Server_Context => Server_Context,
         Launch         => Launch'Access);
      Rose.Interfaces.Memory.Client.Open (Memory_Client, Memory_Cap);
   end Create_Server;

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
      use Rose.Interfaces.Process.Client;

      Kernel_Process : Rose.Capabilities.Capability;
      Start_Cap      : Rose.Capabilities.Capability;
      Initial_Cap    : Rose.Capabilities.Capability;
      Kernel_Client  : Rose.Interfaces.Kernel_Process
        .Client.Kernel_Process_Client;
      Process_Client : Rose.Interfaces.Process.Client.Process_Client;
      Region         : Rose.Interfaces.Region.Client.Region_Client;
      Storage        : Rose.Interfaces.Storage.Client.Storage_Client;
      Start          : Rose.Words.Word;
      Success        : Boolean;
   begin
      Create_Process (Name, Kernel_Process, Start_Cap, Initial_Cap);
      Rose.Interfaces.Kernel_Process.Client.Open
        (Kernel_Client, Kernel_Process);
      Process_Client :=
        Rose.Interfaces.Memory.Client.New_Process
          (Item    => Memory_Client,
           Process => Kernel_Client);

      declare
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Initial_Cap);
         Rose.System_Calls.Send_Cap
           (Params,
            Get_Interface_Cap (Process_Client));
         Rose.System_Calls.Invoke_Capability (Params);
      end;

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

      Rose.Interfaces.Region.Client.Open (Region, Image);
      Rose.Interfaces.Storage.Client.Open (Storage, Store);
      Elf.Loader.Load_Elf_Image
        (Process_Client, Storage, Region, Start, Success);
      if Success then
         Start_Process (Start_Cap, Start);
         return Get_Interface_Cap (Process_Client);
      else
         Rose.Console_IO.Put_Line ("elf: failed to load process");
         return 0;
      end if;
   end Launch;

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
