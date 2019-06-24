with Rose.Addresses;
with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Rose.Interfaces.Memory.Server;
with Rose.Interfaces.Process_Memory.Server;

with Rose.Interfaces.Region.Client;

with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Client;

with Mem.Calls;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Virtual_Map;

package body Mem.Server is

   Server : Rose.Server.Server_Context;

   function New_Process
     (Id         : Rose.Objects.Capability_Identifier;
      Process    : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure Register_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Exec_Base   : Rose.Words.Word;
      Exec_Bound  : Rose.Words.Word;
      Text_Base   : Rose.Words.Word;
      Text_Bound  : Rose.Words.Word;
      Data_Base   : Rose.Words.Word;
      Data_Bound  : Rose.Words.Word;
      Stack_Base  : Rose.Words.Word;
      Stack_Bound : Rose.Words.Word);

   procedure Take_Physical_Memory
     (Id     : in     Rose.Objects.Capability_Identifier;
      Size   : in     Rose.Words.Word;
      Start  :    out Rose.Words.Word;
      Amount :    out Rose.Words.Word);

   procedure Add_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Offset        : Rose.Words.Word;
      Flags         : Rose.Words.Word);

   procedure Add_Nonpersistent_Segment
     (Id            : in     Rose.Objects.Capability_Identifier;
      Virtual_Base  : in     Rose.Words.Word;
      Virtual_Bound : in     Rose.Words.Word;
      Flags         : in     Rose.Words.Word);

   procedure Page_Fault
     (Id       : Rose.Objects.Capability_Identifier;
      Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Memory.Page_Access_Type);

   procedure Kill (Id : Rose.Objects.Capability_Identifier);

   procedure Protection_Fault
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String);

   -------------------------------
   -- Add_Nonpersistent_Segment --
   -------------------------------

   procedure Add_Nonpersistent_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      use Rose.Interfaces.Process_Memory;
      use type Rose.Words.Word;

   begin
      Mem.Processes.Add_Nonpersistent_Segment
        (Process      => Id,
         Virtual_Base => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Readable     => (Flags and Segment_Readable) /= 0,
         Writable     => (Flags and Segment_Writable) /= 0,
         Executable   => (Flags and Segment_Executable) /= 0);
   end Add_Nonpersistent_Segment;

   -----------------
   -- Add_Segment --
   -----------------

   procedure Add_Segment
     (Id            : Rose.Objects.Capability_Identifier;
      Virtual_Base  : Rose.Words.Word;
      Virtual_Bound : Rose.Words.Word;
      Region        : Rose.Capabilities.Capability;
      Offset        : Rose.Words.Word;
      Flags         : Rose.Words.Word)
   is
      use Rose.Interfaces.Process_Memory;
      use Rose.Interfaces.Region.Client;
      use type Rose.Words.Word;

      Client : Region_Client;
   begin
      Open (Client, Region);
      Mem.Processes.Add_Segment
        (Process       => Id,
         Virtual_Base  => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Region        => Client,
         Region_Offset => Offset,
         Readable      => (Flags and Segment_Readable) /= 0,
         Writable      => (Flags and Segment_Writable) /= 0,
         Executable    => (Flags and Segment_Executable) /= 0);
   end Add_Segment;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin

      Console_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      Region_Count_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      Region_Range_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);
      Start_Paging_Cap :=
        Rose.System_Calls.Client.Get_Capability (Take_Next_Cap);

      Rose.Console_IO.Open (Console_Cap);

      Mem.Calls.Load_Memory_Map;

      Rose.Interfaces.Memory.Server.Create_Server
        (Server_Context => Server,
         New_Process    => New_Process'Access,
         Register_Process => Register_Process'Access,
         Page_Fault     => Page_Fault'Access,
         Take_Physical_Memory => Take_Physical_Memory'Access);

      Rose.Interfaces.Process_Memory.Server.Attach_Interface
        (Server_Context => Server,
         Add_Segment    => Add_Segment'Access,
         Add_Nonpersistent_Segment => Add_Nonpersistent_Segment'Access,
         Destroy        => Kill'Access,
         Get_Object_Id  => Mem.Processes.Get_Object_Id'Access,
         Instanced      => True);

   end Create_Server;

   ----------
   -- Kill --
   ----------

   procedure Kill (Id : Rose.Objects.Capability_Identifier) is
   begin
      Mem.Processes.Kill_Process (Id);
   end Kill;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Id         : Rose.Objects.Capability_Identifier;
      Process    : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability
   is
      pragma Unreferenced (Id);
   begin
      return Mem.Processes.New_Process (Process);
   end New_Process;

   -------------------
   -- On_Page_Fault --
   -------------------

   procedure Page_Fault
     (Id       : Rose.Objects.Capability_Identifier;
      Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Memory.Page_Access_Type)
   is
      pragma Unreferenced (Id);
      use Rose.Addresses;
      use all type Rose.Interfaces.Memory.Page_Access_Type;
      Valid,
      Readable,
      Writable,
      Executable : Boolean := False;
      Virtual_Page : constant Virtual_Page_Address :=
                       Virtual_Page_Address (Virtual);
      Physical_Page : constant Physical_Page_Address :=
                        Physical_Page_Address (Physical);
      Process_Id    : constant Rose.Objects.Capability_Identifier :=
                        Mem.Processes.Get_Process_Id (Object);
      Page_Object : Rose.Objects.Object_Id;
   begin
      if not Mem.Processes.Is_Valid_Process_Id (Process_Id) then
         Protection_Fault (Object, Virtual_Page, "invalid process id");
         return;
      end if;

      Mem.Processes.Get_Process_Segment
        (Process_Id, Virtual_Page, Page_Object,
         Valid, Readable, Writable, Executable);

      if not Valid then
         Protection_Fault (Object, Virtual_Page, "invalid page reference");
         return;
      end if;

      case Action is
         when Read =>
            if not Readable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to read non-readable page");
               return;
            end if;
         when Write =>
            if not Writable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to write non-writable page");
               return;
            end if;
         when Execute =>
            if not Executable then
               Protection_Fault (Object, Virtual_Page,
                                 "attempt to execute non-executable page");
               return;
            end if;
      end case;

      if Physical_Page = 0 then
         declare
            Have_Page : Boolean;
            Page      : Rose.Addresses.Physical_Page_Address;
         begin
            Mem.Physical_Map.Allocate_Page
              (Page, Have_Page);
            if not Have_Page then
               Mem.Virtual_Map.Reclaim (Page, Have_Page);
            end if;

            if not Have_Page then
               Rose.Console_IO.Put_Line ("no page");
               Mem.Processes.Fault_Process (Process_Id);
            else

               Mem.Processes.Initialize_Page
                 (Process       => Process_Id,
                  Physical_Page => Page,
                  Virtual_Page  => Virtual_Page);

               Mem.Virtual_Map.Map
                 (Process       => Object,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Page,
                  Readable      => True,
                  Writable      => Action = Write,
                  Executable    => Action = Execute);


               Mem.Processes.Resume_Process (Process_Id);

            end if;
         end;
      else
         declare
            Mapping   : constant Mem.Virtual_Map.Virtual_Page_Mapping :=
                          Mem.Virtual_Map.Get (Process_Id, Virtual_Page);
         begin
            case Action is
               when Read =>
                  Mem.Processes.Fault_Process (Process_Id);
               when Execute =>
                  Mem.Processes.Fault_Process (Process_Id);
               when Write =>
                  if Mem.Virtual_Map.Writable (Mapping) then
                     Mem.Processes.Fault_Process (Process_Id);
                  else
                     Mem.Virtual_Map.Set_Read_Write (Mapping);
                     Mem.Processes.Resume_Process (Process_Id);
                  end if;
            end case;
         end;
      end if;

   end Page_Fault;

   ----------------------
   -- Protection_Fault --
   ----------------------

   procedure Protection_Fault
     (Process : Rose.Objects.Object_Id;
      Address : Rose.Addresses.Virtual_Page_Address;
      Message : String)
   is
      use Rose.Words;
   begin
      Rose.Console_IO.Put ("mem: error pid ");
      Rose.Console_IO.Put (Rose.Words.Word_64 (Process));
      Rose.Console_IO.Put ("; page ");
      Rose.Console_IO.Put (Word (Address) * 4096);
      Rose.Console_IO.Put (": ");
      Rose.Console_IO.Put (Message);
      Rose.Console_IO.New_Line;
      Mem.Processes.Fault_Process (Mem.Processes.Get_Process_Id (Process));
   end Protection_Fault;

   ----------------------
   -- Register_Process --
   ----------------------

   procedure Register_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability;
      Exec_Base   : Rose.Words.Word;
      Exec_Bound  : Rose.Words.Word;
      Text_Base   : Rose.Words.Word;
      Text_Bound  : Rose.Words.Word;
      Data_Base   : Rose.Words.Word;
      Data_Bound  : Rose.Words.Word;
      Stack_Base  : Rose.Words.Word;
      Stack_Bound : Rose.Words.Word)
   is
      pragma Unreferenced (Id);
      Process_Id : constant Rose.Objects.Capability_Identifier :=
                     Mem.Processes.Register_Process (Process);
      function A
        (Value : Rose.Words.Word)
         return Rose.Addresses.Virtual_Page_Address
      is (Rose.Addresses.Virtual_Page_Address (Value));

   begin
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Process_Id,
         Virtual_Base  => A (Exec_Base),
         Virtual_Bound => A (Exec_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => True);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Process_Id,
         Virtual_Base  => A (Text_Base),
         Virtual_Bound => A (Text_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => False);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Process_Id,
         Virtual_Base  => A (Data_Base),
         Virtual_Bound => A (Data_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Process_Id,
         Virtual_Base  => A (Stack_Base),
         Virtual_Bound => A (Stack_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False);
   end Register_Process;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Start_Paging_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
      Rose.Server.Start_Server (Server);
   end Start_Server;

   --------------------------
   -- Take_Physical_Memory --
   --------------------------

   procedure Take_Physical_Memory
     (Id     : in     Rose.Objects.Capability_Identifier;
      Size   : in     Rose.Words.Word;
      Start  :    out Rose.Words.Word;
      Amount :    out Rose.Words.Word)
   is
      pragma Unreferenced (Id);
   begin
      Start := Rose.Words.Word
        (Mem.Physical_Map.Take_Memory
           (Rose.Addresses.Physical_Bytes (Size)));
      Amount := Size;
   end Take_Physical_Memory;

end Mem.Server;
