with System.Storage_Elements;

with Rose.Addresses;
with Rose.Capabilities;
with Rose.Limits;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

with Rose.Console_IO;

with Rose.Interfaces.Heap.Server;
with Rose.Interfaces.Memory.Server;
with Rose.Interfaces.Process.Server;
with Rose.Interfaces.Receiver.Server;
with Rose.Interfaces.Segment;

with Rose.Interfaces.Region.Client;

with Rose.Server;
with Rose.System_Calls;
with Rose.System_Calls.Client;

with Mem.Calls;
with Mem.Checkpoints;
with Mem.Page_Table;
with Mem.Physical_Map;
with Mem.Processes;
with Mem.Virtual_Map;

package body Mem.Server is

   Server : Rose.Server.Server_Context;

   function New_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   procedure Exit_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Exit_Status : Natural);

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
      Stack_Bound : Rose.Words.Word;
      Environment : System.Storage_Elements.Storage_Array);

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

   procedure Checkpoint
     (Id          : Rose.Objects.Capability_Identifier;
      Append_Page : Rose.Capabilities.Capability);

   function Published_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Heap_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability;

   function Current_Bound
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Words.Word;

   procedure Request_New_Bound
     (Id        : Rose.Objects.Capability_Identifier;
      New_Bound : Rose.Words.Word);

   procedure Publish_Interface
     (Id            : Rose.Objects.Capability_Identifier;
      Interface_Cap : Rose.Capabilities.Capability);

   procedure Send_Cap
     (Id  : Rose.Objects.Capability_Identifier;
      Cap : Rose.Capabilities.Capability)
      renames Publish_Interface;

   procedure Page_Fault
     (Id       : Rose.Objects.Capability_Identifier;
      Object   : Rose.Objects.Object_Id;
      Virtual  : Rose.Words.Word;
      Physical : Rose.Words.Word;
      Action   : Rose.Interfaces.Memory.Page_Access_Type);

   procedure Destroy (Id : Rose.Objects.Capability_Identifier);

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
      use Rose.Interfaces.Segment;
      use type Rose.Words.Word;

   begin
      Mem.Processes.Add_Nonpersistent_Segment
        (Process      => Process_Id (Id),
         Virtual_Base => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Readable     => (Flags and Segment_Readable) /= 0,
         Writable     => (Flags and Segment_Writable) /= 0,
         Executable   => (Flags and Segment_Executable) /= 0,
         Resizable    => (Flags and Segment_Resizable) /= 0);
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
      use Rose.Interfaces.Segment;
      use Rose.Interfaces.Region.Client;
      use type Rose.Words.Word;

      Client : Region_Client;
   begin

      Open (Client, Region);
      Mem.Processes.Add_Segment
        (Process       => Process_Id (Id),
         Virtual_Base  => Rose.Addresses.Virtual_Page_Address (Virtual_Base),
         Virtual_Bound => Rose.Addresses.Virtual_Page_Address (Virtual_Bound),
         Region        => Client,
         Region_Offset => Offset,
         Readable      => (Flags and Segment_Readable) /= 0,
         Writable      => (Flags and Segment_Writable) /= 0,
         Executable    => (Flags and Segment_Executable) /= 0,
         Resizable    => (Flags and Segment_Resizable) /= 0);
   end Add_Segment;

   ----------------
   -- Checkpoint --
   ----------------

   procedure Checkpoint
     (Id          : Rose.Objects.Capability_Identifier;
      Append_Page : Rose.Capabilities.Capability)
   is
      pragma Unreferenced (Id);
      procedure Append (Buffer : System.Address);

      ------------
      -- Append --
      ------------

      procedure Append (Buffer : System.Address) is
         Params : aliased Rose.Invocation.Invocation_Record;
      begin
         Rose.System_Calls.Initialize_Send (Params, Append_Page);
         Rose.System_Calls.Send_Buffer (Params, Rose.Limits.Page_Size,
                                        Buffer, False);
         Rose.System_Calls.Invoke_Capability (Params);
      end Append;

   begin
      Mem.Checkpoints.Checkpoint (Append'Access);
   end Checkpoint;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is

      function Get_Cap (Index : Positive) return Rose.Capabilities.Capability
      is (Rose.System_Calls.Client.Get_Capability
          (Get_Cap_From_Set, (1 => Rose.Words.Word (Index))));

   begin

      Rose.System_Calls.Use_Capabilities
        (Create_Endpoint => Create_Endpoint_Cap);

      Console_Cap      := Get_Cap (1);
      Region_Count_Cap := Get_Cap (2);
      Region_Range_Cap := Get_Cap (3);
      Start_Paging_Cap := Get_Cap (4);

      Rose.Console_IO.Open (Console_Cap);

      Mem.Calls.Load_Memory_Map;

      Rose.Interfaces.Memory.Server.Create_Server
        (Server_Context => Server,
         New_Process    => New_Process'Access,
         Register_Process => Register_Process'Access,
         Page_Fault     => Page_Fault'Access,
         Take_Physical_Memory => Take_Physical_Memory'Access,
         Checkpoint => Checkpoint'Access);

      Rose.Interfaces.Process.Server.Attach_Interface
        (Server_Context => Server,
         Destroy => Destroy'Access,
         Add_Segment    => Add_Segment'Access,
         Add_Nonpersistent_Segment => Add_Nonpersistent_Segment'Access,
         Published_Interface => Published_Interface'Access,
         Get_Object_Id  => Mem.Processes.Get_Object_Id'Access,
         Exit_Process   => Exit_Process'Access,
         Heap_Interface => Heap_Interface'Access,
         Publish_Interface => Publish_Interface'Access,
         Instanced      => True);

      Rose.Interfaces.Heap.Server.Attach_Interface
        (Server_Context    => Server,
         Current_Bound     => Current_Bound'Access,
         Request_New_Bound => Request_New_Bound'Access,
         Instanced         => True);

      Rose.Interfaces.Receiver.Server.Attach_Interface
        (Server_Context => Server,
         Send_Cap       => Send_Cap'Access,
         Instanced      => True);

   end Create_Server;

   ----------------------
   -- Current_Heap_Top --
   ----------------------

   function Current_Bound
     (Id : Rose.Objects.Capability_Identifier)
     return Rose.Words.Word
   is
   begin
      return Rose.Words.Word
        (Mem.Processes.Get_Process_Heap_Bound
           (Process => Process_Id (Id)));
   end Current_Bound;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : Rose.Objects.Capability_Identifier) is
   begin
      Mem.Processes.Kill_Process (Process_Id (Id));
   end Destroy;

   ------------------
   -- Exit_Process --
   ------------------

   procedure Exit_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Exit_Status : Natural)
   is
      pragma Unreferenced (Exit_Status);
   begin
      Mem.Page_Table.Delete_All (Process_Id (Id));
      Mem.Processes.Kill_Process (Process_Id (Id));
   end Exit_Process;

   --------------------
   -- Heap_Interface --
   --------------------

   function Heap_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
   begin
      return Mem.Processes.Get_Heap_Cap (Process_Id (Id));
   end Heap_Interface;

   -----------------
   -- New_Process --
   -----------------

   function New_Process
     (Id          : Rose.Objects.Capability_Identifier;
      Process     : Rose.Capabilities.Capability)
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
      Pid    : constant Process_Id :=
                        Mem.Processes.Get_Process_Id (Object);
      Page_Object : Rose.Objects.Object_Id;
   begin

      if not Mem.Processes.Is_Valid_Process_Id (Pid) then
         Protection_Fault (Object, Virtual_Page, "invalid process id");
         return;
      end if;

      Mem.Processes.Get_Process_Segment
        (Pid, Virtual_Page, Page_Object,
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
               Mem.Processes.Fault_Process (Pid);
            else

               Mem.Processes.Initialize_Page
                 (Process       => Pid,
                  Physical_Page => Page,
                  Virtual_Page  => Virtual_Page);

               Mem.Virtual_Map.Map
                 (Process       => Object,
                  Virtual_Page  => Virtual_Page,
                  Physical_Page => Page,
                  Readable      => Readable,
                  Writable      => Writable,
                  Executable    => Executable);

               Mem.Processes.Resume_Process (Pid);

            end if;
         end;
      else
         declare
            Position : constant Mem.Page_Table.Cursor :=
              Mem.Page_Table.Find (Pid, Virtual_Page);
         begin
            if not Mem.Page_Table.Has_Element (Position) then
               Mem.Processes.Fault_Process (Pid);
            else

               case Action is
                  when Read =>
                     Mem.Processes.Fault_Process (Pid);
                  when Execute =>
                     Mem.Processes.Fault_Process (Pid);
                  when Write =>
                     if Mem.Page_Table.Is_Writable (Position) then
                        --  page was already writable
                        Rose.Console_IO.Put_Line
                          ("mem: second fault on writable page");
                        Rose.Console_IO.Put (Object);
                        Rose.Console_IO.New_Line;

                        Rose.Console_IO.Put ("mem: page fault: pid=");
                        Rose.Console_IO.Put (Natural (Pid));
                        Rose.Console_IO.Put ("; virtual=");
                        Rose.Console_IO.Put (Virtual);
                        Rose.Console_IO.Put ("; physical=");
                        Rose.Console_IO.Put (Physical);
                        Rose.Console_IO.Put ("; action=");
                        Rose.Console_IO.Put (case Action is
                                                when Read    => "read",
                                                when Write   => "write",
                                                when Execute => "execute");
                        Rose.Console_IO.New_Line;

                        Mem.Processes.Fault_Process (Pid);
                     else
                        --  page should be made writable, and dirty
                        --  flag should be set (since this is the
                        --  first write)
                        Mem.Page_Table.Set_Writable (Position);
                        Mem.Processes.Resume_Process (Pid);
                     end if;
               end case;
            end if;
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

   -----------------------
   -- Publish_Interface --
   -----------------------

   procedure Publish_Interface
     (Id            : Rose.Objects.Capability_Identifier;
      Interface_Cap : Rose.Capabilities.Capability)
   is
   begin
      Mem.Processes.Set_Published_Interface_Cap
        (Process_Id (Id), Interface_Cap);
      Rose.Server.Unblock_Endpoint
        (Server, Rose.Interfaces.Process.Published_Interface_Endpoint);
   end Publish_Interface;

   -------------------------
   -- Published_Interface --
   -------------------------

   function Published_Interface
     (Id : Rose.Objects.Capability_Identifier)
      return Rose.Capabilities.Capability
   is
      use type Rose.Capabilities.Capability;
      use type Rose.Objects.Capability_Identifier;
   begin
      if Id = 0 then
         Rose.Console_IO.Put_Line
           ("mem: bad pid: 0");
         return 0;
      end if;

      return Cap : constant Rose.Capabilities.Capability :=
        Mem.Processes.Published_Interface_Cap (Process_Id (Id))
      do
         if Cap = Rose.Capabilities.Null_Capability then
            Rose.Server.Block_Current_Request (Server);
         end if;
      end return;
   end Published_Interface;

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
      Stack_Bound : Rose.Words.Word;
      Environment : System.Storage_Elements.Storage_Array)
   is
      pragma Unreferenced (Id);
      Pid : constant Process_Id :=
                     Mem.Processes.Register_Process (Process);
      function A
        (Value : Rose.Words.Word)
         return Rose.Addresses.Virtual_Page_Address
      is (Rose.Addresses.Virtual_Page_Address (Value));

   begin
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Exec_Base),
         Virtual_Bound => A (Exec_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => True,
         Resizable     => False);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Text_Base),
         Virtual_Bound => A (Text_Bound),
         Readable      => True,
         Writable      => False,
         Executable    => False,
         Resizable     => False);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Data_Base),
         Virtual_Bound => A (Data_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => False);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Data_Bound),
         Virtual_Bound => A (Data_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => True);
      Mem.Processes.Add_Nonpersistent_Segment
        (Process       => Pid,
         Virtual_Base  => A (Stack_Base),
         Virtual_Bound => A (Stack_Bound),
         Readable      => True,
         Writable      => True,
         Executable    => False,
         Resizable     => False);
      Mem.Processes.Add_Environment (Pid, Environment);
   end Register_Process;

   ----------------------
   -- Request_Heap_Top --
   ----------------------

   procedure Request_New_Bound
     (Id        : Rose.Objects.Capability_Identifier;
      New_Bound : Rose.Words.Word)
   is
   begin
      Mem.Processes.Resize_Segment
        (Process      => Process_Id (Id),
         New_Virtual_Bound =>
           Rose.Addresses.Virtual_Page_Address (New_Bound));
   end Request_New_Bound;

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
