with Rose.Invocation;

with Rose.Boot.Console;
with Rose.Kernel.Heap;
with Rose.Kernel.Processes.Queue;

with Rose.Capabilities.Layout;

package body Rose.Kernel.Clock is

   use Rose.Words;

   Ticks : Word := 0;

   Remaining_Ticks : Word := 0;
   Timeout_Object  : Rose.Objects.Object_Id;
   Timeout_Cap     : Rose.Capabilities.Capability;

   Current_Allocated_Memory : Physical_Bytes := 0;
   Current_Available_Memory : Physical_Bytes := 0;

   -------------------
   -- Current_Ticks --
   -------------------

   function Current_Ticks return Rose.Words.Word is
   begin
      return Ticks;
   end Current_Ticks;

   -----------------------
   -- Handle_Clock_Tick --
   -----------------------

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status
   is
      Current_Pid : constant Rose.Kernel.Processes.Process_Id :=
                      Rose.Kernel.Processes.Current_Process_Id;
   begin
      Ticks := Ticks + 1;

      if Ticks mod 20 = 0 then
         declare
            Allocated   : Physical_Bytes;
            Available   : Physical_Bytes;
            Name        : String (1 .. 20);
            Name_Last   : Natural;
         begin
            Rose.Kernel.Heap.Get_Status (Allocated, Available);
            Rose.Kernel.Processes.Get_Process_Name
              (Current_Pid, Name, Name_Last);
            Rose.Boot.Console.Status_Line
              (Current_Process => Name (1 .. Name_Last),
               Current_Ticks   => Ticks,
               Page_Faults     => Rose.Kernel.Processes.Page_Fault_Count,
               Mem_Allocated   => Current_Allocated_Memory,
               Mem_Available   => Current_Available_Memory,
               Heap_Allocated  => Allocated,
               Heap_Available  => Available);
         end;
      end if;

      if Remaining_Ticks > 0 then
         Remaining_Ticks := Remaining_Ticks - 1;
         if Remaining_Ticks = 0 then
            declare
               use Rose.Invocation;
               Pid    : constant Rose.Kernel.Processes.Process_Id :=
                          Rose.Kernel.Processes.To_Process_Id
                            (Timeout_Object);
               Params : constant Rose.Invocation.Invocation_Record :=
                          Invocation_Record'
                            (Control        => Control_Word'
                               (Flags          =>
                                  (Send => True,
                                   others => False),
                                Last_Sent_Word => 0,
                                others         => <>),
                             Cap            => Timeout_Cap,
                             others         => <>);
               Layout : Rose.Capabilities.Layout.Capability_Layout;
            begin

               Rose.Kernel.Processes.Get_Cap (Pid, Timeout_Cap, Layout);

               Rose.Kernel.Processes.Send_To_Endpoint
                 (From_Process_Id => Rose.Kernel.Processes.To_Process_Id (1),
                  To_Process_Id   => Pid,
                  Sender_Cap      => Params.Cap,
                  Endpoint        => Layout.Header.Endpoint,
                  Identifier      => Layout.Header.Identifier,
                  Params          => Params);
            end;
         end if;
      end if;

      if Rose.Kernel.Processes.Use_Tick then
         Rose.Kernel.Processes.Set_Current_State
           (Current_Pid, Rose.Kernel.Processes.Ready);
         Rose.Kernel.Processes.Queue.Quantum_Finished (Current_Pid);
         return Rose.Kernel.Interrupts.Not_Finished;
      end if;

      return Rose.Kernel.Interrupts.Finished;
   end Handle_Clock_Tick;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Timeout : Rose.Words.Word;
      Object  : Rose.Objects.Object_Id;
      Cap     : Rose.Capabilities.Capability)
   is
   begin
      Remaining_Ticks := Timeout;
      Timeout_Object  := Object;
      Timeout_Cap     := Cap;
   end Set_Timeout;

   ----------------
   -- Update_Mem --
   ----------------

   procedure Update_Mem
     (Allocated, Available : Rose.Addresses.Physical_Bytes)
   is
   begin
      Current_Available_Memory := Available;
      Current_Allocated_Memory := Allocated;
   end Update_Mem;

end Rose.Kernel.Clock;
