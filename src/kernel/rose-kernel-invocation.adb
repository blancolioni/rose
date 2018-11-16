with Ada.Unchecked_Conversion;

with Rose.Words;
with Rose.Boot.Console;

with Rose.Capabilities.Layout;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Processes.Debug;

with Rose.Kernel.Debug;
with Rose.Kernel.Panic;

with Rose.Kernel.Capabilities;
with Rose.Kernel.Validation;

with Rose.Invocation.Trace;

package body Rose.Kernel.Invocation is

   Current_Page_Fault_Count : Natural := 0;

   -----------------------
   -- Handle_Page_Fault --
   -----------------------

   procedure Handle_Page_Fault
     (Address              : Rose.Addresses.Virtual_Address;
      User_Mode            : Boolean;
      Protection_Violation : Boolean;
      Write_Attempt        : Boolean;
      Execution_Attempt    : Boolean)
   is
      Virtual_Page : constant Rose.Addresses.Virtual_Page_Address :=
                       Rose.Addresses.Virtual_Address_To_Page (Address);
   begin

      if False then
         Rose.Boot.Console.Put
           ("page-fault: ");
         Rose.Boot.Console.Put (Rose.Words.Word (Address));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put
           (Rose.Kernel.Processes.Current_ESP
              (Rose.Kernel.Processes.Current_Process_Id));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (if User_Mode then 'u' else 's');
         Rose.Boot.Console.Put (if Protection_Violation then 'p' else 'r');
         Rose.Boot.Console.Put (if Write_Attempt then 'w' else '-');
         Rose.Boot.Console.Put (if Execution_Attempt then 'x' else '-');
         Rose.Boot.Console.New_Line;
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id, False);

      end if;

      Rose.Kernel.Validation.Validate;

      Current_Page_Fault_Count :=
        Current_Page_Fault_Count + 1;

      if not Rose.Kernel.Processes.Have_Process_Handlers then
         Rose.Kernel.Panic.Panic
           (Message => "page fault but no handler installed",
            Addr    => Address);
      end if;

      if not User_Mode then
         Rose.Kernel.Panic.Panic
           (Message => "page fault in supervisor mode",
            Addr    => Address);
      end if;

      Rose.Kernel.Processes.Handle_Page_Fault
        (Virtual_Page      => Virtual_Page,
         Is_Mapped         => Protection_Violation,
         Read_Attempt      =>
            not Write_Attempt and then not Execution_Attempt,
         Write_Attempt     => Write_Attempt,
         Execution_Attempt => Execution_Attempt);

      Rose.Kernel.Processes.Queue.Choose_Process;

      Rose.Kernel.Validation.Validate;
   end Handle_Page_Fault;

   -----------------------
   -- Invoke_Capability --
   -----------------------

   procedure Invoke_Capability
     (Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Capabilities;
      use Rose.Capabilities.Layout;
      use Rose.Invocation;
      use Rose.Kernel.Processes;
      use type Rose.Objects.Process_Id;
      Process_Id : constant Rose.Objects.Process_Id := Current_Process_Id;
      Log : constant Boolean :=
              Log_Invocation
                  or else Log_Process_Activity = Process_Id;
      Log_Details : constant Boolean :=
                      Log and then
                          Log_Detailed_Invocation = Process_Id;
      Cap : Rose.Capabilities.Layout.Generic_Capability_Layout;
      function To_Word_32 is
        new Ada.Unchecked_Conversion (Rose.Invocation.Invocation_Access,
                                      Rose.Words.Word_32);
      pragma Unreferenced (To_Word_32);
   begin

      Rose.Kernel.Validation.Validate;

      if Params.Control.Flags (Block)
        or else Params.Control.Flags (Send_Buffer)
      then
         Set_Current_State (Process_Id, Blocked);
      end if;

      Params.Reply_Cap := Params.Cap;

      if not Current_Process_Cap (Params.Cap, Cap) then
         Rose.Boot.Console.Put ("invoke: bad cap ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Cap));
         Rose.Boot.Console.Put (" for pid ");
         Rose.Boot.Console.Put
           (Rose.Words.Word_8 (Rose.Kernel.Processes.Current_Process_Id));
         Rose.Boot.Console.New_Line;
         Rose.Invocation.Trace.Put (Params.all, True);
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id, True);
         Rose.Kernel.Panic.Panic ("bad cap");
         Return_Error (Params, Rose.Invocation.Invalid_Capability);
      else
         if Log then
            Rose.Kernel.Debug.Put_Call
              ("invoke", Process_Id, Cap, Params.all);
            if Log_Details then
               Rose.Kernel.Processes.Debug.Report_Process
                 (Process_Id, True);
            end if;
         end if;

         Rose.Kernel.Capabilities.Handle
           (Cap, Params);
      end if;

      if Params.Control.Flags (Rose.Invocation.Error) then
         Rose.Kernel.Processes.Set_Current_State
           (Rose.Kernel.Processes.Current_Process_Id,
            Rose.Kernel.Processes.Ready);
      end if;

      Rose.Kernel.Processes.Set_Current_Invocation (Params.all);
      Rose.Kernel.Processes.Queue.Choose_Process;
      Rose.Kernel.Validation.Validate;

   end Invoke_Capability;

   ----------------------
   -- Page_Fault_Count --
   ----------------------

   function Page_Fault_Count
     return Natural
   is
   begin
      return Current_Page_Fault_Count;
   end Page_Fault_Count;

end Rose.Kernel.Invocation;
