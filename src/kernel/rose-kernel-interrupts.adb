with Rose.Boot.Console;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Interrupts is

   type Interrupt_Argument_Spec is
     (No_Argument_Handler,
      Word_Argument_Handler,
      CPU_Exception_Handler);

   type Interrupt_Handler_Record
     (Spec : Interrupt_Argument_Spec := No_Argument_Handler);

   type Interrupt_Handler_Access is
     access all Interrupt_Handler_Record;

   type Interrupt_Handler_Record
     (Spec : Interrupt_Argument_Spec := No_Argument_Handler)
   is
      record
         Next : Interrupt_Handler_Access;
         case Spec is
            when No_Argument_Handler =>
               No_Argument : Interrupt_Handler;
            when Word_Argument_Handler =>
               Word_Argument : Interrupt_Word_Handler;
            when CPU_Exception_Handler =>
               Ex            : Exception_Handler;
         end case;
      end record;

   Max_Interrupt_Handlers : constant := 100;

   type Interrupt_Handler_Array is
     array (1 .. Max_Interrupt_Handlers)
     of aliased Interrupt_Handler_Record;

   Interrupt_Handler_Table : Interrupt_Handler_Array;
   Interrupt_Handler_Count : Natural := 0;

   Interrupt_Table        : array (Rose.Arch.Interrupts.Interrupt_Vector)
     of Interrupt_Handler_Access :=
       (others => null);

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Handler_Access);

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   function Handle_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Argument  : Rose.Words.Word)
      return Interrupt_Handler_Status
   is
      use Rose.Arch.Interrupts;
      It        : Interrupt_Handler_Access := Interrupt_Table (Interrupt);
      State     : Interrupt_Handler_Status := Finished;
      New_State : Interrupt_Handler_Status;
      Page_Fault_Address : Rose.Words.Word_32;
      pragma Import (C, Page_Fault_Address, "page_fault_address");
   begin

      if False and then Interrupt /= Clock_Interrupt then
         Rose.Boot.Console.Put ("handle-interrupt: vector = ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Interrupt));
         Rose.Boot.Console.Put (" code = ");
         Rose.Boot.Console.Put (Argument);
         Rose.Boot.Console.New_Line;
      end if;

      if It = null then
         Rose.Boot.Console.Put ("kernel: ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Interrupt));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (Argument);
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (Page_Fault_Address);
         Rose.Boot.Console.Put (": no handler                       ");
         Rose.Boot.Console.New_Line;
         Rose.Kernel.Processes.Debug.Report_Process
           (Rose.Kernel.Processes.Current_Process_Id, False);
      end if;

      while It /= null loop
         case It.Spec is
            when No_Argument_Handler =>
               New_State := It.No_Argument.all;
            when Word_Argument_Handler =>
               New_State := It.Word_Argument (Argument);
            when CPU_Exception_Handler =>
               It.Ex.all;
               New_State := Finished;
         end case;
         if New_State = Not_Finished then
            State := New_State;
         end if;
         It := It.Next;
      end loop;
      return State;
   end Handle_Interrupt;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Handler_Access)
   is
   begin
      if Interrupt_Table (Interrupt) = null then
         Interrupt_Table (Interrupt) :=
           Interrupt_Handler_Table (Interrupt_Handler_Count)'Access;
      else
         declare
            It : Interrupt_Handler_Access :=
                   Interrupt_Table (Interrupt);
         begin
            while It.Next /= null loop
               It := It.Next;
            end loop;
            It.Next := Handler;
         end;
      end if;
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Handler)
   is
   begin
      Interrupt_Handler_Count := Interrupt_Handler_Count + 1;
      Interrupt_Handler_Table (Interrupt_Handler_Count) :=
        (No_Argument_Handler, null, Handler);
      Set_Handler
        (Interrupt, Interrupt_Handler_Table (Interrupt_Handler_Count)'Access);
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Word_Handler)
   is
   begin
      Interrupt_Handler_Count := Interrupt_Handler_Count + 1;
      Interrupt_Handler_Table (Interrupt_Handler_Count) :=
        (Word_Argument_Handler, null, Handler);
      Set_Handler
        (Interrupt, Interrupt_Handler_Table (Interrupt_Handler_Count)'Access);
   end Set_Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Exception_Handler)
   is
   begin
      Interrupt_Handler_Count := Interrupt_Handler_Count + 1;
      Interrupt_Handler_Table (Interrupt_Handler_Count) :=
        (CPU_Exception_Handler, null, Handler);
      Set_Handler
        (Interrupt, Interrupt_Handler_Table (Interrupt_Handler_Count)'Access);
   end Set_Handler;

end Rose.Kernel.Interrupts;
