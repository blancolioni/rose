with Rose.Capabilities.Layout;
with Rose.Invocation;

with Rose.Boot.Console;
with Rose.Kernel.Processes;

package body Rose.Kernel.Interrupts is

   type Interrupt_Argument_Spec is
     (No_Argument_Handler,
      Word_Argument_Handler,
      CPU_Exception_Handler,
      Capability_Handler);

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
               No_Argument    : Interrupt_Handler;
            when Word_Argument_Handler =>
               Word_Argument  : Interrupt_Word_Handler;
            when CPU_Exception_Handler =>
               Ex             : Exception_Handler;
            when Capability_Handler =>
               Handler_Object : Rose.Objects.Object_Id;
               Handler_Cap    : Rose.Capabilities.Capability;
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

   procedure Send_Interrupt_Cap
     (To_Object : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability;
      Argument  : Rose.Words.Word);

   ----------------------
   -- Handle_Interrupt --
   ----------------------

   function Handle_Interrupt
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Argument  : Rose.Words.Word)
      return Interrupt_Handler_Status
   is
      It        : Interrupt_Handler_Access := Interrupt_Table (Interrupt);
      State     : Interrupt_Handler_Status := Finished;
      New_State : Interrupt_Handler_Status;
   begin

      if It = null then
         Rose.Boot.Console.Put ("kernel: ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Interrupt));
         Rose.Boot.Console.Put (" ");
         Rose.Boot.Console.Put (Argument);
         Rose.Boot.Console.Put (": no handler                       ");
         Rose.Boot.Console.New_Line;
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
            when Capability_Handler =>
               Send_Interrupt_Cap
                 (It.Handler_Object, It.Handler_Cap, Argument);
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
   -- Has_Handler --
   -----------------

   function Has_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector)
      return Boolean
   is
   begin
      return Interrupt_Table (Interrupt) /= null;
   end Has_Handler;

   ------------------------
   -- Send_Interrupt_Cap --
   ------------------------

   procedure Send_Interrupt_Cap
     (To_Object : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability;
      Argument  : Rose.Words.Word)
   is
      use Rose.Invocation;
      Pid : constant Rose.Kernel.Processes.Process_Id :=
              Rose.Kernel.Processes.To_Process_Id (To_Object);
      Params : Rose.Invocation.Invocation_Record :=
                 Invocation_Record'
                   (Control        =>
                                 Control_Word'
                      (Flags          => (Send => True, Send_Words => True,
                                          others => False),
                       Last_Sent_Word => 0,
                       others => <>),
                    Cap            => Cap,
                    others         => <>);
      Layout : Rose.Capabilities.Layout.Capability_Layout;
   begin
      Rose.Kernel.Processes.Get_Cap (Pid, Cap, Layout);

      Params.Data (0) := Argument;
      Rose.Kernel.Processes.Send_To_Endpoint
        (From_Process_Id => Rose.Kernel.Processes.To_Process_Id (1),
         To_Process_Id   => Pid,
         Sender_Cap      => Params.Cap,
         Endpoint        => Layout.Header.Endpoint,
         Identifier      => Layout.Header.Identifier,
         Params          => Params);
   end Send_Interrupt_Cap;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Handler   : Interrupt_Handler_Access)
   is
   begin
      if Interrupt_Table (Interrupt) = null then
         Interrupt_Table (Interrupt) := Handler;
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

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (Interrupt : Rose.Arch.Interrupts.Interrupt_Vector;
      Object    : Rose.Objects.Object_Id;
      Cap       : Rose.Capabilities.Capability)
   is
   begin
      Interrupt_Handler_Count := Interrupt_Handler_Count + 1;
      Interrupt_Handler_Table (Interrupt_Handler_Count) :=
        (Capability_Handler, null, Object, Cap);
      Set_Handler
        (Interrupt, Interrupt_Handler_Table (Interrupt_Handler_Count)'Access);
   end Set_Handler;

end Rose.Kernel.Interrupts;
