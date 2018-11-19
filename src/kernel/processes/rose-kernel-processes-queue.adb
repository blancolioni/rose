with Rose.Words;
with Rose.Kernel.Debug;
with Rose.Kernel.Panic;

with Rose.Kernel.Processes.Debug;
with Rose.Boot.Console;

with Rose.Arch;

package body Rose.Kernel.Processes.Queue is

   type Process_Queue_Type is
      record
         First, Last : Kernel_Process_Access;
      end record;

   Process_Queue : array (Process_Priority) of Process_Queue_Type :=
                     (others => (others => null));

   --------------------
   -- Choose_Process --
   --------------------

   procedure Choose_Process is
      use type Rose.Objects.Process_Id;
   begin
      Next_Process := null;
      for I in Process_Queue'Range loop
         if Process_Queue (I).First /= null then
            declare
               It : Kernel_Process_Access := Process_Queue (I).First;
            begin
               while It /= null loop
                  exit when It.State = Ready;
                  It := It.Queue_Next;
               end loop;

               if It /= null then
                  Next_Process := It;
                  exit;
               end if;
            end;
         end if;
      end loop;

      Current_Process := Next_Process;

      declare
         use Rose.Objects;
      begin
         if Next_Process = null
           or else Next_Process.Pid = 1
         then
            loop
               Rose.Arch.Enable_Interrupts;
               Rose.Arch.Halt;
               Rose.Arch.Disable_Interrupts;
            end loop;
         end if;
      end;

      Resume_Current_Process;

   end Choose_Process;

   ---------------------
   -- Dequeue_Process --
   ---------------------

   procedure Dequeue_Process
     (Process : Rose.Objects.Process_Id)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (Process)'Access;
      Q : Process_Queue_Type renames
            Process_Queue (P.Priority);
   begin
      if Q.First = P then
         Q.First := P.Queue_Next;
      else
         declare
            It : Kernel_Process_Access := Q.First;
         begin
            while It.Queue_Next /= null
              and then It.Queue_Next /= P
            loop
               It := It.Queue_Next;
            end loop;
            if It.Queue_Next = null then
               Rose.Kernel.Panic.Panic
                 ("attempted to dequeue unqueued process: ",
                  Rose.Words.Word (Process));
            else
               It.Queue_Next := P.Queue_Next;
               if Q.Last = P then
                  Q.Last := It;
               end if;
            end if;
         end;
      end if;

      P.Queue_Next := null;
--        Rose.Boot.Console.Put_Line ("Dequeue: " & P.Name);
   end Dequeue_Process;

   -------------------
   -- Queue_Process --
   -------------------

   procedure Queue_Process
     (Process : Rose.Objects.Process_Id)
   is
      P : constant Kernel_Process_Access :=
            Process_Table (Process)'Access;
      Q : Process_Queue_Type renames
            Process_Queue (P.Priority);
   begin
      P.Queue_Next := null;
      if Q.First = null then
         Q.First := P;
         Q.Last := P;
      else
         Q.Last.Queue_Next := P;
         Q.Last := P;
      end if;

--        if True then
--           Rose.Boot.Console.Put ("Queue: ");
--           Rose.Boot.Console.Put (P.Name);
--           Rose.Boot.Console.Put (" ");
--           Rose.Boot.Console.Put (Words.Word_8 (P.Priority));
--           Rose.Boot.Console.Put (" ");
--          Rose.Boot.Console.Put (Words.Word_8'(Process_State'Pos (P.State)));
--           Rose.Boot.Console.Put (" ");
--           Rose.Boot.Console.Put (P.Stack.EIP);
--           Rose.Boot.Console.Put (" ");
--           Rose.Boot.Console.Put (P.Stack.ESP);
--           Rose.Boot.Console.New_Line;
--        end if;
   end Queue_Process;

   ----------------------------
   -- Resume_Current_Process --
   ----------------------------

   procedure Resume_Current_Process is
      use type Rose.Objects.Process_Id;
      Log         : constant Boolean :=
                      Log_Reply
                          or else Log_Process_Activity
                            = Next_Process.Pid;
      Log_Details : constant Boolean :=
                      Log and then
                          Log_Detailed_Invocation
                            = Next_Process.Pid;
   begin
      Invocation_Reply := (if Current_Process.Flags (Invoke_Reply)
                           then 1 else 0);
      Interrupted_Resume := (if Current_Process.Flags (Interrupt_Resume)
                             then 1 else 0);

      if not Current_Process.Flags (Invoke_Reply)
        and then not Current_Process.Flags (Interrupt_Resume)
      then
         if Log then
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Current_Process_Id));
            Rose.Boot.Console.Put (": resuming with no reply");
            Rose.Boot.Console.New_Line;
         end if;
      end if;

      if Current_Process.Flags (Invoke_Reply) then
         if Log then
            Rose.Kernel.Debug.Put_Call
              ("reply", Current_Process.Pid,
               Current_Process.Cap_Cache
                 (Current_Process.Current_Params.Cap),
               Current_Process.Current_Params);

            if Log_Details then
               Debug.Report_Process (Current_Process.Pid, False);
            end if;
         end if;

         Process_Invocation_Record := Current_Process.Current_Params;
         Current_Process.Flags (Invoke_Reply) := False;
      end if;

      if Current_Process.Flags (Interrupt_Resume) then
         if Log then
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Current_Process_Id));
            Rose.Boot.Console.Put (": resuming from interrupt");
            Rose.Boot.Console.New_Line;
            if Log_Details then
               Debug.Report_Process (Current_Process_Id, False);
            end if;
         end if;
         Current_Process.Flags (Interrupt_Resume) := False;
      end if;
   end Resume_Current_Process;

end Rose.Kernel.Processes.Queue;
