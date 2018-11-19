with System;
with Rose.Boot.Console;
with Rose.Addresses;

package body Rose.Kernel.Processes.Debug is

   Saved_ESP   : Rose.Words.Word_32;
   Saved_Stack : array (1 .. 256) of Rose.Words.Word_32;

   procedure Save_Current_Stack;
   pragma Export (C, Save_Current_Stack, "debug_save_current_stack");

   procedure Check_Current_Stack;
   pragma Export (C, Check_Current_Stack, "debug_check_current_stack");

   -------------------------
   -- Check_Current_Stack --
   -------------------------

   procedure Check_Current_Stack is
      use Rose.Objects;
      use Rose.Words;
      ESP   : Virtual_Address :=
                Virtual_Address (Current_Process.Stack.ESP);
      Index : Positive := 1;
   begin
      if Current_Process_Id /= Log_Process_Stack then
         return;
      end if;

      if Current_Process.Stack.ESP /= Saved_ESP then
         Saved_Process_Address := Saved_ESP;
         Report_Process (Current_Process_Id, True);
         return;
      end if;

      while ESP < Process_Stack_Bound
        and then Index < Saved_Stack'Last
      loop
         declare
            A : constant System.Address :=
                  To_System_Address (ESP);
            X : Word_32;
            pragma Import (Ada, X);
            for X'Address use A;
         begin
            if X /= Saved_Stack (Index) then
               Rose.Boot.Console.Put (Word_32 (ESP));
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put (Saved_Stack (Index));
               Rose.Boot.Console.Put (" ");
               Rose.Boot.Console.Put (X);
               Rose.Boot.Console.New_Line;
            end if;

            Index := Index + 1;
            ESP := ESP + 4;
         end;
      end loop;
   end Check_Current_Stack;

   ----------------------------
   -- Report_Current_Process --
   ----------------------------

   procedure Report_Current_Process is
      use Rose.Objects;
   begin
      if Current_Process_Id = Log_Process_Stack then
         Report_Process (Current_Process_Id, True);
      end if;
   end Report_Current_Process;

   --------------------
   -- Report_Process --
   --------------------

   procedure Report_Process
     (Pid : Rose.Objects.Process_Id;
      Show_Stack : Boolean := False)
   is
      use Rose.Boot.Console;
      use Rose.Words;
      Proc : Kernel_Process_Entry renames
               Process_Table (Pid);
   begin
      Put ("Name:   ");
      Put (Proc.Name);
      Put ("     pid ");
      Put (Word_8 (Pid));
      New_Line;
      Put ("  gs:");
      Put (Word_8 (Proc.Stack.GS));
      Put (" fs:");
      Put (Word_8 (Proc.Stack.FS));
      Put (" es:");
      Put (Word_8 (Proc.Stack.ES));
      Put (" ds:");
      Put (Word_8 (Proc.Stack.DS));
      Put (" edi:");
      Put (Proc.Stack.EDI);
      Put (" esi:");
      Put (Proc.Stack.ESI);
      Put (" ebp:");
      Put (Proc.Stack.EBP);
      New_Line;
      Put (" edx:");
      Put (Proc.Stack.EDX);
      Put (" ecx:");
      Put (Proc.Stack.ECX);
      Put (" ebx:");
      Put (Proc.Stack.EBX);
      Put (" eax:");
      Put (Proc.Stack.EAX);
      New_Line;
      Put (" eip:");
      Put (Proc.Stack.EIP);
      Put ("  cs:");
      Put (Proc.Stack.CS);
      Put (" psw:");
      Put (Proc.Stack.PSW);
      Put (" esp:");
      Put (Proc.Stack.ESP);
      Put ("  ss:");
      Put (Proc.Stack.SS);
      New_Line;
      if Saved_Process_Address /= 0 then
         Put ("  saved address: ");
         Put (Saved_Process_Address);
         New_Line;
      end if;

      if Show_Stack then
         declare
            ESP        : constant Word_32 := Proc.Stack.ESP;
            X          : Word_32 := ESP - ESP mod 16;
            Start_Line : Boolean := True;
            Line_Count : Natural := 0;
         begin
            while X < Process_Stack_Bound and then Line_Count < 32 loop
               if Start_Line then
                  Put (X);
                  Put (": ");
                  Start_Line := False;
               end if;

               Put (" ");

               if X >= ESP then
                  declare
                     A : constant System.Address :=
                           Rose.Addresses.To_System_Address
                             (Rose.Addresses.Virtual_Address (X));
                     Y : Word_32;
                     pragma Import (Ada, Y);
                     for Y'Address use A;
                  begin
                     Put (Y);
                  end;
               else
                  Put ("         ");
               end if;
               X := X + 4;
               if X mod 16 = 0 then
                  New_Line;
                  Start_Line := True;
                  Line_Count := Line_Count + 1;
               end if;
            end loop;

            if X mod 16 /= 0 then
               New_Line;
            end if;
         end;
      end if;
   end Report_Process;

   ------------------------
   -- Save_Current_Stack --
   ------------------------

   procedure Save_Current_Stack is
      use Rose.Objects;
      use Rose.Words;
      ESP : Virtual_Address :=
              Virtual_Address (Current_Process.Stack.ESP);
      Index : Positive := 1;
   begin
      if Current_Process_Id /= Log_Process_Stack then
         return;
      end if;
      Saved_ESP := Current_Process.Stack.ESP;

      while ESP < Process_Stack_Bound
        and then Index < Saved_Stack'Last
      loop
         declare
            A : constant System.Address :=
                  To_System_Address (ESP);
            X : Word_32;
            pragma Import (Ada, X);
            for X'Address use A;
         begin
            Saved_Stack (Index) := X;
            Index := Index + 1;
            ESP := ESP + 4;
         end;
      end loop;
   end Save_Current_Stack;

end Rose.Kernel.Processes.Debug;
