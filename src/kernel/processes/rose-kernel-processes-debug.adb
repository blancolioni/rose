with System;
with Rose.Boot.Console;
with Rose.Addresses;

package body Rose.Kernel.Processes.Debug is

   ---------
   -- Put --
   ---------

   procedure Put (Pid : Process_Id) is
      Name : String renames Process_Table (Pid).Name;
   begin
      Rose.Boot.Console.Put (Natural (Pid));
      if Name (Name'First) /= '/' then
         Rose.Boot.Console.Put ("/");
      end if;
      for Ch of Name loop
         if Ch = ' ' then  --  exit when triggers an infinite loop warning
            exit;          --  on gcc 8.2.1
         end if;
         Rose.Boot.Console.Put (Ch);
      end loop;
      Rose.Boot.Console.Put ("[");
      Rose.Boot.Console.Put
        (case Process_Table (Pid).State is
            when Available   => "A",
            when Starting    => "S",
            when Ready       => "R",
            when Running     => "X",
            when Blocked     => "B",
            when Interrupted => "I",
            when Faulted     => "F",
            when Killed      => "K");

      Rose.Boot.Console.Put ("]");
   end Put;

   --------------------
   -- Report_Process --
   --------------------

   procedure Report_Process
     (Pid : Rose.Kernel.Processes.Process_Id;
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
      Put (Natural (Pid));
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

end Rose.Kernel.Processes.Debug;
