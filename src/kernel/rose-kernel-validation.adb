with System;

with Rose.Addresses;
with Rose.Boot.Console;
with Rose.Words;

with Rose.Kernel.Debug;
with Rose.Kernel.Processes;
with Rose.Kernel.Panic;

package body Rose.Kernel.Validation is

   subtype Validated_Process_Id is Rose.Objects.Process_Id range 2 .. 16;
   subtype Validated_Capability is Rose.Capabilities.Capability range 1 .. 64;

   type Allocated_Cap_Array is
     array (Validated_Process_Id, Validated_Capability)
     of Rose.Capabilities.Layout.Capability_Type;

   type Validated_Stack is array (1 .. 16) of Rose.Words.Word_32;

   type Process_Validation_Record is
      record
         Saved   : Boolean             := False;
         Checked : Boolean             := False;
         EIP     : Rose.Words.Word_32  := 16#BAAD_F00D#;
         ESP     : Rose.Words.Word_32  := 16#BAAD_F00D#;
         Stack   : Validated_Stack     := (others => 16#BAAD_F00D#);
      end record;

   Allocated : Allocated_Cap_Array :=
                 (others => (others => Rose.Capabilities.Layout.Null_Cap));

   Procs : array (Validated_Process_Id) of Process_Validation_Record;

   function Check_Range
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability)
     return Boolean;

   procedure Validation_Error
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability;
      Message : String);

   procedure Process_Validation_Error
     (Process  : Rose.Objects.Process_Id;
      Saved    : Rose.Words.Word_32;
      Reported : Rose.Words.Word_32;
      Message  : String);

   ---------------------------
   -- Check_Current_Process --
   ---------------------------

   procedure Check_Current_Process is
      use Rose.Objects;
      use Rose.Words;
      Process : constant Rose.Objects.Process_Id :=
                  Rose.Kernel.Processes.Current_Process_Id;
      ESP     : constant System.Address :=
                  Rose.Addresses.To_System_Address
                    (Rose.Addresses.Virtual_Address
                       (Rose.Kernel.Processes.Current_ESP (Process)));
      Stack   : Validated_Stack;
      pragma Import (Ada, Stack);
      for Stack'Address use ESP;
   begin
      if Process = 0 then
         Validation_Error (Process, 0, "invalid process");
      elsif Process = 1 then
         null;  --  we are idle
         --  Validation_Error (Process, 0, "cannot check process 1");
      elsif Process not in Validated_Process_Id then
         Validation_Error (Process, 0, "process id too large");
      elsif not Procs (Process).Saved then
         if Procs (Process).Checked then
            null;
--              Validation_Error (Process, 0, "process not saved");
         else
            Procs (Process).Checked := True;
         end if;
      else
         if Procs (Process).ESP /=
           Rose.Kernel.Processes.Current_ESP (Process)
         then
            Process_Validation_Error
              (Process, Procs (Process).ESP,
               Rose.Kernel.Processes.Current_ESP (Process),
               "ESP does not match");
         end if;
         if Procs (Process).EIP /=
           Rose.Kernel.Processes.Current_EIP (Process)
         then
            Process_Validation_Error
              (Process, Procs (Process).EIP,
               Rose.Kernel.Processes.Current_EIP (Process),
               "EIP does not match");
         end if;

         declare
            Stack_Index : Positive := 1;
            Stack_Addr  : Word_32  :=
                            Rose.Kernel.Processes.Current_ESP (Process);
            Saved_Stack : Validated_Stack renames
                            Procs (Process).Stack;
            Errors      : Boolean := False;
         begin
            while Stack_Addr < Process_Stack_Bound
              and then Stack_Index <= Saved_Stack'Last
            loop
               if Saved_Stack (Stack_Index) /= Stack (Stack_Index) then
                  if not Errors then
                     Errors := True;
                     Rose.Boot.Console.Put_Line
                       ("Address   Saved     Restored");
                  end if;
                  Rose.Boot.Console.Put (Stack_Addr);
                  Rose.Boot.Console.Put (" ");
                  Rose.Boot.Console.Put (Saved_Stack (Stack_Index));
                  Rose.Boot.Console.Put (" ");
                  Rose.Boot.Console.Put (Stack (Stack_Index));
                  Rose.Boot.Console.New_Line;
               end if;
               Stack_Index := Stack_Index + 1;
               Stack_Addr := Stack_Addr + 1;
            end loop;
            if Errors then
               Validation_Error (Process, 0, "stack does not match");
            end if;
         end;
      end if;
   end Check_Current_Process;

   -----------------
   -- Check_Range --
   -----------------

   function Check_Range
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability)
      return Boolean
   is
      use type Rose.Objects.Process_Id;
      use type Rose.Capabilities.Capability;
   begin
      if Process not in Allocated'Range (1) then
         if Process = 0 then
            Validation_Error (Process, Cap, "invalid process");
         elsif Process = 1 then
            Validation_Error (Process, Cap, "kernel cannot allocate cap");
         else
            Validation_Error (Process, Cap, "process id too large");
         end if;
         return False;
      end if;

      if Cap not in Allocated'Range (2) then
         if Cap = 0 then
            Validation_Error (Process, Cap, "cannot create null capability");
         else
            Validation_Error (Process, Cap, "cap too large");
         end if;
         return False;
      end if;

      return True;
   end Check_Range;

   ----------------
   -- Create_Cap --
   ----------------

   procedure Create_Cap
     (Process  : Rose.Objects.Process_Id;
      Cap      : Rose.Capabilities.Capability;
      Cap_Type : Rose.Capabilities.Layout.Capability_Type)
   is
      use Rose.Capabilities.Layout;
   begin
      if Check_Range (Process, Cap) then
         if Allocated (Process, Cap) /= Null_Cap then
            Validation_Error (Process, Cap, "cap already allocated");
         else
            Allocated (Process, Cap) := Cap_Type;
         end if;
      end if;
   end Create_Cap;

   ----------------
   -- Delete_Cap --
   ----------------

   procedure Delete_Cap
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability)
   is
      use Rose.Capabilities.Layout;
   begin
      if Check_Range (Process, Cap) then
         if Allocated (Process, Cap) = Null_Cap then
            Validation_Error (Process, Cap, "cap not allocated");
         else
            Allocated (Process, Cap) := Null_Cap;
         end if;
      end if;
   end Delete_Cap;

   ------------------------------
   -- Process_Validation_Error --
   ------------------------------

   procedure Process_Validation_Error
     (Process  : Rose.Objects.Process_Id;
      Saved    : Rose.Words.Word_32;
      Reported : Rose.Words.Word_32;
      Message  : String)
   is
      use Rose.Boot.Console;
   begin
      Put ("process ");
      Put (Rose.Words.Word_8 (Process));
      Put (" saved: ");
      Put (Saved);
      Put (" reported: ");
      Put (Reported);
      Put (": error: ");
      Put (Message);
      New_Line;
      Rose.Kernel.Panic.Panic ("validation error");
   end Process_Validation_Error;

   --------------------------
   -- Save_Current_Process --
   --------------------------

   procedure Save_Current_Process is
      Process : constant Rose.Objects.Process_Id :=
                  Rose.Kernel.Processes.Current_Process_Id;
      ESP     : constant System.Address :=
                  Rose.Addresses.To_System_Address
                    (Rose.Addresses.Virtual_Address
                       (Rose.Kernel.Processes.Current_ESP (Process)));
      Stack   : Validated_Stack;
      pragma Import (Ada, Stack);
      for Stack'Address use ESP;
   begin

      if Process in Validated_Process_Id then
         Procs (Process) :=
           Process_Validation_Record'
             (Saved   => True,
              Checked => True,
              EIP     => Rose.Kernel.Processes.Current_EIP (Process),
              ESP     => Rose.Kernel.Processes.Current_ESP (Process),
              Stack   => Stack);
      else
         Validation_Error (Process, 0, "bad pid");
      end if;

   end Save_Current_Process;

   --------------
   -- Validate --
   --------------

   procedure Validate is
      use Rose.Capabilities.Layout;
   begin
      for Process in Allocated'Range (1) loop
         if Rose.Kernel.Processes.Is_Valid_Process_Id (Process) then
            for Cap in Allocated'Range (2) loop
               if Rose.Kernel.Processes.Has_Cap (Process, Cap) then
                  if Allocated (Process, Cap) = Null_Cap then
                     Validation_Error
                       (Process, Cap, "cap should be allocated");
                  elsif Rose.Kernel.Processes.Cap_Type (Process, Cap)
                    /= Allocated (Process, Cap)
                  then
                     Validation_Error
                       (Process, Cap, "mismatched cap type");
                  end if;
               else
                  if Allocated (Process, Cap) /= Null_Cap then
                     Validation_Error
                       (Process, Cap, "cap should not be allocated");
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Validate;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Process : Rose.Objects.Process_Id;
      Cap     : Rose.Capabilities.Capability;
      Message : String)
   is
      use Rose.Boot.Console;
   begin
      Put ("process ");
      Put (Rose.Words.Word_8 (Process));
      Put (" cap ");
      Put (Rose.Words.Word_8 (Cap));
      if Rose.Kernel.Processes.Is_Valid_Process_Id (Process)
        and then Rose.Kernel.Processes.Has_Cap (Process, Cap)
      then
         Put (" ");
         Rose.Kernel.Debug.Put_Cap_Type
           (Rose.Kernel.Processes.Cap_Type (Process, Cap));
      end if;
      Put (": validation error: ");
      Put (Message);
      New_Line;
      Rose.Kernel.Panic.Panic ("validation error");
   end Validation_Error;

end Rose.Kernel.Validation;
