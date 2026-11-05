with Rose.Addresses;
with Rose.Words;
with Rose.Boot.Console;

with Rose.Capabilities.Layout;

with Rose.Kernel.Processes;
with Rose.Kernel.Processes.Queue;
with Rose.Kernel.Processes.Debug;

with Rose.Kernel.Debug;
with Rose.Kernel.Panic;

with Rose.Kernel.Capabilities;

with Rose.Invocation.Trace;

package body Rose.Kernel.Invocation is

   -----------------------------
   -- Fault_Invocation_Record --
   -----------------------------

   procedure Fault_Invocation_Record
     (Address : Rose.Words.Word_32)
   is
      use Rose.Kernel.Processes;
   begin
      Rose.Kernel.Processes.Debug.Put (Current_Process_Id);
      Rose.Boot.Console.Put (": invalid invocation record at ");
      Rose.Boot.Console.Put (Address);
      Rose.Boot.Console.New_Line;
      Set_Current_State (Current_Process_Id, Faulted);
      Rose.Kernel.Processes.Queue.Choose_Process;
   end Fault_Invocation_Record;

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
      Pid : constant Process_Id := Current_Process_Id;
      Log         : constant Boolean :=
                      (Rose.Kernel.Processes.Trace
                         (Current_Process_Id)
                       or else Log_Invocation)
        and then not Params.Control.Flags
          (No_Trace);
        --  or else Params.Control.Last_Sent_Cap > 3
        --      or else Params.Control.Last_Sent_Word > 7;

      Log_Details : constant Boolean := False;
      Cap         : Rose.Capabilities.Layout.Capability_Layout;
   begin

      if Params.Control.Flags (Block)
        or else Params.Control.Flags (Send_Buffer)
      then
         Set_Current_State (Pid, Blocked);
      end if;

      if False then
         if Params.Control.Last_Sent_Cap > 3 then
            Rose.Boot.Console.Put_Line
              ("warning: more than 4 sent caps is deprecated");
         end if;
         if Params.Control.Last_Sent_Word > 7 then
            Rose.Boot.Console.Put_Line
              ("warning: more than 8 sent words is deprecated");
         end if;
      end if;

      Params.Reply_Cap := Params.Cap;

      if not Has_Cap (Pid, Params.Cap) then
         Rose.Kernel.Processes.Debug.Put (Pid);
         Rose.Boot.Console.Put (": invoke: bad cap ");
         Rose.Boot.Console.Put (Rose.Words.Word_8 (Params.Cap));
         Rose.Boot.Console.New_Line;
         Rose.Invocation.Trace.Put (Params.all, True);
         Rose.Kernel.Processes.Return_Error
           (Params, Rose.Invocation.Invalid_Capability);
      else
         Get_Cap (Pid, Params.Cap, Cap);
         if Log then
            Rose.Kernel.Debug.Put_Call
              ("invoke", Cap, Params.all);
            if Log_Details then
               Rose.Kernel.Processes.Debug.Report_Process (Pid);
            end if;
         end if;

         if Cap.Header.Single_Use then
            Rose.Kernel.Processes.Delete_Cap (Pid, Params.Cap);
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

   end Invoke_Capability;

   --------------------------------
   -- Is_Valid_Invocation_Record --
   --------------------------------

   function Is_Valid_Invocation_Record
     (Address : Rose.Words.Word_32)
      return Rose.Words.Word_32
   is
      use Rose.Addresses;
      use Rose.Kernel.Processes;
      Record_Bytes : constant Virtual_Address :=
                       Rose.Invocation.Invocation_Record'Size / 8;
      First        : constant Virtual_Address := Virtual_Address (Address);
      Pid          : constant Process_Id := Current_Process_Id;

      function Is_Mapped return Boolean;
      --  True if every page of the record is mapped in Pid, and Pid can
      --  reach it from user mode.

      ---------------
      -- Is_Mapped --
      ---------------

      function Is_Mapped return Boolean is
      begin
         for Page in Virtual_Address_To_Page (First)
           .. Virtual_Address_To_Page (First + Record_Bytes - 1)
         loop
            if not Is_User_Page (Pid, Page) then
               return False;
            end if;
         end loop;
         return True;
      end Is_Mapped;

   begin
      if First < Virtual_Page_Bytes
        or else First >= Kernel_Virtual_Base - Record_Bytes
      then
         return 0;
      end if;

      --  A record that passed at this address, with no page table changed
      --  since, would pass again (#717).

      if Has_Checked_Record (Pid, Address) then
         if Check_Record_Cache and then not Is_Mapped then
            Rose.Kernel.Panic.Panic
              ("checked invocation record is no longer mapped",
               Rose.Words.Word (Address));
         end if;
         return 1;
      end if;

      if not Is_Mapped then
         return 0;
      end if;

      Set_Checked_Record (Pid, Address);
      return 1;
   end Is_Valid_Invocation_Record;

end Rose.Kernel.Invocation;
