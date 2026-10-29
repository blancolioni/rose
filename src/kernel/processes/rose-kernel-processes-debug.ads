package Rose.Kernel.Processes.Debug is

   procedure Report_Process
     (Pid        : Process_Id;
      Show_Stack : Boolean := False);

   procedure Put (Pid : Process_Id);

   procedure Put_EIP (Pid : Process_Id);
   --  Put the instruction pointer that Pid was interrupted at.

end Rose.Kernel.Processes.Debug;
