package Rose.Kernel.Processes.Debug is

   procedure Report_Process
     (Pid        : Process_Id;
      Show_Stack : Boolean := False);

   procedure Put (Pid : Process_Id);

end Rose.Kernel.Processes.Debug;
