package Rose.Kernel.Processes.Debug is

   procedure Report_Process
     (Pid : Rose.Objects.Process_Id;
      Show_Stack : Boolean := False);

   procedure Report_Current_Process;
   pragma Export (C, Report_Current_Process, "debug_report_process");

end Rose.Kernel.Processes.Debug;
