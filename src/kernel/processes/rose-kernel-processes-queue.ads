package Rose.Kernel.Processes.Queue is

   procedure Choose_Process;

   procedure Queue_Process
     (Process : Rose.Kernel.Processes.Process_Id);

   procedure Dequeue_Process
     (Process : Rose.Kernel.Processes.Process_Id);

   procedure Remove_Process
     (Process : Rose.Kernel.Processes.Process_Id);
   --  Take Process off its queue, if it is on it.

   procedure Resume_Current_Process;

   procedure Quantum_Finished (Process : Rose.Kernel.Processes.Process_Id);

end Rose.Kernel.Processes.Queue;
