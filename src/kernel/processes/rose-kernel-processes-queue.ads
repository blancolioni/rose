package Rose.Kernel.Processes.Queue is

   procedure Choose_Process;

   procedure Queue_Process
     (Process : Rose.Objects.Process_Id);

   procedure Dequeue_Process
     (Process : Rose.Objects.Process_Id);

end Rose.Kernel.Processes.Queue;
