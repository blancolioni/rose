with Rose.Boot.Console;

with Rose.Kernel.Processes;

package body Rose.Kernel.Checkpoint is

   ----------------------
   -- Enter_Checkpoint --
   ----------------------

   procedure Enter_Checkpoint is
   begin
      Rose.Boot.Console.Checkpoint_Status (True);
      Rose.Boot.Console.Put_Line ("Entering checkpoint");
      Rose.Kernel.Processes.Enter_Checkpoint;
      Rose.Boot.Console.Put_Line ("Checkpoint mode ready");
   end Enter_Checkpoint;

   ----------------------
   -- Leave_Checkpoint --
   ----------------------

   procedure Leave_Checkpoint is
   begin
      Rose.Boot.Console.Put_Line ("Leaving checkpoint");
      Rose.Kernel.Processes.Leave_Checkpoint;
      Rose.Boot.Console.Checkpoint_Status (False);
   end Leave_Checkpoint;

end Rose.Kernel.Checkpoint;
