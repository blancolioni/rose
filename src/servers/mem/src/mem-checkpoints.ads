with System;

package Mem.Checkpoints is

   procedure Checkpoint
     (Append : not null access
        procedure (Buffer : System.Address));

end Mem.Checkpoints;
