with System.Storage_Elements;

package Rose.Kernel.Processes.Snapshot is

   procedure Write_Tables
     (Storage    : out System.Storage_Elements.Storage_Array;
      Page_Count : out Natural;
      Success    : out Boolean);
   --  Write the kernel's tables for every persistent process into
   --  Storage, in the format described in Rose.Checkpoints, and set
   --  Page_Count to the number of pages used.  Success is False, and
   --  nothing is written, if Storage is too small; it is also False if
   --  more processes are waiting to send to one process than its record
   --  can hold.

end Rose.Kernel.Processes.Snapshot;
