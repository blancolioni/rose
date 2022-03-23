with Rose.Capabilities;
with Rose.Kernel.Interrupts;
with Rose.Words;

package Rose.Kernel.Clock is

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Update_Mem
     (Allocated, Available : Rose.Addresses.Physical_Bytes;
      Dirty_Pages          : Natural);

   procedure Set_Timeout
     (Timeout : Rose.Words.Word;
      Object  : Rose.Objects.Object_Id;
      Cap     : Rose.Capabilities.Capability);

   function Current_Ticks return Rose.Words.Word;

end Rose.Kernel.Clock;
