with Rose.Capabilities;
with Rose.Kernel.Interrupts;
with Rose.Words;

package Rose.Kernel.Clock is

   function Handle_Clock_Tick
     return Rose.Kernel.Interrupts.Interrupt_Handler_Status;

   procedure Update_Mem
     (Allocated, Available : Rose.Addresses.Physical_Bytes;
      Dirty_Pages          : Natural;
      Store_Reads          : Natural);

   procedure Set_Timeout
     (Timeout : Rose.Words.Word;
      Object  : Rose.Objects.Object_Id;
      Cap     : Rose.Capabilities.Capability);

   function Current_Ticks return Rose.Words.Word;
   --  The low 32 bits of the tick count, which wrap after about 16 months.

   procedure Current_Ticks_64 (Low, High : out Rose.Words.Word);
   --  The tick count as two 32-bit halves. The count starts at zero at
   --  every boot, and the halves are read together.

end Rose.Kernel.Clock;
