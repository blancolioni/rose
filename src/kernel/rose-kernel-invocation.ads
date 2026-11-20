with Rose.Invocation;
with Rose.Words;

package Rose.Kernel.Invocation is

   procedure Invoke_Capability
     (Params : Rose.Invocation.Invocation_Access);

   pragma Export (C, Invoke_Capability, "invoke_capability");

   --  The system call entry copies the caller's invocation record into
   --  the kernel from the address the caller gives.  It checks the
   --  address first, and if the record isn't all in mapped user memory,
   --  faults the caller instead of copying it (#306).

   function Is_Valid_Invocation_Record
     (Address : Rose.Words.Word_32)
      return Rose.Words.Word_32;
   --  1 if the whole record at Address is in user memory that the
   --  current process has mapped, 0 if not. The page tables are not read
   --  again for a record at the same address as the last one that passed,
   --  if no page table has changed since (#717).

   pragma Export
     (C, Is_Valid_Invocation_Record, "is_valid_invocation_record");

   procedure Fault_Invocation_Record
     (Address : Rose.Words.Word_32);
   --  Fault the current process for the record at Address, and choose
   --  the next process to run.

   pragma Export (C, Fault_Invocation_Record, "fault_invocation_record");

   procedure Poison_Unsent
     (Params : in out Rose.Invocation.Invocation_Record);
   --  Fill each data word, cap and buffer field of Params that its flags
   --  and counts say wasn't sent: data word N with BAD0_000N, cap N with
   --  BAD0_0C0N, which is never a valid cap, and the buffer with an
   --  address in the kernel and a length that no buffer has. Used when
   --  Poison_Invocations is set (#892).

end Rose.Kernel.Invocation;
