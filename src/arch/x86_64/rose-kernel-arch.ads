with Rose.Words;

package Rose.Kernel.Arch is

   type Registers is
      record
         EAX, EBX, ECX, EDX : Rose.Words.Word_32;
         ESI, EDI           : Rose.Words.Word_32;
         EBP, ESP, EIP      : Rose.Words.Word_32;
         EFlags             : Rose.Words.Word_32;
      end record;

end Rose.Kernel.Arch;
