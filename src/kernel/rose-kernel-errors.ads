with Rose.Words;                       use Rose.Words;

package Rose.Kernel.Errors is

   procedure Warning (Source  : String;
                      Message : String;
                      Code    : Word_32);

   procedure Fatal (Message : String;
                    Code    : Word_32);

   procedure Fatal (Message : String;
                    Mem1    : Rose.Addresses.Physical_Address;
                    Mem2    : Rose.Addresses.Physical_Address);

   procedure Handle_Divide_By_Zero
     (IP : Rose.Words.Word);

   procedure Handle_Overflow;

   procedure Handle_Invalid_Opcode;

   procedure Handle_Invalid_TSS;

   procedure Handle_Segment_Not_Present;

   procedure Handle_Stack_Segment_Fault;

   procedure Handle_Double_Fault;

end Rose.Kernel.Errors;
