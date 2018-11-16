with Rose.Arch.Interrupts;
with Rose.Boot.Console;

package body Rose.Kernel.Errors is

   -----------
   -- Fatal --
   -----------

   procedure Fatal (Message : String;
                    Code    : Word_32)
   is
   begin
      Rose.Boot.Console.New_Line;
      Rose.Boot.Console.Put ("Fatal: ");
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put (Code);
      Rose.Boot.Console.New_Line;
      Rose.Arch.Halt;
   end Fatal;

   -----------
   -- Fatal --
   -----------

   procedure Fatal (Message : String;
                    Mem1    : Physical_Address;
                    Mem2    : Physical_Address)
   is
   begin
      Rose.Boot.Console.Put ("Fatal: ");
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put (Word_32 (Mem1));
      Rose.Boot.Console.Put ("  ");
      Rose.Boot.Console.Put (Word_32 (Mem2));
      Rose.Boot.Console.New_Line;
      Rose.Arch.Halt;
   end Fatal;

   ---------------------------
   -- Handle_Divide_By_Zero --
   ---------------------------

   procedure Handle_Divide_By_Zero
     (IP : Rose.Words.Word)
   is
   begin
      Fatal ("divide by zero exception", IP);
   end Handle_Divide_By_Zero;

   -------------------------
   -- Handle_Double_Fault --
   -------------------------

   procedure Handle_Double_Fault is
   begin
      Fatal ("double fault exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Double_Fault));
   end Handle_Double_Fault;

   ---------------------------
   -- Handle_Invalid_Opcode --
   ---------------------------

   procedure Handle_Invalid_Opcode is
   begin
      Fatal ("invalid opcode exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Invalid_Opcode));
   end Handle_Invalid_Opcode;

   ------------------------
   -- Handle_Invalid_TSS --
   ------------------------

   procedure Handle_Invalid_TSS is
   begin
      Fatal ("invalid TSS exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Invalid_TSS));
   end Handle_Invalid_TSS;

   ---------------------
   -- Handle_Overflow --
   ---------------------

   procedure Handle_Overflow is
   begin
      Fatal ("overflow exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Overflow));
   end Handle_Overflow;

   --------------------------------
   -- Handle_Segment_Not_Present --
   --------------------------------

   procedure Handle_Segment_Not_Present is
   begin
      Fatal ("segment not present exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Segment_Not_Present));
   end Handle_Segment_Not_Present;

   --------------------------------
   -- Handle_Stack_Segment_Fault --
   --------------------------------

   procedure Handle_Stack_Segment_Fault is
   begin
      Fatal ("stack segment fault exception",
             Rose.Words.Word_32 (Rose.Arch.Interrupts.Stack_Segment_Fault));
   end Handle_Stack_Segment_Fault;

   -------------
   -- Warning --
   -------------

   procedure Warning (Source  : String;
                      Message : String;
                      Code    : Word_32)
   is
   begin
      Rose.Boot.Console.Put (Source);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put (Message);
      Rose.Boot.Console.Put (": ");
      Rose.Boot.Console.Put (Code);
      Rose.Boot.Console.New_Line;
   end Warning;

end Rose.Kernel.Errors;
