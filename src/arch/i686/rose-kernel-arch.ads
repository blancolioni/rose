with Rose.Words;                       use Rose.Words;

package Rose.Kernel.Arch is

   type Stack_Frame is
      record
         GS  : Word_16;
         FS  : Word_16;
         ES  : Word_16;
         DS  : Word_16;
         EDI : Word_32;
         ESI : Word_32;
         EBP : Word_32;
         XSP : Word_32;
         EBX : Word_32;
         EDX : Word_32;
         ECX : Word_32;
         EAX : Word_32;
         EIP : Word_32;      --  last register pushed by hardware
         CS  : Word_32;
         PSW : Word_32;
         ESP : Word_32;
         SS  : Word_32;
      end record
   with Pack, Size => 15 * 32;

   function Process_Stack_Frame
     (Start_EIP       : Word_32;
      Start_ESP       : Word_32 := Process_Stack_Bound;
      Start_PSW       : Word_32 := 16#0000_0200#;
      Code_Segment    : Word_32 := 16#18#;
      Data_Segment    : Word_32 := 16#20#;
      Privilege_Level : Word_32 := 3)
      return Stack_Frame;

end Rose.Kernel.Arch;
