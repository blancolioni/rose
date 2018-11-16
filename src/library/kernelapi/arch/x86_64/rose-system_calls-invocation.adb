with System.Machine_Code;               use System.Machine_Code;

separate (Rose.System_Calls)
procedure Invoke_Capability (Item : aliased in out Invocation_Record) is
   type IR_Access is access all Invocation_Record;
   IR : constant IR_Access := Item'Access;
begin
   Asm ("pushl %%ecx", Volatile => True);
   Asm ("movl %0, %%esp",
        Inputs => IR_Access'Asm_Input ("g", IR),
        Volatile => True);
   Asm ("movl $1f, %%edx", Volatile => True);
   Asm ("1:int $0x30", Volatile => True);
   Asm ("popl %%ecx", Volatile => True);

end Invoke_Capability;
