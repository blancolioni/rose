with System.Machine_Code;               use System.Machine_Code;

separate (Rose.System_Calls)
procedure Enter_Kernel
  (Item : aliased in out Rose.Invocation.Invocation_Record)
is
   pragma Unreferenced (Item);
begin
   Asm ("jmp 0xffff0000", Volatile => True);
end Enter_Kernel;
