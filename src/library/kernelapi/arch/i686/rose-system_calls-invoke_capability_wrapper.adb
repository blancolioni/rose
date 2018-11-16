with System.Machine_Code;               use System.Machine_Code;

separate (Rose.System_Calls)
procedure Invoke_Capability_Wrapper
  (Item : aliased in out Rose.Invocation.Invocation_Record)
is
   pragma Unreferenced (Item);
begin
   Asm ("jmp 0xf0000000", Volatile => True);
end Invoke_Capability_Wrapper;
