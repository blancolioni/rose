with System.Machine_Code;               use System.Machine_Code;

separate (Rose.System_Calls)
procedure Invoke_Capability
  (Item : aliased in out Rose.Invocation.Invocation_Record)
is
   pragma Unreferenced (Item);
begin
   Asm ("jmp 0xffff0000", Volatile => True);
end Invoke_Capability;
