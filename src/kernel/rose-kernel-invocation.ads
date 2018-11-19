with Rose.Invocation;

package Rose.Kernel.Invocation is

   procedure Invoke_Capability
     (Params : Rose.Invocation.Invocation_Access);

   pragma Export (C, Invoke_Capability, "invoke_capability");

end Rose.Kernel.Invocation;
