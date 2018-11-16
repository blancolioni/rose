with Rose.Invocation;

package Rose.Kernel.Invocation is

   procedure Invoke_Capability
     (Params : Rose.Invocation.Invocation_Access);

   pragma Export (C, Invoke_Capability, "invoke_capability");

   procedure Handle_Page_Fault
     (Address              : Rose.Addresses.Virtual_Address;
      User_Mode            : Boolean;
      Protection_Violation : Boolean;
      Write_Attempt        : Boolean;
      Execution_Attempt    : Boolean);

   function Page_Fault_Count
     return Natural;

end Rose.Kernel.Invocation;
