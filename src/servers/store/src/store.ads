with Rose.Capabilities;

package Store is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap         : Rose.Capabilities.Capability;
   --  The boot console, which the store no longer writes to (#1212). It
   --  stays the first cap of its set, so that the others keep their
   --  places.
   Kernel_Log_Cap      : Rose.Capabilities.Capability;
   --  Where the store writes its messages: the kernel's log, on serial.

end Store;
