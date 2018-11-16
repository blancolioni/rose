with Rose.Invocation;
with Rose.System_Calls;

package body Rose.Devices.Kernel is

   -------------------------------
   -- Enable_Invocation_Logging --
   -------------------------------

   procedure Enable_Invocation_Logging
     (Kernel_Cap : Rose.Capabilities.Capability;
      Enabled    : Boolean)
   is
      use all type Rose.Invocation.Invocation_Flag;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialise_Send
        (Invocation => Params,
         Flags      => (1 => Send),
         Cap        => Kernel_Cap,
         Endpoint   => 29,
         Send_Words => (1 => Boolean'Pos (Enabled)));

      Rose.System_Calls.Invoke_Capability (Params);
   end Enable_Invocation_Logging;

end Rose.Devices.Kernel;
