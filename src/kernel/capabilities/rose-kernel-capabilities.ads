with Rose.Capabilities.Layout;
with Rose.Invocation;

package Rose.Kernel.Capabilities is

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Generic_Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities;
