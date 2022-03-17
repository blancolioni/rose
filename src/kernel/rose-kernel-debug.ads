with Rose.Capabilities.Layout;
with Rose.Invocation;

package Rose.Kernel.Debug is

   procedure Put_Cap_Type
     (Cap_Type : Rose.Capabilities.Layout.Capability_Type);

   procedure Put_Cap_Layout
     (Name   : String;
      Cap    : Rose.Capabilities.Capability;
      Layout : Rose.Capabilities.Layout.Capability_Layout);

   procedure Put_Call
     (Name   : String;
      Layout : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Record);

end Rose.Kernel.Debug;
