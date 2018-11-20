with Rose.Capabilities.Layout;
with Rose.Invocation;
with Rose.Objects;

package Rose.Kernel.Debug is

   procedure Put_Cap_Type
     (Cap_Type : Rose.Capabilities.Layout.Capability_Type);

   procedure Put_Call
     (Name   : String;
      Pid    : Rose.Objects.Process_Id;
      Layout : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Record);

end Rose.Kernel.Debug;
