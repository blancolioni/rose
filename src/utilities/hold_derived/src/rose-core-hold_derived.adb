with Derived_Holder;

procedure Rose.Core.Hold_Derived
  (Parent : in out Rose.Interfaces.Commands.Lookup.Lookup_Object'Class;
   Child  : in out Rose.Interfaces.Commands.Lookup.Lookup_Object'Class)
is
begin
   --  The generated main has given Rescind_Cap to Use_Capabilities and
   --  Standard_Output to Ada.Text_IO. The launch call completes, and
   --  then it runs Derived_Holder.Start_Server, which never returns.

   Derived_Holder.Create_Server
     (Parent => Parent.Lookup_Cap,
      Child  => Child.Lookup_Cap);
end Rose.Core.Hold_Derived;
