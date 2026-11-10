with Rose.Capabilities;

package Derived_Holder is

   --  Hold two derived capabilities across a restore, and check them the
   --  first time the process is restored (#691, ADR 0001).

   procedure Create_Server
     (Parent : Rose.Capabilities.Capability;
      Child  : Rose.Capabilities.Capability);
   --  Hold Parent, a capability derived from the command registry's
   --  Commands interface, and Child, derived from Parent, and serve
   --  Restorable, so that the kernel says when the process is restored.

   procedure Start_Server;
   --  Serve. It does not return.

end Derived_Holder;
