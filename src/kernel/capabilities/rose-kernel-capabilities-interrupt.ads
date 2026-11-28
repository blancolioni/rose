with Rose.Init_Capabilities;

package Rose.Kernel.Capabilities.Interrupt is

   Reserve_Interrupt_Endpoint : constant := 1;

   Acknowledge_Endpoint       : constant := 3;
   --  Unmask the cap's line, which the kernel masks when it sends an
   --  interrupt from a level-triggered line (#295). Reserve replies with
   --  a cap for it, if the caller receives caps.

   Derive_Endpoint            : constant :=
                                  Rose.Init_Capabilities
                                    .Derive_Interrupt_Endpoint;
   --  Only init's Interrupts_Cap has this endpoint
   --  (Rose.Init_Capabilities).  It replies with a cap that reserves one
   --  vector in its own range.

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Interrupt;
