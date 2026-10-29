package Tests.Invalid_Caps is

   function Invalid_Cap_Test return Boolean;
   --  Invoking a cap that the process does not hold (#239): cap zero, the
   --  null cap, fails with Invalid_Capability, and the kernel carries on.

end Tests.Invalid_Caps;
