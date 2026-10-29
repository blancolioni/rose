package Tests.Invalid_Caps is

   function Invalid_Cap_Test return Boolean;
   --  Invoking a cap that the process does not hold (#239): cap zero, the
   --  null cap, fails with Invalid_Capability, and the kernel carries on.

   function No_Buffer_Test return Boolean;
   --  Sending text or a storage array, or receiving into the buffer,
   --  without an invoke buffer fails with No_Invoke_Buffer (#239). The
   --  test takes the buffer away and puts it back before it returns.

end Tests.Invalid_Caps;
