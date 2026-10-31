package Tests.Signed is

   function Signed_Test return Boolean;
   --  Signed IDL types (blancolioni/idle#68): Integer_32 and Integer_64
   --  go into an invocation as their two's complement words and come out
   --  as the same values, at the ends of their ranges and either side of
   --  zero.

end Tests.Signed;
