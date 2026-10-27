package Rose.Interfaces.Objects is

   --  For the user-mode IDL target (see "User-mode target" in the IDL
   --  chapter of the Rose book).

   pragma Pure (Rose.Interfaces.Objects);

   Invocation_Error : exception;
   --  Raised by a generated client when a call through it fails.

   Not_Served : exception;
   --  Raised when an object of the program that is not attached to an
   --  interface is passed to another process as that interface.

end Rose.Interfaces.Objects;
