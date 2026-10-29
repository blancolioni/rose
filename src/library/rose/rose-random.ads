with System.Storage_Elements;

package Rose.Random is

   --  Random bytes that are different each time, for salts (#455). They
   --  come from RDRAND if the processor has it, and otherwise from the
   --  timestamp counter, mixed. They are not good enough for secrets:
   --  #476 is for a source that is.

   procedure Fill (Item : out System.Storage_Elements.Storage_Array);

end Rose.Random;
