with Rose.Addresses;                   use Rose.Addresses;
with Rose.Words;                       use Rose.Words;

package Rose.Arch is

   procedure Outb (Port  : Word_16;
                   Value : Word_8);

   function Inb (Port : Word_16)
                return Word_8;

   procedure Halt;

   procedure Copy_Memory (From   : Physical_Address;
                          To     : Physical_Address;
                          Length : Physical_Address);

end Rose.Arch;
