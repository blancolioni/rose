with Rose.Words;

package Rose.Kernel.Panic is

   procedure Panic (Message : String);

   procedure Panic (Message : String;
                    Value   : Rose.Words.Word);

   procedure Panic (Message : String;
                    Addr    : Physical_Address);

   procedure Panic (Message : String;
                    Addr    : Virtual_Address);

   pragma No_Return (Panic);

end Rose.Kernel.Panic;
