package Rose.Words is

   pragma Pure (Rose.Words);

   type Word_4 is mod 2 ** 4;
   for Word_4'Size use 4;

   type Word_8 is mod 2 ** 8;
   for Word_8'Size use 8;

   type Word_16 is mod 2**16;
   for Word_16'Size use 16;

   type Word_32 is mod 2**32;
   for Word_32'Size use 32;

   type Word_64 is mod 2**64;
   for Word_64'Size use 64;

   subtype Word is Word_32;

end Rose.Words;
