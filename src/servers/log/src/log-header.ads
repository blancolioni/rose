with System.Storage_Elements;

private package Log.Header is

   Log_Magic   : constant Rose.Words.Word_64 := 16#8F8F_FFE0_FA39_FDF7#;
   Entry_Magic : constant Rose.Words.Word_64 := 16#7E75_3634_A356_8547#;

   subtype Log_Reserved is
     System.Storage_Elements.Storage_Array (1 .. 4064);

   --  Reset increments Generation, and an entry belongs to the log only
   --  if its own Generation matches, so that entries written before a
   --  reset are not found again.  A log header written before version 2
   --  has zeroes here, so it reads as generation 0.

   --  The log starts at First_Entry, whose entry has sequence number
   --  First_Sequence; each entry after it has the next number (#951).
   --  A header written before version 3 has zeroes in First_Sequence,
   --  and its entries are of an older version, so its log is empty.
   --  The two are in the header's last sector, so that a write of the
   --  header that a crash cuts short can't change one without the
   --  other.

   type Log_Header_Type is
      record
         Magic          : Rose.Words.Word_64 := Log_Magic;
         Generation     : Rose.Words.Word_64 := 0;
         Reserved       : Log_Reserved := (others => 0);
         First_Sequence : Rose.Words.Word_64 := 1;
         First_Entry    : Log_Location_Id := 1;
      end record
     with Pack, Size => Log_Page_Bits;

   subtype Entry_Reserved is
     System.Storage_Elements.Storage_Array (1 .. 4048);

   --  An entry's pages follow its header, and wrap around from the end
   --  of the log to location 1, so Log_Last can be before the header.
   --  Sequence is one more than the previous entry's: a header from an
   --  earlier lap names the right location and generation, but not the
   --  right sequence number.

   type Entry_Flags is new Rose.Words.Word_32;

   Is_Active_Entry : constant Entry_Flags := 1;

   type Entry_Header_Type is
      record
         Magic     : Rose.Words.Word_64 := Entry_Magic;
         Version   : Rose.Words.Word_32 := Log_Version;
         Flags     : Entry_Flags := 0;
         Generation : Rose.Words.Word_64 := 0;
         Log_First : Log_Location_Id;
         Log_Last  : Log_Location_Id;
         Sequence  : Rose.Words.Word_64 := 0;
         Reserved  : Entry_Reserved := (others => 0);
      end record
     with Pack, Size => Log_Page_Bits;

end Log.Header;
