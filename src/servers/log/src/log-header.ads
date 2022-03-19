with System.Storage_Elements;

private package Log.Header is

   Log_Magic   : constant Rose.Words.Word_64 := 16#8F8F_FFE0_FA39_FDF7#;
   Entry_Magic : constant Rose.Words.Word_64 := 16#7E75_3634_A356_8547#;

   subtype Log_Reserved is
     System.Storage_Elements.Storage_Array (1 .. 4080);

   type Log_Header_Type is
      record
         Magic    : Rose.Words.Word_64 := Log_Magic;
         Reserved : Log_Reserved := (others => 0);
         First_Entry : Log_Location_Id := 1;
      end record
     with Pack, Size => Log_Page_Bits;

   subtype Entry_Reserved is
     System.Storage_Elements.Storage_Array (1 .. 4064);

   type Entry_Flags is new Rose.Words.Word_32;

   Is_Active_Entry : constant Entry_Flags := 1;

   type Entry_Header_Type is
      record
         Magic     : Rose.Words.Word_64 := Entry_Magic;
         Version   : Rose.Words.Word_32 := Log_Version;
         Flags     : Entry_Flags := 0;
         Log_First : Log_Location_Id;
         Log_Last  : Log_Location_Id;
         Reserved  : Entry_Reserved;
      end record
     with Pack, Size => Log_Page_Bits;

end Log.Header;
