with Ada.IO_Exceptions;

package body Rose.Streams is

   use Ada.Streams;

   Page_Length : constant := 4096;
   --  The most that one read or write carries.

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Reader_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      Last := Item'First - 1;

      --  Each read goes into a buffer of its own that starts at 1, so
      --  that its Last is the number of elements read, whatever Item's
      --  bounds are.

      while Last < Item'Last loop
         declare
            Buffer : Stream_Element_Array
              (1 .. Stream_Element_Offset'Min
                      (Page_Length, Item'Last - Last));
            Count  : Stream_Element_Offset;
         begin
            Stream.Reader.Read (Buffer, Count);
            exit when Count < Buffer'First;
            Item (Last + 1 .. Last + Count) := Buffer (1 .. Count);
            Last := Last + Count;
         end;
      end loop;
   end Read;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Writer_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      pragma Unreferenced (Stream, Item, Last);
   begin
      raise Ada.IO_Exceptions.Mode_Error with "a writer cannot be read";
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Reader_Stream;
      Item   : Stream_Element_Array)
   is
      pragma Unreferenced (Stream, Item);
   begin
      raise Ada.IO_Exceptions.Mode_Error with "a reader cannot be written";
   end Write;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Writer_Stream;
      Item   : Stream_Element_Array)
   is
      First : Stream_Element_Offset := Item'First;
   begin
      while First <= Item'Last loop
         declare
            Last : constant Stream_Element_Offset :=
              Stream_Element_Offset'Min (Item'Last, First + Page_Length - 1);
         begin
            Stream.Writer.Write (Item (First .. Last));
            First := Last + 1;
         end;
      end loop;
   end Write;

end Rose.Streams;
