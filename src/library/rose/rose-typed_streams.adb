with Ada.Streams;
with Interfaces;

package body Rose.Typed_Streams is

   use System.Storage_Elements;

   package Unsigned renames Standard.Interfaces;
   use type Unsigned.Unsigned_32;
   use type Unsigned.Unsigned_64;

   Format_Word   : constant Unsigned.Unsigned_32 := 16#3153_4C52#;
   --  "RLS1" in its bytes, little-endian.

   Header_Length : constant Storage_Count := 16;
   --  The format word, the type id and the table's length.

   function Get_Word
     (Buffer : Storage_Array;
      Index  : Storage_Offset)
      return Unsigned.Unsigned_32;
   --  The little-endian word at Buffer (Index .. Index + 3).

   procedure Put_Word
     (Buffer : in out Storage_Array;
      Last   : in out Storage_Offset;
      Value  : Unsigned.Unsigned_32);

   function Fill
     (Item  : in out Reader;
      Count : Storage_Count)
      return Boolean;
   --  Read from Source until Count bytes are unread, or Source ends, and
   --  return True if they are. Count is at most Capacity.

   procedure Read_Into
     (Item   : in out Reader;
      Buffer : in out Storage_Array);
   --  Fill all of Buffer from Source, without Item's buffer, which is
   --  empty. Raises Format_Error if Source ends first.

   procedure Send
     (Item : in out Writer;
      Data : Storage_Array);
   --  Write Data to Target.

   procedure Start (Item : in out Writer);
   procedure Start (Item : in out Reader);
   --  Write the header, or read and check it.

   -----------------
   -- End_Of_List --
   -----------------

   function End_Of_List (Item : in out Reader) return Boolean is
   begin
      Start (Item);
      return not Fill (Item, 1);
   end End_Of_List;

   ----------
   -- Fill --
   ----------

   function Fill
     (Item  : in out Reader;
      Count : Storage_Count)
      return Boolean
   is
   begin
      if Item.Last - Item.First + 1 >= Count then
         return True;
      end if;

      --  Move what is unread to the start, to make room after it.

      if Item.First > 1 then
         declare
            Unread : constant Storage_Count := Item.Last - Item.First + 1;
         begin
            Item.Buffer (1 .. Unread) := Item.Buffer (Item.First .. Item.Last);
            Item.First := 1;
            Item.Last := Unread;
         end;
      end if;

      while not Item.Ended and then Item.Last < Count loop
         declare
            use Ada.Streams;
            Room : Stream_Element_Array
              (1 .. Stream_Element_Offset (Item.Capacity - Item.Last));
            Got  : Stream_Element_Offset;
         begin
            Item.Source.Read (Room, Got);
            if Got < Room'First then
               Item.Ended := True;
            else
               for Element of Room (Room'First .. Got) loop
                  Item.Last := Item.Last + 1;
                  Item.Buffer (Item.Last) := Storage_Element (Element);
               end loop;
            end if;
         end;
      end loop;

      return Item.Last - Item.First + 1 >= Count;
   end Fill;

   -----------
   -- Flush --
   -----------

   procedure Flush (Item : in out Writer) is
   begin
      Start (Item);
      if Item.Last > 0 then
         Send (Item, Item.Buffer (1 .. Item.Last));
         Item.Last := 0;
      end if;
   end Flush;

   ---------
   -- Get --
   ---------

   procedure Get
     (Item  : in out Reader;
      Value : out Element)
   is
      Length : Storage_Count;
   begin
      Start (Item);
      if not Fill (Item, 4) then
         raise Format_Error with
           (if Item.First > Item.Last then "end of list"
            else "record cut short");
      end if;

      Length :=
        Storage_Count (Get_Word (Item.Buffer, Item.First) and 16#FF_FFFF#);
      if Length < 4 then
         raise Format_Error with "record shorter than its header";
      end if;

      if Length <= Item.Capacity then
         if not Fill (Item, Length) then
            raise Format_Error with "record cut short";
         end if;

         declare
            Last : Storage_Offset := Item.First - 1;
         begin
            Decode
              (Item.Buffer (Item.First .. Item.First + Length - 1),
               Last, Value);
         exception
            when Constraint_Error =>
               raise Format_Error with "record does not decode";
         end;
         Item.First := Item.First + Length;

      else
         --  A record longer than the buffer: what is buffered, and the
         --  rest straight from Source.

         declare
            Whole    : Storage_Array (1 .. Length);
            Buffered : constant Storage_Count := Item.Last - Item.First + 1;
            Last     : Storage_Offset := 0;
         begin
            Whole (1 .. Buffered) := Item.Buffer (Item.First .. Item.Last);
            Item.First := 1;
            Item.Last := 0;
            Read_Into (Item, Whole (Buffered + 1 .. Length));
            Decode (Whole, Last, Value);
         exception
            when Constraint_Error =>
               raise Format_Error with "record does not decode";
         end;
      end if;
   end Get;

   --------------
   -- Get_Word --
   --------------

   function Get_Word
     (Buffer : Storage_Array;
      Index  : Storage_Offset)
      return Unsigned.Unsigned_32
   is
      Result : Unsigned.Unsigned_32 := 0;
   begin
      for Offset in reverse Storage_Offset range 0 .. 3 loop
         Result := Unsigned.Shift_Left (Result, 8)
           or Unsigned.Unsigned_32 (Buffer (Index + Offset));
      end loop;
      return Result;
   end Get_Word;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item  : in out Writer;
      Value : Element)
   is
      Length : constant Storage_Count := Encoded_Length (Value);
   begin
      Start (Item);
      if Length > Item.Capacity - Item.Last then
         Flush (Item);
      end if;

      if Length > Item.Capacity then
         declare
            Whole : Storage_Array (1 .. Length);
            Last  : Storage_Offset := 0;
         begin
            Encode (Value, Whole, Last);
            Send (Item, Whole);
         end;
      else
         Encode (Value, Item.Buffer, Item.Last);
      end if;
   end Put;

   --------------
   -- Put_Word --
   --------------

   procedure Put_Word
     (Buffer : in out Storage_Array;
      Last   : in out Storage_Offset;
      Value  : Unsigned.Unsigned_32)
   is
   begin
      for Shift in 0 .. 3 loop
         Last := Last + 1;
         Buffer (Last) :=
           Storage_Element
             (Unsigned.Shift_Right (Value, 8 * Shift) and 16#FF#);
      end loop;
   end Put_Word;

   ---------------
   -- Read_Into --
   ---------------

   procedure Read_Into
     (Item   : in out Reader;
      Buffer : in out Storage_Array)
   is
      Last : Storage_Offset := Buffer'First - 1;
   begin
      while Last < Buffer'Last loop
         if Item.Ended then
            raise Format_Error with "record cut short";
         end if;

         declare
            use Ada.Streams;
            Room : Stream_Element_Array
              (1 .. Stream_Element_Offset (Buffer'Last - Last));
            Got  : Stream_Element_Offset;
         begin
            Item.Source.Read (Room, Got);
            if Got < Room'First then
               Item.Ended := True;
            else
               for Element of Room (Room'First .. Got) loop
                  Last := Last + 1;
                  Buffer (Last) := Storage_Element (Element);
               end loop;
            end if;
         end;
      end loop;
   end Read_Into;

   ----------
   -- Send --
   ----------

   procedure Send
     (Item : in out Writer;
      Data : Storage_Array)
   is
      use Ada.Streams;
      Bytes : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length));
      Index : Stream_Element_Offset := 0;
   begin
      for Element of Data loop
         Index := Index + 1;
         Bytes (Index) := Stream_Element (Element);
      end loop;
      Item.Target.Write (Bytes);
   end Send;

   -----------
   -- Start --
   -----------

   procedure Start (Item : in out Writer) is
   begin
      if Item.Started then
         return;
      end if;
      Item.Started := True;

      declare
         Table  : constant Storage_Array := Fields;
         Id     : constant Unsigned.Unsigned_64 :=
           Unsigned.Unsigned_64 (Type_Id);
         Header : Storage_Array (1 .. Header_Length + Table'Length);
         Last   : Storage_Offset := 0;
      begin
         Put_Word (Header, Last, Format_Word);
         Put_Word (Header, Last,
                   Unsigned.Unsigned_32 (Id and 16#FFFF_FFFF#));
         Put_Word (Header, Last,
                   Unsigned.Unsigned_32 (Unsigned.Shift_Right (Id, 32)));
         Put_Word (Header, Last, Unsigned.Unsigned_32 (Table'Length));
         Header (Last + 1 .. Header'Last) := Table;

         if Header'Length <= Item.Capacity - Item.Last then
            Item.Buffer (Item.Last + 1 .. Item.Last + Header'Length) :=
              Header;
            Item.Last := Item.Last + Header'Length;
         else
            Send (Item, Header);
         end if;
      end;
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start (Item : in out Reader) is
      Table_Length : Storage_Count;
   begin
      if Item.Started then
         return;
      end if;
      Item.Started := True;

      if not Fill (Item, Header_Length) then
         if Item.First > Item.Last then
            --  An empty stream is an empty list.
            return;
         end if;
         raise Format_Error with "header cut short";
      end if;

      if Get_Word (Item.Buffer, Item.First) /= Format_Word then
         raise Format_Error with "not a list";
      end if;

      declare
         Id : constant Unsigned.Unsigned_64 :=
           Unsigned.Unsigned_64 (Get_Word (Item.Buffer, Item.First + 4))
           or Unsigned.Shift_Left
                (Unsigned.Unsigned_64
                   (Get_Word (Item.Buffer, Item.First + 8)), 32);
      begin
         if Id /= Unsigned.Unsigned_64 (Type_Id) then
            raise Format_Error with "a list of another record";
         end if;
      end;

      Table_Length :=
        Storage_Count (Get_Word (Item.Buffer, Item.First + 12));
      Item.First := Item.First + Header_Length;

      --  The table isn't needed to decode a list of Element, whose codec
      --  copes with fields added or missing, so it is skipped.

      while Table_Length > 0 loop
         declare
            Part : constant Storage_Count :=
              Storage_Count'Min (Table_Length, Item.Capacity);
         begin
            if not Fill (Item, Part) then
               raise Format_Error with "field table cut short";
            end if;
            Item.First := Item.First + Part;
            Table_Length := Table_Length - Part;
         end;
      end loop;
   end Start;

end Rose.Typed_Streams;
