package body Rose.Command_Line.Items
  with SPARK_Mode
is

   subtype Wide is Long_Long_Integer;
   --  The sizes and offsets here are added and multiplied while they are
   --  still being checked against the buffer, so they are 64 bits: a
   --  length can be any 32-bit value.

   function Byte
     (Buffer   : System.Storage_Elements.Storage_Array;
      Position : Wide)
      return Wide
   with Pre => Buffer'First = 1
               and then Buffer'Last in 0 ..
                 System.Storage_Elements.Storage_Offset (Natural'Last)
               and then Position in 0 .. Wide (Buffer'Last) - 1;
   --  The byte at Position bytes from the start of the buffer.

   ----------
   -- Byte --
   ----------

   function Byte
     (Buffer   : System.Storage_Elements.Storage_Array;
      Position : Wide)
      return Wide
   is (Wide (Buffer (1 + System.Storage_Elements.Storage_Offset (Position))));

   -------------
   -- Element --
   -------------

   function Element
     (Buffer     : System.Storage_Elements.Storage_Array;
      First_Item : Positive;
      Count      : Natural;
      Index      : Positive)
      return String
   is
      Size    : constant Wide := Wide (Buffer'Last);
      Item    : constant Wide := Wide (First_Item) + Wide (Index) - 1;
      Offset  : Wide := 0;
      --  The bytes from the start of the buffer to the item Current.
      Current : Wide := 1;
      Length  : Wide := 0;

   begin
      if Index > Count then
         return "";
      end if;

      --  An item that is not all in the buffer is taken to be empty.

      while Current <= Item loop
         pragma Loop_Variant (Increases => Current);
         pragma Loop_Invariant (Offset in 0 .. Size + 3);
         pragma Loop_Invariant (Current in 1 .. Item);

         if Offset + 4 > Size then
            return "";
         end if;

         Length := Byte (Buffer, Offset)
           + Byte (Buffer, Offset + 1) * 2 ** 8
           + Byte (Buffer, Offset + 2) * 2 ** 16
           + Byte (Buffer, Offset + 3) * 2 ** 24;

         if Length > Size - Offset - 4 then
            return "";
         end if;

         exit when Current = Item;
         Offset := Offset + 4 + (Length + 3) / 4 * 4;
         Current := Current + 1;
      end loop;

      declare
         Text_Length : constant Natural := Natural (Length);
         Text        : String (1 .. Text_Length) := (others => ' ');
      begin
         for I in Text'Range loop
            Text (I) := Character'Val (Byte (Buffer, Offset + 3 + Wide (I)));
         end loop;

         return Text;
      end;
   end Element;

end Rose.Command_Line.Items;
