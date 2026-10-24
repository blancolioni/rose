with Rose.Words;

package body Rose.Command_Line is

   use System.Storage_Elements;

   -------------
   -- Element --
   -------------

   function Element
     (List  : String_List;
      Index : Positive)
      return String
   is
      Item   : constant Positive := List.First_Item + Index - 1;
      Offset : Storage_Offset := 0;
      Length : Storage_Count := 0;
   begin
      if Index > List.Count then
         return "";
      end if;

      --  Each item is a 32-bit length, the characters, and padding to a
      --  multiple of four bytes. An item that is not all in the buffer is
      --  taken to be empty.

      for Current in 1 .. Item loop
         if Offset + 4 > List.Buffer_Length then
            return "";
         end if;

         declare
            Length_Word : Rose.Words.Word_32
              with Import, Address => List.Buffer + Offset;
         begin
            Length := Storage_Count (Length_Word);
         end;

         if Offset + 4 + Length > List.Buffer_Length then
            return "";
         end if;

         exit when Current = Item;
         Offset := Offset + 4 + (Length + 3) / 4 * 4;
      end loop;

      declare
         Text : constant String (1 .. Natural (Length))
           with Import, Address => List.Buffer + Offset + 4;
      begin
         return Text;
      end;
   end Element;

end Rose.Command_Line;
