with Rose.Command_Line.Items;

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
      --  The only place that the buffer is an address.  What is in it is
      --  decoded by Rose.Command_Line.Items, which is in SPARK.
      Buffer : constant Storage_Array (1 .. List.Buffer_Length)
        with Import, Address => List.Buffer;
   begin
      return Items.Element (Buffer, List.First_Item, List.Count, Index);
   end Element;

end Rose.Command_Line;
