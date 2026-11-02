with System.Storage_Elements;

package Rose.Command_Line.Items
  with SPARK_Mode
is

   use type System.Storage_Elements.Storage_Offset;

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   --  Decoding the items of a launch buffer (see Rose.Command_Line.Launch).
   --  The buffer comes from another process, so what is in it, and how long
   --  it is, is not to be trusted.  This takes it as an array, and the one
   --  conversion from an address to that array is in Rose.Command_Line.

   function Element
     (Buffer     : System.Storage_Elements.Storage_Array;
      First_Item : Positive;
      Count      : Natural;
      Index      : Positive)
      return String
   with Pre  => Buffer'First = 1
                and then Buffer'Last in 0 ..
                  System.Storage_Elements.Storage_Offset (Natural'Last),
        Post => Element'Result'First = 1
                and then Element'Result'Length <= Natural (Buffer'Last)
                and then (if Index > Count then Element'Result'Length = 0);
   --  The item First_Item + Index - 1 of the buffer, or "" if Index is
   --  greater than Count, or if the item is not all in the buffer.  Each
   --  item is a 32-bit little-endian length, the characters, and padding to
   --  a multiple of four bytes.
   --
   --  The precondition is true of the buffers that Rose.Command_Line
   --  makes: from 1, and no longer than the target's Storage_Count.  It is
   --  stated in Buffer'Last and not Buffer'Length because gnatprove is a
   --  64-bit program, which makes the length of a Storage_Array a larger
   --  type than the Natural that it is checked against.

end Rose.Command_Line.Items;
