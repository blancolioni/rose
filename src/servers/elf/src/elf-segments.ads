with Rose.Words;

with Elf.Format;

package Elf.Segments
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   --  What the loader may trust of an executable's program headers.  An
   --  executable comes from the install media or from the store, so its
   --  headers may be corrupt or hostile: this takes them and the size of
   --  the image, and gives either the segments to load, which lie inside
   --  the image and the address space of a process and do not overlap, or
   --  that the headers are not valid.

   use type Rose.Words.Word_32;
   use type Rose.Words.Word_64;

   Max_Program_Headers : constant := 64;
   --  The most program headers that the loader reads.  Executables have
   --  about seven.

   Program_Header_Bytes : constant := 32;

   Address_Space : constant Rose.Words.Word_64 := 2 ** 32;

   type Segment is
      record
         File_Offset     : Rose.Words.Word_32 := 0;
         File_Size       : Rose.Words.Word_32 := 0;
         Virtual_Address : Rose.Words.Word_32 := 0;
         Memory_Size     : Rose.Words.Word_32 := 0;
         Readable        : Boolean := False;
         Writable        : Boolean := False;
         Executable      : Boolean := False;
      end record;
   --  The bytes File_Offset .. File_Offset + File_Size - 1 of the image
   --  are the first of the Memory_Size bytes at Virtual_Address; the rest
   --  are zero.

   function Valid
     (Item       : Segment;
      Image_Size : Rose.Words.Word_64;
      Limit      : Rose.Words.Word_64)
      return Boolean
   is (Item.Memory_Size > 0
       and then Item.File_Size <= Item.Memory_Size
       and then Rose.Words.Word_64 (Item.File_Offset)
                + Rose.Words.Word_64 (Item.File_Size) <= Image_Size
       and then Rose.Words.Word_64 (Item.Virtual_Address)
                + Rose.Words.Word_64 (Item.Memory_Size) <= Limit
       and then (Item.Writable or else Item.File_Size > 0));
   --  A segment is inside the image, which is Image_Size bytes, and in the
   --  address space below Limit, without wrapping around.  One that is not
   --  writable is mapped from the image, so it is not empty.

   function Disjoint (Left, Right : Segment) return Boolean
   is (Rose.Words.Word_64 (Left.Virtual_Address)
       + Rose.Words.Word_64 (Left.Memory_Size)
       <= Rose.Words.Word_64 (Right.Virtual_Address)
       or else Rose.Words.Word_64 (Right.Virtual_Address)
               + Rose.Words.Word_64 (Right.Memory_Size)
               <= Rose.Words.Word_64 (Left.Virtual_Address));

   type Segment_Array is array (1 .. Max_Program_Headers) of Segment;

   type Segment_List is
      record
         Count : Natural := 0;
         Items : Segment_Array;
      end record;

   function Valid
     (List       : Segment_List;
      Image_Size : Rose.Words.Word_64;
      Limit      : Rose.Words.Word_64)
      return Boolean
   is (List.Count <= Max_Program_Headers
       and then
         (for all I in 1 .. List.Count =>
            Valid (List.Items (I), Image_Size, Limit))
       and then
         (for all J in 1 .. List.Count =>
            (for all I in 1 .. J - 1 =>
               Disjoint (List.Items (I), List.Items (J)))));

   function Table_Fits
     (Header     : Elf.Format.Elf_Header;
      Image_Size : Rose.Words.Word_64)
      return Boolean
   is (Natural (Header.E_Phnum) <= Max_Program_Headers
       and then Natural (Header.E_Phentsize) = Program_Header_Bytes
       and then Rose.Words.Word_64 (Header.E_Phoff)
                + Rose.Words.Word_64 (Header.E_Phnum) * Program_Header_Bytes
                <= Image_Size);
   --  The program headers are all in the image, and there are not more
   --  than the loader reads.

   type Program_Header_Array is
     array (Positive range <>) of Elf.Format.Program_Header;

   procedure Describe
     (Headers    : Program_Header_Array;
      Image_Size : Rose.Words.Word_64;
      Limit      : Rose.Words.Word_64;
      List       : out Segment_List;
      Success    : out Boolean)
   with Pre  => Headers'Length <= Max_Program_Headers,
        Post => (if Success then Valid (List, Image_Size, Limit));
   --  The segments of the loadable headers (PT_LOAD), or not Success if
   --  any of them is not Valid, or overlaps another.

end Elf.Segments;
