package Tests.Unsaved_Memory is

   function Unsaved_Memory_Test return Boolean;
   --  Rose.Unsaved_Memory (#1132): allocate two segments, and check that
   --  they are in the window kept for them, that they don't overlap, that
   --  they start as zeros, and that what is written to each stays there.
   --  Release the first, and check that its address is used again, that
   --  the new segment is zeros again, and that releasing an address that
   --  isn't allocated raises Mapping_Error. That a checkpoint doesn't
   --  save them is test-boot's password check.

end Tests.Unsaved_Memory;
