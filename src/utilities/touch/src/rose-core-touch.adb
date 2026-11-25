procedure Rose.Core.Touch
  (File : in out Rose.Interfaces.Files.Transactional_Writable_File
                   .Transactional_Writable_File_Object'Class)
is
begin

   --  Petal has already made the file if there was none, so the commit
   --  changes nothing. If a new file has no name until something is
   --  committed to it (ADR 0027), this is what names it.

   File.Commit;
end Rose.Core.Touch;
