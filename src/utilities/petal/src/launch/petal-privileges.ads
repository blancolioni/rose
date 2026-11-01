package Petal.Privileges is

   --  The decisions of sudo (ADR 0021, section 2), on names alone, so that
   --  they can be tested on the host. A command's roles have categories,
   --  and sudo names the categories that the user wants for one command,
   --  separated by commas.

   function Covers
     (Name     : String;
      Category : String)
      return Boolean;
   --  True if the category Name covers the category Category: they are the
   --  same, Category is below Name (hardware covers hardware.ports), or
   --  Name is "*".

   function Covered
     (Names    : String;
      Category : String)
      return Boolean;
   --  True if one of the comma-separated Names covers Category.

   function Count (Names : String) return Natural;
   --  The number of comma-separated names in Names.

   function Item
     (Names : String;
      Index : Positive)
      return String;
   --  The Index'th name in Names, or "" if it has fewer.

end Petal.Privileges;
