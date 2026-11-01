package body Petal.Privileges is

   -----------
   -- Count --
   -----------

   function Count (Names : String) return Natural is
      Result : Natural := 0;
   begin
      if Names'Length > 0 then
         Result := 1;
         for Ch of Names loop
            if Ch = ',' then
               Result := Result + 1;
            end if;
         end loop;
      end if;
      return Result;
   end Count;

   -------------
   -- Covered --
   -------------

   function Covered
     (Names    : String;
      Category : String)
      return Boolean
   is
   begin
      return (for some I in 1 .. Count (Names) =>
                Covers (Item (Names, I), Category));
   end Covered;

   ------------
   -- Covers --
   ------------

   function Covers
     (Name     : String;
      Category : String)
      return Boolean
   is
   begin
      return Name = "*"
        or else Name = Category
        or else (Category'Length > Name'Length
                 and then Category (Category'First
                                    .. Category'First + Name'Length - 1)
                            = Name
                 and then Category (Category'First + Name'Length) = '.');
   end Covers;

   ----------
   -- Item --
   ----------

   function Item
     (Names : String;
      Index : Positive)
      return String
   is
      Number : Positive := 1;
      First  : Positive := Names'First;
   begin
      for I in Names'Range loop
         if Names (I) = ',' then
            if Number = Index then
               return Names (First .. I - 1);
            end if;
            Number := Number + 1;
            First := I + 1;
         end if;
      end loop;
      return (if Number = Index then Names (First .. Names'Last) else "");
   end Item;

end Petal.Privileges;
