package body Rose.Containers.Bounded_Hashed_Maps is


   --------------
   -- Contains --
   --------------

   function Contains
     (Key       : Key_Type)
      return Boolean
   is
   begin
      return Has_Element (Find (Key));
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Position  : in out Cursor)
   is
      H : Hash_Type := Hash (Key (Position));
      N : Hash_Type := (H + 1) mod Modulus;
   begin
      while Container.Buckets (N) /= 0 loop
         if Hash (Container.Elements (Container.Buckets (N)).Key) <= H then
            Container.Buckets (H) := Container.Buckets (N);
            H := N;
         end if;
         N := (N + 1) mod Modulus;
      end loop;
      Container.Buckets (H) := 0;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element (Position : Cursor) return Element_Type is
   begin
      return Container.Elements (Count_Type (Position)).Element;
   end Element;

   -------------
   -- Element --
   -------------

   function Element
     (Key       : Key_Type)
      return Element_Type
   is
      Position : constant Cursor := Find (Key);
   begin
      return Result : Element_Type do
         if Has_Element (Position) then
            Result := Element (Position);
         end if;
      end return;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Key       : Key_Type)
      return Cursor
   is
      H : Hash_Type := Hash (Key) mod Modulus;
   begin
      while Container.Buckets (H) /= 0
        and then Container.Elements (Container.Buckets (H)).Key /= Key
      loop
         H := (H + 1) mod Modulus;
      end loop;
      return Cursor (Container.Buckets (H));
   end Find;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= 0;
   end Has_Element;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      H : Hash_Type :=
            Hash (Key) mod Modulus;
   begin
      if Contains (Key) then
         Inserted := False;
         return;
      end if;

      if Is_Full then
         Inserted := False;
         return;
      end if;

      Container.Length := Container.Length + 1;
      Container.Elements (Container.Length) :=
        Element_Record'
          (Key     => Key,
           Element => New_Item);

      Position := Cursor (Container.Length);

      while Container.Buckets (H) /= 0 loop
         H := (H + 1) mod Modulus;
      end loop;

      Container.Buckets (H) := Container.Length;

   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Inserted : Boolean;
      Position : Cursor;
   begin
      Insert (Key, New_Item, Position, Inserted);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full return Boolean is
   begin
      return Container.Length >= Container.Elements'Length;
   end Is_Full;

   ---------
   -- Key --
   ---------

   function Key (Position : Cursor) return Key_Type is
   begin
      return Container.Elements (Count_Type (Position)).Key;
   end Key;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      Container.Elements (Count_Type (Position)).Element := New_Item;
   end Replace_Element;

end Rose.Containers.Bounded_Hashed_Maps;
