package body Rose.Containers.Bounded_Hashed_Maps
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts, loop invariants and variants are for the proof, and the
   --  boot servers' run-time system has no System.Assertions.

   function Find_Bucket
     (Container : Map;
      Index     : Count_Type)
      return Hash_Type
   with Pre  => Index in 1 .. Capacity,
        Post => Find_Bucket'Result < Modulus;
   --  Return the bucket which refers to Container.Elements (Index)

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Map;
      Key       : Key_Type)
      return Boolean
   is (Has_Element (Find (Container, Key)));

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Map;
      Position  : in out Cursor)
   is
      Index : constant Count_Type := Count_Type (Position);
      Last  : constant Count_Type := Container.Length;
      Hole  : Hash_Type := Find_Bucket (Container, Index);
      N     : Hash_Type := (Hole + 1) mod Modulus;
   begin

      --  backward shift: an entry following the hole can fill it
      --  unless its home bucket lies cyclically in (Hole, N].  The
      --  entries that follow end at an empty bucket, and there is one
      --  (Capacity is less than Modulus).

      for Step in 1 .. Modulus - 1 loop
         pragma Loop_Invariant (Hole < Modulus and then N < Modulus);
         pragma Loop_Invariant (Container.Length = Last);

         exit when Container.Buckets (N) = 0;

         declare
            Home : constant Hash_Type :=
                     Hash (Container.Elements (Container.Buckets (N)).Key)
                     mod Modulus;
         begin
            if (N + Modulus - Home) mod Modulus
              >= (N + Modulus - Hole) mod Modulus
            then
               Container.Buckets (Hole) := Container.Buckets (N);
               Hole := N;
            end if;
         end;
         N := (N + 1) mod Modulus;
      end loop;
      Container.Buckets (Hole) := 0;

      --  move the last element into the freed slot

      if Index /= Last then
         Container.Buckets (Find_Bucket (Container, Last)) := Index;
         Container.Elements (Index) := Container.Elements (Last);
      end if;

      Container.Length := Last - 1;
      Position := No_Element;
   end Delete;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      Position  : Cursor)
      return Element_Type
   is (Container.Elements (Count_Type (Position)).Element);

   -------------
   -- Element --
   -------------

   function Element
     (Container : Map;
      Key       : Key_Type)
      return Element_Type
   is
      Position : constant Cursor := Find (Container, Key);
   begin
      return Container.Elements (Count_Type (Position)).Element;
   end Element;

   ----------
   -- Find --
   ----------

   function Find
     (Container : Map;
      Key       : Key_Type)
      return Cursor
   with Refined_Post => Find'Result = No_Element
                        or else Count_Type (Find'Result) in 1 .. Capacity
   is
      Home : constant Hash_Type := Hash (Key) mod Modulus;
      H    : Hash_Type;
   begin
      for Probe in 0 .. Modulus - 1 loop
         H := (Home + Probe) mod Modulus;

         if Container.Buckets (H) = 0 then
            return No_Element;
         end if;

         if Container.Elements (Container.Buckets (H)).Key = Key then
            return Cursor (Container.Buckets (H));
         end if;
      end loop;

      return No_Element;
   end Find;

   -----------------
   -- Find_Bucket --
   -----------------

   function Find_Bucket
     (Container : Map;
      Index     : Count_Type)
      return Hash_Type
   is
      Home : constant Hash_Type :=
               Hash (Container.Elements (Index).Key) mod Modulus;
   begin
      for Probe in 0 .. Modulus - 1 loop
         declare
            H : constant Hash_Type := (Home + Probe) mod Modulus;
         begin
            if Container.Buckets (H) = Index then
               return H;
            end if;
         end;
      end loop;

      return Home;
   end Find_Bucket;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   is
      Found : constant Cursor := Find (Container, Key);
      Home  : constant Hash_Type := Hash (Key) mod Modulus;
      H     : Hash_Type;
   begin
      Position := Found;
      Inserted := False;

      if Has_Element (Found) or else Is_Full (Container) then
         return;
      end if;

      --  The first empty bucket from the home of Key on.

      for Probe in 0 .. Modulus - 1 loop
         pragma Loop_Invariant (Container.Length < Capacity);

         H := (Home + Probe) mod Modulus;

         if Container.Buckets (H) = 0 then
            Container.Length := Container.Length + 1;
            Container.Elements (Container.Length) :=
              Element_Record'
                (Key     => Key,
                 Element => New_Item);
            Container.Buckets (H) := Container.Length;
            Position := Cursor (Container.Length);
            Inserted := True;
            return;
         end if;
      end loop;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   is
      Inserted : Boolean;
      Position : Cursor;
   begin
      Insert (Container, Key, New_Item, Position, Inserted);
   end Insert;

   ---------
   -- Key --
   ---------

   function Key
     (Container : Map;
      Position  : Cursor)
      return Key_Type
   is (Container.Elements (Count_Type (Position)).Key);

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   is
   begin
      Container.Elements (Count_Type (Position)).Element := New_Item;
   end Replace_Element;

end Rose.Containers.Bounded_Hashed_Maps;
