generic
   Capacity : Count_Type;
   Modulus  : Hash_Type;
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Hash_Type;
package Rose.Containers.Bounded_Hashed_Maps
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   pragma Unevaluated_Use_Of_Old (Allow);

   pragma Compile_Time_Error
     (Modulus = 0 or else Modulus > 2 ** 30,
      "the buckets are numbered with a Hash_Type, and added together");

   pragma Compile_Time_Error
     (Hash_Type (Capacity) >= Modulus,
      "a map with as many elements as buckets has no empty bucket to end a"
      & " search with");

   type Map is limited private;
   --  A hash table with linear probing, which holds at most Capacity
   --  elements in Modulus buckets.

   function Is_Empty (Container : Map) return Boolean;
   function Is_Full (Container : Map) return Boolean;

   function Length (Container : Map) return Count_Type;

   type Cursor is private;
   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   function Is_Valid (Container : Map; Position : Cursor) return Boolean;
   --  Position is that of an element of Container.

   function Key
     (Container : Map;
      Position  : Cursor)
      return Key_Type
   with Pre => Is_Valid (Container, Position);

   function Element
     (Container : Map;
      Position  : Cursor)
      return Element_Type
   with Pre => Is_Valid (Container, Position);

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type)
   with Pre  => Is_Valid (Container, Position),
        Post => Length (Container) = Length (Container)'Old;

   function Contains
     (Container : Map;
      Key       : Key_Type)
      return Boolean;

   function Element
     (Container : Map;
      Key       : Key_Type)
      return Element_Type
   with Pre => Contains (Container, Key);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean)
   with Post => (if Inserted
                 then Length (Container) = Length (Container)'Old + 1
                 else Length (Container) = Length (Container)'Old);
   --  Inserted is False if Key is in Container, when Position is its
   --  position, or if Container is full, when it is No_Element.

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type)
   with Post => Length (Container) <= Length (Container)'Old + 1;

   procedure Delete
     (Container : in out Map;
      Position  : in out Cursor)
   with Pre  => Is_Valid (Container, Position),
        Post => Position = No_Element
                and then Length (Container) + 1 = Length (Container)'Old;

   function Find
     (Container : Map;
      Key       : Key_Type)
      return Cursor;
   --  The position of Key, or No_Element.

private

   type Element_Record is
      record
         Key     : Key_Type;
         Element : Element_Type;
      end record;

   type Elements_Type is array (Count_Type range <>) of Element_Record;

   subtype Bucket_Type is Count_Type range 0 .. Capacity;
   --  The index in Elements of the element in a bucket, or 0 if it is
   --  empty.

   type Buckets_Type is array (Hash_Type range <>) of Bucket_Type;

   type Map is limited
      record
         Length   : Count_Type range 0 .. Capacity := 0;
         Elements : Elements_Type (1 .. Capacity);
         Buckets  : Buckets_Type (0 .. Modulus - 1) := (others => 0);
      end record;

   type Cursor is new Count_Type;
   No_Element : constant Cursor := 0;

   function Is_Empty (Container : Map) return Boolean
   is (Container.Length = 0);

   function Is_Full (Container : Map) return Boolean
   is (Container.Length >= Capacity);

   function Length (Container : Map) return Count_Type
   is (Container.Length);

   function Has_Element (Position : Cursor) return Boolean
   is (Position /= 0);

   function Is_Valid (Container : Map; Position : Cursor) return Boolean
   is (Position /= 0 and then Count_Type (Position) <= Container.Length);

end Rose.Containers.Bounded_Hashed_Maps;
