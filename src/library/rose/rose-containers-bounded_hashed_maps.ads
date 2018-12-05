generic
   Capacity : Count_Type;
   Modulus  : Hash_Type;
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Rose.Containers.Bounded_Hashed_Maps is

   type Map is limited private;

   function Is_Empty (Container : Map) return Boolean;
   function Is_Full (Container : Map) return Boolean;

   type Cursor is private;
   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;
   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out Map;
      Position  : Cursor;
      New_Item  : Element_Type);

   function Contains
     (Container : Map;
      Key       : Key_Type)
      return Boolean;

   function Element
     (Container : Map;
      Key       : Key_Type)
      return Element_Type;

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Delete
     (Container : in out Map;
      Position  : in out Cursor);

   function Find
     (Container : Map;
      Key       : Key_Type)
      return Cursor;

private

   type Elements_Type is array (Count_Type range <>) of Element_Type;
   type Buckets_Type is array (Hash_Type range <>) of Count_Type;

   type Map is
      record
         Length   : Count_Type                    := 0;
         Elements : Elements_Type (1 .. Capacity) := (others => <>);
         Buckets  : Buckets_Type (1 .. Modulus)   := (others => 0);
      end record;

   type Cursor is
      record
         Container : access Map;
         Current   : Count_Type;
      end record;

   No_Element : constant Cursor := (null, 0);

end Rose.Containers.Bounded_Hashed_Maps;
