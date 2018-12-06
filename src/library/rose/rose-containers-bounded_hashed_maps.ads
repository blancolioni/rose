generic
   Capacity : Count_Type;
   Modulus  : Hash_Type;
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Hash_Type;
package Rose.Containers.Bounded_Hashed_Maps is

   function Is_Empty return Boolean;
   function Is_Full return Boolean;

   type Cursor is private;
   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;
   function Key (Position : Cursor) return Key_Type;
   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Position  : Cursor;
      New_Item  : Element_Type);

   function Contains
     (Key       : Key_Type)
      return Boolean;

   function Element
     (Key       : Key_Type)
      return Element_Type;

   procedure Insert
     (Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Delete
     (Position  : in out Cursor);

   function Find
     (Key       : Key_Type)
      return Cursor;

private

   type Element_Record is
      record
         Key     : Key_Type;
         Element : Element_Type;
      end record;

   type Elements_Type is array (Count_Type range <>) of Element_Record;
   type Buckets_Type is array (Hash_Type range <>) of Count_Type;

   type Map is
      record
         Length   : Count_Type                    := 0;
         Elements : Elements_Type (1 .. Capacity) := (others => <>);
         Buckets  : Buckets_Type (1 .. Modulus)   := (others => 0);
      end record;

   type Cursor is new Count_Type;
   No_Element : constant Cursor := 0;

   Container : Map;

end Rose.Containers.Bounded_Hashed_Maps;
