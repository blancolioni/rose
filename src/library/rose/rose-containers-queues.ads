generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Rose.Containers.Queues is

   type Queue (Capacity : Count_Type) is limited private;

   function Is_Empty
     (Container : Queue)
      return Boolean;

   function Length
     (Container : Queue)
      return Count_Type;

   procedure Insert
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type);

   procedure Replace
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type);

   procedure Delete
     (Container : in out Queue;
      Key       : Key_Type);

   function First_Element
     (Container : Queue)
      return Element_Type;

   function First_Key
     (Container : Queue)
      return Key_Type;

   procedure Delete_First
     (Container : in out Queue);

   procedure Iterate
     (Container : Queue;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type));

private

   type Queue_Element is
      record
         Key : Key_Type;
         Element : Element_Type;
      end record;

   type Queue_Vector is array (Count_Type range <>) of Queue_Element;

   type Queue (Capacity : Count_Type) is limited
      record
         Vector : Queue_Vector (1 .. Capacity);
         Length : Count_Type := 0;
      end record;

   function Length
     (Container : Queue)
      return Count_Type
   is (Container.Length);

   function First_Element
     (Container : Queue)
      return Element_Type
   is (Container.Vector (1).Element);

   function First_Key
     (Container : Queue)
      return Key_Type
   is (Container.Vector (1).Key);

end Rose.Containers.Queues;
