generic
   type Key_Type is private;
   type Element_Type is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Rose.Containers.Queues
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts are proved (make prove), not executed.

   pragma Unevaluated_Use_Of_Old (Allow);

   subtype Capacity_Type is Count_Type range 0 .. 2 ** 30 - 1;
   --  A queue is a binary heap, in which the children of node I are 2I and
   --  2I + 1: with a larger capacity, they would not all be a Count_Type.

   type Queue (Capacity : Capacity_Type) is limited private;

   function Is_Empty
     (Container : Queue)
      return Boolean;

   function Length
     (Container : Queue)
      return Count_Type;

   procedure Insert
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type)
   with Pre  => Length (Container) < Container.Capacity,
        Post => Length (Container) = Length (Container)'Old + 1;

   procedure Replace
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type)
   with Post => Length (Container) = Length (Container)'Old;
   --  Give the first element that equals Element the key Key.

   procedure Delete
     (Container : in out Queue;
      Key       : Key_Type)
   with Post => Length (Container) <= Length (Container)'Old
                and then Length (Container) + 1 >= Length (Container)'Old;
   --  Delete the first element that has the key Key, if there is one.

   function First_Element
     (Container : Queue)
      return Element_Type
   with Pre => not Is_Empty (Container);

   function First_Key
     (Container : Queue)
      return Key_Type
   with Pre => not Is_Empty (Container);

   procedure Delete_First
     (Container : in out Queue)
   with Pre  => not Is_Empty (Container),
        Post => Length (Container) = Length (Container)'Old - 1;

   procedure Iterate
     (Container : Queue;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type))
   with SPARK_Mode => Off;

private

   type Queue_Element is
      record
         Key : Key_Type;
         Element : Element_Type;
      end record;

   type Queue_Vector is array (Count_Type range <>) of Queue_Element;

   type Queue (Capacity : Capacity_Type) is limited
      record
         Vector : Queue_Vector (1 .. Capacity);
         Length : Count_Type := 0;
      end record
   with Type_Invariant => Queue.Length <= Queue.Capacity;

   function Is_Empty
     (Container : Queue)
      return Boolean
   is (Container.Length = 0);

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
