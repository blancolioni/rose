package body Rose.Containers.Queues is

   procedure Heapify
     (Container : in out Queue);

   procedure Sift_Down
     (Container : in out Queue;
      Start     : Count_Type);

   procedure Sift_Up
     (Container : in out Queue;
      Start     : Count_Type;
      Node      : Count_Type);

   function Key
     (Container : Queue;
      Index     : Count_Type)
      return Key_Type
   is (Container.Vector (Index).Key);

   procedure Swap_Elements
     (Container        : in out Queue;
      Index_1, Index_2 : Count_Type);

   function Left_Child
     (Node : Count_Type)
      return Count_Type
   is (Node * 2);

   function Right_Child
     (Node : Count_Type)
      return Count_Type
   is (Node * 2 + 1)
     with Unreferenced;

   function Parent
     (Node : Count_Type)
      return Count_Type
   is (Node / 2);

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Queue;
      Key       : Key_Type)
   is
   begin
      for I in 1 .. Container.Length loop
         if Container.Vector (I).Key = Key then
            if I < Container.Length then
               Container.Vector (I) :=
                 Container.Vector (Container.Length);
            end if;
            Container.Length := Container.Length - 1;
            Sift_Down (Container, I);
            return;
         end if;
      end loop;
   end Delete;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Container : in out Queue)
   is
   begin
      Container.Vector (1) := Container.Vector (Container.Length);
      Container.Length := Container.Length - 1;
      Sift_Down (Container, 1);
   end Delete_First;

   -------------
   -- Heapify --
   -------------

   procedure Heapify
     (Container : in out Queue)
   is
      Start : Count_Type := Parent (Container.Length);
   begin
      while Start > 0 loop
         Sift_Down (Container, Start);
         Start := Start - 1;
      end loop;
   end Heapify;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type)
   is
   begin
      Container.Length := Container.Length + 1;
      Container.Vector (Container.Length) := (Key, Element);
      Sift_Up (Container, 1, Container.Length);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Container : Queue)
      return Boolean
   is
   begin
      return Container.Length = 0;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Queue;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type))
   is
   begin
      for Item of Container.Vector loop
         Process (Item.Key, Item.Element);
      end loop;
   end Iterate;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Queue;
      Key       : Key_Type;
      Element   : Element_Type)
   is
   begin
      for I in 1 .. Container.Length loop
         if Container.Vector (I).Element = Element then
            Container.Vector (I).Key := Key;
            Heapify (Container);
            return;
         end if;
      end loop;
   end Replace;

   ---------------
   -- Sift_Down --
   ---------------

   procedure Sift_Down
     (Container : in out Queue;
      Start     : Count_Type)
   is
      Root   : Count_Type := Start;
      Child  : Count_Type;
      Swap : Count_Type;
      Last   : constant Count_Type := Container.Length;
   begin
      while Left_Child (Root) <= Last loop
         Child := Left_Child (Root);
         Swap := Root;

         if Key (Container, Child) < Key (Container, Swap) then
            Swap := Child;
         end if;

         if Child + 1 <= Last
           and then Key (Container, Child + 1) < Key (Container, Swap)
         then
            Swap := Child + 1;
         end if;

         if Swap = Root then
            return;
         end if;

         Swap_Elements (Container, Swap, Root);

         Root := Swap;
      end loop;
   end Sift_Down;

   -------------
   -- Sift_Up --
   -------------

   procedure Sift_Up
     (Container : in out Queue;
      Start     : Count_Type;
      Node      : Count_Type)
   is
      Child : Count_Type := Node;
   begin
      while Child > Start loop
         declare
            P : constant Count_Type := Parent (Child);
         begin
            if Key (Container, P) < Key (Container, Child) then
               Swap_Elements (Container, P, Child);
               Child := P;
            else
               exit;
            end if;
         end;
      end loop;
   end Sift_Up;

   ----------
   -- Swap --
   ----------

   procedure Swap_Elements
     (Container        : in out Queue;
      Index_1, Index_2 : Count_Type)
   is
      X : constant Queue_Element := Container.Vector (Index_1);
   begin
      Container.Vector (Index_1) := Container.Vector (Index_2);
      Container.Vector (Index_2) := X;
   end Swap_Elements;

end Rose.Containers.Queues;
