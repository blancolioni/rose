package body Rose.Containers.Queues
  with SPARK_Mode
is

   pragma Assertion_Policy (Ignore);
   --  The contracts, loop invariants and variants are for the proof, and the
   --  boot servers' run-time system has no System.Assertions.

   procedure Heapify
     (Container : in out Queue)
   with Pre  => Container.Length <= Container.Capacity,
        Post => Length (Container) = Length (Container)'Old;

   procedure Sift_Down
     (Container : in out Queue;
      Start     : Count_Type)
   with Pre  => Container.Length <= Container.Capacity
                and then Start >= 1
                and then Start <= Container.Capacity,
        Post => Length (Container) = Length (Container)'Old;

   procedure Sift_Up
     (Container : in out Queue;
      Start     : Count_Type;
      Node      : Count_Type)
   with Pre  => Container.Length <= Container.Capacity
                and then Start >= 1
                and then Node <= Container.Length,
        Post => Length (Container) = Length (Container)'Old;

   function Key
     (Container : Queue;
      Index     : Count_Type)
      return Key_Type
   is (Container.Vector (Index).Key)
   with Pre => Index in 1 .. Container.Capacity;

   procedure Swap_Elements
     (Container        : in out Queue;
      Index_1, Index_2 : Count_Type)
   with Pre  => Container.Length <= Container.Capacity
                and then Index_1 in 1 .. Container.Capacity
                and then Index_2 in 1 .. Container.Capacity,
        Post => Length (Container) = Length (Container)'Old;

   function Left_Child
     (Node : Count_Type)
      return Count_Type
   is (Node * 2)
   with Pre => Node <= Capacity_Type'Last;

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
               Container.Length := Container.Length - 1;

               --  the moved element may belong above or below I

               Sift_Up (Container, 1, I);
               Sift_Down (Container, I);
            else
               Container.Length := Container.Length - 1;
            end if;
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
         pragma Loop_Variant (Decreases => Start);
         pragma Loop_Invariant (Start <= Container.Capacity);
         pragma Loop_Invariant
           (Length (Container) = Length (Container)'Loop_Entry);
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Queue;
      Process   : not null access
        procedure (Key : Key_Type;
                   Element : Element_Type))
   with SPARK_Mode => Off
   is
   begin
      for Item of Container.Vector (1 .. Container.Length) loop
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
      Swap   : Count_Type;
      Last   : constant Count_Type := Container.Length;
   begin
      while Left_Child (Root) <= Last loop
         pragma Loop_Variant (Increases => Root);
         pragma Loop_Invariant (Root >= Start and then Root < Last);
         pragma Loop_Invariant (Last = Container.Length);

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
         pragma Loop_Variant (Decreases => Child);
         pragma Loop_Invariant (Child <= Container.Length);
         pragma Loop_Invariant
           (Length (Container) = Length (Container)'Loop_Entry);

         declare
            P : constant Count_Type := Parent (Child);
         begin
            if Key (Container, Child) < Key (Container, P) then
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
