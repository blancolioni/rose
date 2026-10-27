with Ada.Text_IO;

package body Tests.Dispatching is

   --  The tests program has no secondary stack, so nothing here is
   --  returned by a function: a tagged result is returned on the
   --  secondary stack.

   package Shapes is

      type Shape is interface;

      function Area (Item : Shape) return Natural is abstract;

      type Rectangle is new Shape with
         record
            Width, Height : Natural;
         end record;

      overriding function Area (Item : Rectangle) return Natural;

      type Square is new Rectangle with null record;

      type Triangle is new Shape with
         record
            Base, Height : Natural;
         end record;

      overriding function Area (Item : Triangle) return Natural;

   end Shapes;

   function Total_Area (A, B, C : Shapes.Shape'Class) return Natural;
   --  The sum of the areas, through dispatching calls.

   ------------
   -- Shapes --
   ------------

   package body Shapes is

      ----------
      -- Area --
      ----------

      overriding function Area (Item : Rectangle) return Natural is
      begin
         return Item.Width * Item.Height;
      end Area;

      ----------
      -- Area --
      ----------

      overriding function Area (Item : Triangle) return Natural is
      begin
         return Item.Base * Item.Height / 2;
      end Area;

   end Shapes;

   ----------------------
   -- Dispatching_Test --
   ----------------------

   function Dispatching_Test return Boolean is
      use Shapes;
      R : constant Rectangle := (Width => 3, Height => 4);
      S : constant Square := (Width => 5, Height => 5);
      T : constant Triangle := (Base => 6, Height => 7);
   begin
      Ada.Text_IO.Put ("Dispatching test");
      return Total_Area (R, S, T) = 12 + 25 + 21
        and then Shape'Class (S) in Rectangle'Class
        and then Shape'Class (T) not in Rectangle'Class
        and then Shape'Class (S) in Square;
   end Dispatching_Test;

   ----------------
   -- Total_Area --
   ----------------

   function Total_Area (A, B, C : Shapes.Shape'Class) return Natural is
   begin
      return A.Area + B.Area + C.Area;
   end Total_Area;

end Tests.Dispatching;
