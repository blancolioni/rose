pragma Ada_2022;

with Ada.Streams.Storage.Unbounded;
with Ada.Text_IO;

package body Tests.Streams is

   type Point is
      record
         X, Y : Integer;
         Name : String (1 .. 3);
      end record;

   Expected_Image : constant String :=
                      ASCII.LF & "(X =>  1," & ASCII.LF
                      & " Y => -2," & ASCII.LF
                      & " NAME => ""abc"")";

   -----------------
   -- Stream_Test --
   -----------------

   function Stream_Test return Boolean is
      use type Ada.Streams.Stream_Element_Count;
      Buffer : aliased Ada.Streams.Storage.Unbounded.Stream_Type;
      Item   : constant Point := (X => 1, Y => -2, Name => "abc");
      Copy   : Point;
   begin
      Ada.Text_IO.Put ("Stream test");

      Point'Write (Buffer'Access, Item);
      Point'Read (Buffer'Access, Copy);

      return Copy = Item
        and then Ada.Streams.Storage.Unbounded.Element_Count (Buffer) = 0
        and then Item'Image = Expected_Image;
   end Stream_Test;

end Tests.Streams;
