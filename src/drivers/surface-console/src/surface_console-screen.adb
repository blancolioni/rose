with Rose.Drawing.Text;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Region;
with Rose.Interfaces.Stream_Reader.Client;
with Rose.Interfaces.Surface.Client;

with Text_Console.Cells;

package body Surface_Console.Screen is

   use Rose.Drawing;
   use Rose.Words;

   Foreground : constant Pixel := RGB (16#AA#, 16#AA#, 16#AA#);
   Bold_Ink   : constant Pixel := RGB (16#FF#, 16#FF#, 16#FF#);
   Background : constant Pixel := 0;
   --  The VGA console's light grey on black, and white for bold.

   Surface : Rose.Interfaces.Surface.Client.Surface_Client;
   Events  : Rose.Interfaces.Stream_Reader.Client.Stream_Reader_Client;
   Target  : Bitmap;
   Width   : Word := 0;
   Height  : Word := 0;

   First_Damaged : Natural := Natural'Last;
   Last_Damaged  : Natural := 0;
   --  The rows that have changed since the last Flush.

   Cursor_Shown  : Boolean := False;
   Cursor_Column : Natural := 0;
   Cursor_Row    : Natural := 0;
   --  Where the cursor is drawn, if it is.

   procedure Damaged (Row : Natural);
   --  Row has changed.

   procedure Hide_Cursor;
   --  Take the cursor away, if it is drawn.

   --  The cell model's drawing.

   procedure Draw
     (Column, Row : Natural;
      Item        : Character;
      Attribute   : Text_Console.Attributes);
   procedure Scroll (Lines : Positive);
   procedure Clear;
   procedure Show_Cursor (Column, Row : Natural);
   procedure Flush;

   package Cells is
     new Text_Console.Cells
       (Max_Columns => 256,
        Max_Rows    => 128,
        Draw        => Draw,
        Scroll      => Scroll,
        Clear       => Clear,
        Show_Cursor => Show_Cursor,
        Flush       => Flush);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Fill (Target, (0, 0, Target.Width, Target.Height), Background);
      Cursor_Shown := False;
      First_Damaged := 0;
      Last_Damaged := Cells.Rows - 1;
   end Clear;

   -------------
   -- Damaged --
   -------------

   procedure Damaged (Row : Natural) is
   begin
      First_Damaged := Natural'Min (First_Damaged, Row);
      Last_Damaged := Natural'Max (Last_Damaged, Row);
   end Damaged;

   -------------
   -- Columns --
   -------------

   function Columns return Positive is (Cells.Columns);

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Column, Row : Natural;
      Item        : Character;
      Attribute   : Text_Console.Attributes)
   is
      Ink   : constant Pixel :=
                (if Attribute.Bold then Bold_Ink else Foreground);
      Paper : constant Pixel := Background;
   begin
      if Cursor_Shown
        and then Column = Cursor_Column
        and then Row = Cursor_Row
      then
         Cursor_Shown := False;
      end if;

      if Attribute.Reverse_Video then
         Text.Draw_Character (Target, Column, Row, Item, Paper, Ink);
      else
         Text.Draw_Character (Target, Column, Row, Item, Ink, Paper);
      end if;
      Damaged (Row);
   end Draw;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      if First_Damaged <= Last_Damaged then
         Surface.Damage
           (X      => 0,
            Y      => Word (First_Damaged * Text.Cell_Height),
            Width  => Width,
            Height =>
              Word ((Last_Damaged - First_Damaged + 1) * Text.Cell_Height));
      end if;
      First_Damaged := Natural'Last;
      Last_Damaged := 0;
   end Flush;

   -----------------
   -- Hide_Cursor --
   -----------------

   procedure Hide_Cursor is
   begin
      if Cursor_Shown then
         Text.Toggle_Cursor (Target, Cursor_Column, Cursor_Row);
         Damaged (Cursor_Row);
         Cursor_Shown := False;
      end if;
   end Hide_Cursor;

   ------------
   -- Notify --
   ------------

   procedure Notify (Signal : Rose.Capabilities.Capability) is
   begin
      Surface.Notify (Signal);
   end Notify;

   ----------
   -- Open --
   ----------

   procedure Open is
   begin
      Rose.Interfaces.Surface.Client.Open (Surface, Surface_Cap);
      Surface.Get_Size (Width, Height);

      declare
         Buffer : Rose.Interfaces.Region.Region_Object'Class :=
                    Surface.Get_Buffer;
      begin
         Target :=
           To_Bitmap
             (Rose.Shared_Regions.Map (Buffer),
              Natural (Width), Natural (Height));
      end;

      declare
         Stream : Rose.Interfaces.Stream_Reader.Stream_Reader_Object'Class :=
                    Surface.Events;
      begin
         Rose.Interfaces.Stream_Reader.Client.Open
           (Events, Stream.Stream_Reader_Cap);
      end;

      Cells.Set_Size
        (Columns =>
           Natural'Max (1, Natural'Min (256, Text.Columns (Target))),
         Rows    =>
           Natural'Max (1, Natural'Min (128, Text.Rows (Target))));
      Clear;
      Cells.Redraw;
   end Open;

   -------------
   -- Operate --
   -------------

   procedure Operate (Operation : Text_Console.Screen_Operation) is
   begin
      Cells.Operate (Operation);
   end Operate;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      Cells.Put (Text);
   end Put;

   -----------------
   -- Read_Events --
   -----------------

   procedure Read_Events
     (Buffer : out System.Storage_Elements.Storage_Array;
      Last   : out System.Storage_Elements.Storage_Count)
   is
   begin
      Rose.Interfaces.Stream_Reader.Client.Read (Events, Buffer, Last);
   end Read_Events;

   ----------
   -- Rows --
   ----------

   function Rows return Positive is (Cells.Rows);

   ------------
   -- Scroll --
   ------------

   procedure Scroll (Lines : Positive) is
   begin
      Hide_Cursor;
      Text.Scroll_Up
        (Target, 0, 0, Cells.Columns, Cells.Rows, Lines, Background);
      First_Damaged := 0;
      Last_Damaged := Cells.Rows - 1;
   end Scroll;

   -----------------
   -- Show_Cursor --
   -----------------

   procedure Show_Cursor (Column, Row : Natural) is
   begin
      Hide_Cursor;
      Text.Toggle_Cursor (Target, Column, Row);
      Cursor_Shown := True;
      Cursor_Column := Column;
      Cursor_Row := Row;
      Damaged (Row);
   end Show_Cursor;

end Surface_Console.Screen;
