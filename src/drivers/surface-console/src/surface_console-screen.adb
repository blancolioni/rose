with Rose.Drawing.Text;
with Rose.Shared_Regions;
with Rose.Words;

with Rose.Interfaces.Region;
with Rose.Interfaces.Reader.Client;
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
   Events  : Rose.Interfaces.Reader.Client.Reader_Client;
   Target  : Bitmap;
   Width   : Word := 0;
   Height  : Word := 0;

   Max_Rows : constant := 128;

   type Row_Flags is array (0 .. Max_Rows - 1) of Boolean;

   Damaged_Rows : Row_Flags := (others => False);
   --  The rows that have changed since the last Flush.  Each row is
   --  flagged on its own, so that moving the cursor from one row to
   --  another damages those two rows, and not the rows between them
   --  (#787).

   Cursor_Shown  : Boolean := False;
   Cursor_Column : Natural := 0;
   Cursor_Row    : Natural := 0;
   --  Where the cursor is drawn, if it is.

   procedure Damaged (Row : Natural);
   --  Row has changed.

   procedure Damaged_All;
   --  Every row has changed.

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
        Max_Rows    => Max_Rows,
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
      Damaged_All;
   end Clear;

   -------------
   -- Damaged --
   -------------

   procedure Damaged (Row : Natural) is
   begin
      Damaged_Rows (Row) := True;
   end Damaged;

   -----------------
   -- Damaged_All --
   -----------------

   procedure Damaged_All is
   begin
      Damaged_Rows (0 .. Cells.Rows - 1) := (others => True);
   end Damaged_All;

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
      Row : Natural := 0;
   begin
      --  One Damage for each run of rows that changed.

      while Row < Cells.Rows loop
         if Damaged_Rows (Row) then
            declare
               First : constant Natural := Row;
            begin
               while Row < Cells.Rows and then Damaged_Rows (Row) loop
                  Damaged_Rows (Row) := False;
                  Row := Row + 1;
               end loop;

               Surface.Damage
                 (X      => 0,
                  Y      => Word (First * Text.Cell_Height),
                  Width  => Width,
                  Height => Word ((Row - First) * Text.Cell_Height));
            end;
         else
            Row := Row + 1;
         end if;
      end loop;
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
         Stream : Rose.Interfaces.Reader.Reader_Object'Class :=
                    Surface.Events;
      begin
         Rose.Interfaces.Reader.Client.Open
           (Events, Stream.Reader_Cap);
      end;

      Cells.Set_Size
        (Columns =>
           Natural'Max (1, Natural'Min (256, Text.Columns (Target))),
         Rows    =>
           Natural'Max (1, Natural'Min (Max_Rows, Text.Rows (Target))));
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
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Rose.Interfaces.Reader.Client.Read (Events, Buffer, Last);
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
      Damaged_All;
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
