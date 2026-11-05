package Text_Console with Pure is

   --  What the two persistent consoles share (ADR 0005, #653): the line
   --  discipline, and the screen as cells of characters. vga-console
   --  draws the cells on the VGA text page, and surface-console on a
   --  surface. Each is a generic, instantiated with the console's own
   --  drawing.

   type Attributes is
      record
         Bold          : Boolean := False;
         Reverse_Video : Boolean := False;
      end record;
   --  How a cell is drawn (Terminal.Set_Attributes, #734).

   Plain : constant Attributes := (others => False);

   type Operation_Kind is
     (Move_Cursor, Clear, Clear_To_End_Of_Line, Scroll, Set_Attributes);

   type Screen_Operation (Kind : Operation_Kind := Clear) is
      record
         case Kind is
            when Move_Cursor =>
               Column, Row : Natural;
            when Scroll =>
               Lines : Integer;
            when Set_Attributes =>
               Attribute : Attributes;
            when Clear | Clear_To_End_Of_Line =>
               null;
         end case;
      end record;
   --  One of Terminal's screen operations (#734).

end Text_Console;
