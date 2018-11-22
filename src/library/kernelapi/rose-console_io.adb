with Rose.System_Calls.Client;

package body Rose.Console_IO is

   Con_Cap : Rose.Capabilities.Capability;

   Text_Buffer   : String (1 .. 4096)
     with Alignment => 4096;
   Buffer_Length : Natural := 0;

   procedure Send_Buffer;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Put (Character'Val (10));
   end New_Line;

   ----------
   -- Open --
   ----------

   procedure Open
     (Console_Cap : Rose.Capabilities.Capability)
   is
   begin
      Con_Cap := Console_Cap;
   end Open;

   ---------
   -- Put --
   ---------

   procedure Put (Ch : Character) is
   begin
      Buffer_Length := Buffer_Length + 1;
      Text_Buffer (Buffer_Length) := Ch;
      if Ch = Character'Val (10) or else Buffer_Length = Text_Buffer'Last then
         Send_Buffer;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Text : String) is
   begin
      for Ch of Text loop
         Put (Ch);
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Num : Integer) is
      S : constant String := "0123456789";
      X : Integer := Num;
   begin
      if X = 0 then
         Put ('0');
      else
         if X < 0 then
            Put ('-');
            X := abs X;
         end if;

         declare
            It    : Natural := X;
            Img   : String (1 .. 16);
            Index : Natural := Img'Last;
         begin
            while It /= 0 loop
               Img (Index) := S (It mod 10 + 1);
               Index := Index - 1;
               It := It / 10;
            end loop;
            Put (Img (Index + 1 .. Img'Last));
         end;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (X     : Natural;
      Width : Positive;
      Pad   : Character := ' ')
   is
      Buffer : String (1 .. 16);
      Index  : Natural := 0;
      S      : constant String := "0123456789";
   begin
      if X = 0 then
         Index := Index + 1;
         Buffer (Index) := '0';
      else
         declare
            It    : Natural := X;
         begin
            while It /= 0 loop
               Index := Index + 1;
               Buffer (Index) := S (It mod 10 + 1);
               It := It / 10;
            end loop;
         end;
      end if;

      if Index < Width then
         declare
            Padding : constant String (1 .. Width - Index) :=
                        (others => Pad);
         begin
            Put (Padding);
         end;
      end if;

      for I in reverse 1 .. Index loop
         Put (Buffer (I));
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Words.Word_4) is
      use Rose.Words;
   begin
      if Item > 9 then
         Put (Character'Val (Word_8 (Item) - 10 + 65));
      else
         Put (Character'Val (Word_8 (Item) + 48));
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Words.Word_8) is
      use Rose.Words;
   begin
      Put (Rose.Words.Word_4 (Item / 16));
      Put (Rose.Words.Word_4 (Item mod 16));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Words.Word_16) is
      use Rose.Words;
   begin
      Put (Rose.Words.Word_8 (Item / 256));
      Put (Rose.Words.Word_8 (Item mod 256));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Words.Word_32) is
      use Rose.Words;
      N : Word_32 := Item;
      T : Word_32;
      Buffer : String (1 .. 9);
   begin
      for I in reverse 1 .. 9 loop
         if I = 5 then
            Buffer (I) := '_';
         else
            T := N mod 16;
            N := N / 16;
            if T > 9 then
               Buffer (I) := Character'Val (T - 10 + 65);
            else
               Buffer (I) := Character'Val (48 + T);
            end if;
         end if;
      end loop;
      Put (Buffer);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Text : String) is
   begin
      Put (Text);
      New_Line;
   end Put_Line;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer is
   begin
      Rose.System_Calls.Client.Send_String
        (Con_Cap, Text_Buffer (1 .. Buffer_Length));
      Buffer_Length := 0;
   end Send_Buffer;

end Rose.Console_IO;
