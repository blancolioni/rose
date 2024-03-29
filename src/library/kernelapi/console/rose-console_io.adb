with Rose.Interfaces.Stream_Writer.Client;
with Rose.System_Calls;

package body Rose.Console_IO is

   Client : Rose.Interfaces.Stream_Writer.Client.Stream_Writer_Client;

   Text_Buffer   : String (1 .. 4096)
     with Alignment => 4096;

   Invoke_Buffer : String (1 .. 4096)
     with Alignment => 4096;

   Buffer_Length : Natural := 0;

   procedure Send_Buffer;

   -----------
   -- Flush --
   -----------

   procedure Flush is
   begin
      Send_Buffer;
   end Flush;

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
      Rose.Interfaces.Stream_Writer.Client.Open (Client, Console_Cap);
      Buffer_Length := 0;
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

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Words.Word_64) is
      use type Rose.Words.Word_64;
   begin
      Put (Rose.Words.Word_32 (Item / 2 ** 32));
      Put ("_");
      Put (Rose.Words.Word_32 (Item mod 2 ** 32));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Rose.Objects.Object_Id) is
   begin
      Put (Rose.Words.Word_64 (Item));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : System.Storage_Elements.Storage_Array) is
      use Rose.Words;
      Offset : Word_16 := 0;
   begin
      for X of Item loop
         if Offset mod 16 = 0 then
            Put (Offset);
            Put (":");
         end if;
         Put (" ");
         Put (Word_8 (X));
         Offset := Offset + 1;
         if Offset mod 16 = 0 then
            New_Line;
         end if;
      end loop;

      if Offset mod 16 /= 0 then
         New_Line;
      end if;
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
      use System.Storage_Elements;
      Storage_Buffer : Storage_Array (1 .. Storage_Count (Buffer_Length));
      for Storage_Buffer'Address use Text_Buffer'Address;
      pragma Import (Ada, Storage_Buffer);
   begin
      if not Rose.System_Calls.Have_Buffer then
         Rose.System_Calls.Use_Buffer
           (Buffer_Address => Invoke_Buffer'Address,
            Buffer_Size  => 4096);
      end if;

      Rose.Interfaces.Stream_Writer.Client.Write
        (Item   => Client,
         Buffer => Storage_Buffer);
      Buffer_Length := 0;
   end Send_Buffer;

end Rose.Console_IO;
