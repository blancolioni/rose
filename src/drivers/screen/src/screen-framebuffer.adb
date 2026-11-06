with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;

package body Screen.Framebuffer is

   use Rose.Words;
   use System.Storage_Elements;

   Window_Address : constant := 16#E000_0000#;
   --  Where the framebuffer is mapped: the start of the device window.

   Tag : Storage_Array (0 .. 39);
   --  The framebuffer tag, up to the end of its colour information.

   Screen_Width  : Word := 0;
   Screen_Height : Word := 0;
   Pitch         : Word := 0;
   --  The bytes from the start of one row of the framebuffer to the next.

   type Field is
      record
         Shift : Word_32;
         Bits  : Word_32;
      end record;
   --  Where a colour is in a framebuffer pixel, and how many bits it has.

   Red, Green, Blue : Field := (0, 8);

   Native : Boolean := True;
   --  True if the framebuffer's pixels are XRGB, like the buffer's, so
   --  that a row can be copied as it is.

   type Pixel_Array is array (Word range <>) of Word_32;

   function Convert (Pixel : Word_32) return Word_32;
   --  Pixel, which is XRGB, in the framebuffer's format.

   procedure Map (Success : out Boolean);
   --  Map the framebuffer, and set Success to False if the kernel refuses.

   -------------
   -- Convert --
   -------------

   function Convert (Pixel : Word_32) return Word_32 is

      function Place (Value : Word_32; Item : Field) return Word_32
      is (Value / 2 ** Natural (8 - Item.Bits) * 2 ** Natural (Item.Shift));

   begin
      return Place (Pixel / 2 ** 16 mod 256, Red)
        or Place (Pixel / 2 ** 8 mod 256, Green)
        or Place (Pixel mod 256, Blue);
   end Convert;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Buffer  : System.Address;
      X, Y    : Rose.Words.Word;
      Columns : Rose.Words.Word;
      Rows    : Rose.Words.Word)
   is
   begin
      if Columns = 0 or else Rows = 0 then
         return;
      end if;

      for Row in Y .. Y + Rows - 1 loop
         declare
            Source : Pixel_Array (1 .. Columns)
              with Import,
                   Address =>
                     Buffer
                     + Storage_Offset ((Row * Screen_Width + X) * 4);
            Target : Pixel_Array (1 .. Columns)
              with Import,
                   Address =>
                     To_Address
                       (Window_Address
                        + Integer_Address (Row * Pitch + X * 4));
         begin
            if Native then
               Target := Source;
            else
               for I in Source'Range loop
                  Target (I) := Convert (Source (I));
               end loop;
            end if;
         end;
      end loop;
   end Copy;

   ------------
   -- Height --
   ------------

   function Height return Rose.Words.Word is (Screen_Height);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Success : out Boolean) is

      function Get_Word (Offset : Storage_Offset) return Word;
      --  The little-endian word at Offset in Tag

      function Get_Field (Offset : Storage_Offset) return Field
      is ((Word_32 (Tag (Offset)), Word_32 (Tag (Offset + 1))));
      --  A colour's position and size, at Offset in Tag

      function Bad (Item : Field) return Boolean
      is (Item.Bits = 0 or else Item.Bits > 8
          or else Item.Shift + Item.Bits > 32);
      --  True if a colour can't be converted to

      --------------
      -- Get_Word --
      --------------

      function Get_Word (Offset : Storage_Offset) return Word is
         Result : Word := 0;
      begin
         for I in reverse Offset .. Offset + 3 loop
            Result := Result * 256 + Word (Tag (I));
         end loop;
         return Result;
      end Get_Word;

      Params : aliased Rose.Invocation.Invocation_Record;
      Size   : Word := 0;
   begin
      Success := False;

      Rose.System_Calls.Initialize_Send (Params, Geometry_Cap);
      Rose.System_Calls.Send_Buffer
        (Params, Tag'Length, Tag'Address, Writable => True);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if not Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Control.Flags (Rose.Invocation.Send_Words)
      then
         Size := Params.Data (0);
      end if;

      --  The tag: the address as 64 bits at 8, the pitch at 16, the width
      --  at 20, the height at 24, the bits per pixel at 28 and the type at
      --  29, which is 1 for RGB. Then, for RGB, each colour's position and
      --  size, from 32.

      if Size < 38
        or else Tag (28) /= 32
        or else Tag (29) /= 1
        or else Get_Word (12) /= 0
        or else Get_Word (20) = 0
        or else Get_Word (24) = 0
        or else Get_Word (16) < Get_Word (20) * 4
      then
         return;
      end if;

      Red := Get_Field (32);
      Green := Get_Field (34);
      Blue := Get_Field (36);

      if Bad (Red) or else Bad (Green) or else Bad (Blue) then
         return;
      end if;

      Map (Success);
      if not Success then
         return;
      end if;

      Native := Red = (16, 8) and then Green = (8, 8) and then Blue = (0, 8);
      Pitch := Get_Word (16);
      Screen_Width := Get_Word (20);
      Screen_Height := Get_Word (24);
   end Initialize;

   ---------
   -- Map --
   ---------

   procedure Map is
      Success : Boolean;
   begin
      Map (Success);
   end Map;

   procedure Map (Success : out Boolean) is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, Framebuffer_Cap);
      Rose.System_Calls.Send_Word (Params, Word'(Window_Address));
      Rose.System_Calls.Invoke_Capability (Params);
      Success := not Params.Control.Flags (Rose.Invocation.Error);
   end Map;

   -----------
   -- Width --
   -----------

   function Width return Rose.Words.Word is (Screen_Width);

end Screen.Framebuffer;
