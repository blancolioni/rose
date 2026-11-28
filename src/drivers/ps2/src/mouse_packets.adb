package body Mouse_Packets is

   use Interfaces;

   Always_One   : constant Byte := 16#08#;
   X_Sign       : constant Byte := 16#10#;
   Y_Sign       : constant Byte := 16#20#;
   Overflow     : constant Byte := 16#C0#;
   Button_Bits  : constant Byte := 16#07#;

   function Signed
     (Low  : Byte;
      Sign : Boolean)
      return Integer_32
   is (Integer_32 (Low) - (if Sign then 256 else 0));
   --  A 9-bit signed value, from its low 8 bits and its sign

   ---------
   -- Add --
   ---------

   procedure Add
     (Item  : in out Combiner;
      Event : Pointer_Event;
      Ready : out Event_Array;
      Last  : out Natural)
   is
   begin
      Ready := (others => (others => <>));
      Last := Ready'First - 1;

      if Event.Buttons = Item.Buttons then
         if Item.Held then
            Item.Sum.DX := Item.Sum.DX + Event.DX;
            Item.Sum.DY := Item.Sum.DY + Event.DY;
            Item.Sum.DZ := Item.Sum.DZ + Event.DZ;
         else
            Item.Sum := Event;
            Item.Held := True;
         end if;
      else
         if Item.Held then
            Last := Last + 1;
            Ready (Last) := Item.Sum;
            Item.Held := False;
         end if;
         Last := Last + 1;
         Ready (Last) := Event;
         Item.Buttons := Event.Buttons;
      end if;
   end Add;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Item  : in out Combiner;
      Event : out Pointer_Event;
      Ready : out Boolean)
   is
   begin
      Ready := Item.Held;
      Event := (if Item.Held then Item.Sum else (others => <>));
      Item.Held := False;
   end Flush;

   ---------
   -- Put --
   ---------

   procedure Put
     (Item     : in out Decoder;
      Value    : Byte;
      Complete : out Boolean;
      Event    : out Pointer_Event)
   is
   begin
      Complete := False;
      Event := (others => <>);

      if Item.Next = 0 and then (Value and Always_One) = 0 then
         return;
      end if;

      Item.Bytes (Item.Next) := Value;
      Item.Next := Item.Next + 1;

      if Item.Next < Item.Size then
         return;
      end if;

      Item.Next := 0;

      declare
         First : constant Byte := Item.Bytes (0);
         Wheel : constant Byte := Item.Bytes (3) and 16#0F#;
      begin
         if (First and Overflow) /= 0 then
            return;
         end if;

         Event.DX := Signed (Item.Bytes (1), (First and X_Sign) /= 0);
         Event.DY := -Signed (Item.Bytes (2), (First and Y_Sign) /= 0);
         if Item.Size = 4 then
            Event.DZ :=
              (if Wheel >= 8 then Integer_32 (Wheel) - 16
               else Integer_32 (Wheel));
         end if;
         Event.Buttons := Unsigned_32 (First and Button_Bits);
         Complete := True;
      end;
   end Put;

   -----------
   -- Reset --
   -----------

   procedure Reset (Item : out Combiner) is
   begin
      Item := (Buttons => 0, Held => False, Sum => (others => <>));
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Item  : in out Decoder;
      Wheel : Boolean)
   is
   begin
      Item := (Size  => (if Wheel then 4 else 3),
               Next  => 0,
               Bytes => (others => 0));
   end Reset;

end Mouse_Packets;
