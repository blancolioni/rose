with Interfaces;

package Mouse_Packets with Pure is

   --  The packets of a PS/2 mouse (#588), as movement and buttons. It
   --  does no I/O, so that it is tested on the host ("make test-ps2").
   --
   --  A packet is three bytes, or four from a mouse with a wheel (device
   --  id 3, the IntelliMouse):
   --
   --     byte 0: bit 0 left, 1 right, 2 middle, 3 always 1, 4 the sign of
   --             X, 5 the sign of Y, 6 X overflowed, 7 Y overflowed
   --     byte 1: X, the low 8 bits of a 9-bit signed movement
   --     byte 2: Y, the same, with Y positive up
   --     byte 3: the wheel, in the low 4 bits, signed

   subtype Byte is Interfaces.Unsigned_8;

   type Pointer_Event is
      record
         DX, DY, DZ : Interfaces.Integer_32  := 0;
         Buttons    : Interfaces.Unsigned_32 := 0;
      end record;
   --  As Devices.Pointer_Listener.On_Pointer gives them: DX and DY are
   --  movement in the mouse's counts, with Y positive down; DZ is the
   --  wheel, positive towards the user; Buttons has bit 0 the left, 1 the
   --  right and 2 the middle.

   type Decoder is private;

   procedure Reset
     (Item  : in out Decoder;
      Wheel : Boolean);
   --  Expect the first byte of a packet next. Wheel: the packets are four
   --  bytes.

   procedure Put
     (Item     : in out Decoder;
      Value    : Byte;
      Complete : out Boolean;
      Event    : out Pointer_Event);
   --  The next byte from the mouse. Complete is True if it ends a packet,
   --  which is then in Event. A byte that should start a packet, but
   --  hasn't bit 3 set, is dropped, which brings a decoder that has lost
   --  its place back into step. A packet with an overflow bit set is
   --  dropped whole, since its movement is wrong.

   type Event_Array is array (Positive range <>) of Pointer_Event;

   type Combiner is private;
   --  Adds up the movement of packets with the same buttons, so that the
   --  listener is called once for a run of them. A change of buttons is
   --  never combined with the movement around it.

   procedure Add
     (Item  : in out Combiner;
      Event : Pointer_Event;
      Ready : out Event_Array;
      Last  : out Natural)
     with Pre => Ready'Length >= 2;
   --  Add a decoded packet. Ready (Ready'First .. Last) are the events to
   --  send now, oldest first: the movement held so far, if Event changes
   --  the buttons, and then Event itself.

   procedure Flush
     (Item  : in out Combiner;
      Event : out Pointer_Event;
      Ready : out Boolean);
   --  The movement held so far, if there is any (Ready), which is then
   --  sent: at the end of each pass of the interrupt handler.

private

   type Packet_Bytes is array (0 .. 3) of Byte;

   type Decoder is
      record
         Size  : Positive := 3;
         Next  : Natural := 0;
         Bytes : Packet_Bytes := (others => 0);
      end record;

   type Combiner is
      record
         Buttons : Interfaces.Unsigned_32 := 0;
         Held    : Boolean := False;
         Sum     : Pointer_Event;
      end record;

end Mouse_Packets;
