with Ada.Streams;

with Rose.Words;

package Virtio_Net.Device is

   --  The card itself: its legacy registers in BAR 0, and its two queues
   --  in the DMA pages, which are identity-mapped, so that an address in
   --  them is also the address to give the card (#291).
   --
   --  The pages hold, in order:
   --    10000 .. 12FFF  the receive queue (queue 0)
   --    13000 .. 15FFF  the transmit queue (queue 1)
   --    16000 .. 1DFFF  sixteen receive buffers of 2 KiB
   --    1E000 .. 1FFFF  four transmit buffers of 2 KiB
   --
   --  A queue of up to 256 entries fits in its three pages, with the used
   --  ring on a page of its own, as the legacy interface lays it out.
   --  Each buffer is two descriptors: the virtio-net header, and the
   --  frame, since a legacy card may expect them apart.

   Max_Frame_Size : constant := 1514;

   procedure Map_Pages;
   --  Map the DMA pages at their own addresses. Their mappings don't
   --  survive a restore, so this is done at start and when restored.

   procedure Start (Success : out Boolean);
   --  Reset the card, accept only the MAC feature, set up both queues,
   --  give the card every receive buffer, and set DRIVER_OK. Success is
   --  False, and the card is marked FAILED, if a queue is bigger than
   --  its pages allow.

   function MAC_Address return Rose.Words.Word_64;
   --  The card's address, the first byte on the wire in bits 47 .. 40

   procedure Interrupted;
   --  Read the ISR status, which tells the card that its interrupt has
   --  been seen, so that it drops the line, and take back the transmit
   --  buffers that it has finished with.

   function Frame_Waiting return Boolean;
   --  True if the card has put a frame in a receive buffer that hasn't
   --  been taken.

   procedure Send
     (Frame : Ada.Streams.Stream_Element_Array;
      Sent  : out Boolean);
   --  Queue Frame to send. Sent is False if it is empty or longer than
   --  Max_Frame_Size, or if every transmit buffer is in use.

   procedure Receive
     (Frame : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset);
   --  Take the oldest frame waiting, and give its buffer back to the
   --  card. Never waits: Last is Frame'First - 1 if no frame is waiting.
   --  A frame longer than Frame is cut short.

end Virtio_Net.Device;
