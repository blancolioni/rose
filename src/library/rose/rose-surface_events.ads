with Ada.Streams;

with Rose.Words;

package Rose.Surface_Events with Pure is

   use type Ada.Streams.Stream_Element_Offset;

   --  A queue of a surface's events, until its holder reads them (ADR
   --  0022, interfaces/screens-surface.idl). An event is four 32-bit
   --  words, in the machine's byte order: its kind, then three words
   --  whose meaning depends on it. The screen driver keeps one for the
   --  screen, the display server one for each of its surfaces, and the
   --  test surface one for itself (#583).

   Key_Event     : constant := 1;
   Pointer_Event : constant := 2;
   Focus_Event   : constant := 3;
   Size_Event    : constant := 4;
   Wheel_Event   : constant := 5;

   Event_Bytes : constant := 16;

   Max_Events : constant := 256;

   type Event_Queue is private;
   --  Empty until something is added.

   procedure Add
     (Queue         : in out Event_Queue;
      Kind          : Rose.Words.Word_32;
      First, Second : Rose.Words.Word_32 := 0;
      Third         : Rose.Words.Word_32 := 0);
   --  Queue an event. If the queue is full, the oldest event is dropped.

   procedure Add_Pointer
     (Queue   : in out Event_Queue;
      X, Y    : Rose.Words.Word_32;
      Buttons : Rose.Words.Word_32);
   --  Queue a pointer event; or, if the newest event queued is a pointer
   --  event with the same buttons, overwrite it, so that movement alone
   --  can't fill the queue (#588).

   function Is_Empty (Queue : Event_Queue) return Boolean;

   function Length (Queue : Event_Queue) return Natural;

   procedure Clear (Queue : in out Event_Queue);

   procedure Take
     (Queue  : in out Event_Queue;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Take as many whole events as fit in Buffer, oldest first. Last is
   --  Buffer'First - 1 if none are queued, or Buffer is too small for
   --  one.

   function Word_At
     (Buffer : Ada.Streams.Stream_Element_Array;
      Offset : Ada.Streams.Stream_Element_Offset)
      return Rose.Words.Word_32
     with Pre => Offset >= Buffer'First and then Offset + 3 <= Buffer'Last;
   --  The word at Offset in a buffer of events that Take filled: the
   --  kind of an event at its first byte, then its words at 4, 8 and 12.

private

   type Event is array (1 .. 4) of Rose.Words.Word_32;

   type Event_Array is array (0 .. Max_Events - 1) of Event;

   type Event_Queue is
      record
         Events : Event_Array := (others => (others => 0));
         Head   : Natural := 0;
         Count  : Natural := 0;
      end record;
   --  The oldest event is Events (Head), and Count are queued.

end Rose.Surface_Events;
