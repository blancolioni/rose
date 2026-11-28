with Ada.Streams;

with Rose.Words;

package Screen.Events is

   --  The surface's events, queued until its holder reads them (ADR 0022,
   --  surface.idl). An event is four 32-bit words, in the machine's byte
   --  order: its kind, then three words whose meaning depends on it.

   Key_Event     : constant := 1;
   Pointer_Event : constant := 2;
   Wheel_Event   : constant := 5;

   Event_Bytes : constant := 16;

   procedure Add
     (Kind              : Rose.Words.Word_32;
      First, Second     : Rose.Words.Word_32 := 0;
      Third             : Rose.Words.Word_32 := 0);
   --  Queue an event. If the queue is full, the oldest event is dropped.

   procedure Add_Pointer
     (X, Y    : Rose.Words.Word_32;
      Buttons : Rose.Words.Word_32);
   --  Queue a pointer event; or, if the newest event queued is a pointer
   --  event with the same buttons, overwrite it, so that movement alone
   --  can't fill the queue (#588).

   function Is_Empty return Boolean;

   procedure Take
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Take as many whole events as fit in Buffer, oldest first. Last is
   --  Buffer'First - 1 if none are queued, or Buffer is too small for
   --  one.

end Screen.Events;
