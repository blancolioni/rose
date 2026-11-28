with Ada.Command_Line;
with Ada.Streams;                use Ada.Streams;
with Ada.Text_IO;

with Rose.Surface_Events;        use Rose.Surface_Events;
with Rose.Words;                 use Rose.Words;

procedure Test_Surface_Events is

   --  The tests of a surface's event queue (#583), run on the host. Each
   --  check is reported, and the exit status is 1 if any failed.

   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Name : String; Ok : Boolean);

   function Take_All
     (Queue : in out Event_Queue)
      return Stream_Element_Array;
   --  Every event queued, as Take writes them

   function Kind_Of
     (Events : Stream_Element_Array;
      Index  : Positive)
      return Word_32
   is (Word_At (Events, Events'First
                + Stream_Element_Offset ((Index - 1) * Event_Bytes)));

   function Field
     (Events : Stream_Element_Array;
      Index  : Positive;
      Number : Positive)
      return Word_32
   is (Word_At (Events, Events'First
                + Stream_Element_Offset ((Index - 1) * Event_Bytes
                                         + 4 * Number)));

   -----------
   -- Check --
   -----------

   procedure Check (Name : String; Ok : Boolean) is
   begin
      Checks := Checks + 1;
      if not Ok then
         Failures := Failures + 1;
      end if;
      Ada.Text_IO.Put_Line ((if Ok then "ok:   " else "FAIL: ") & Name);
   end Check;

   --------------
   -- Take_All --
   --------------

   function Take_All
     (Queue : in out Event_Queue)
      return Stream_Element_Array
   is
      Buffer : Stream_Element_Array (1 .. Max_Events * Event_Bytes);
      Last   : Stream_Element_Offset;
   begin
      Take (Queue, Buffer, Last);
      return Buffer (1 .. Last);
   end Take_All;

begin

   declare
      Q : Event_Queue;
   begin
      Check ("a new queue is empty", Is_Empty (Q));
      Add (Q, Key_Event, 16#61#, 2);
      Add (Q, Size_Event, 640, 480);
      Check ("two events", Length (Q) = 2);
      declare
         E : constant Stream_Element_Array := Take_All (Q);
      begin
         Check ("Take gives whole events, oldest first",
                E'Length = 2 * Event_Bytes
                and then Kind_Of (E, 1) = Key_Event
                and then Field (E, 1, 1) = 16#61#
                and then Field (E, 1, 2) = 2
                and then Kind_Of (E, 2) = Size_Event
                and then Field (E, 2, 1) = 640
                and then Field (E, 2, 2) = 480);
      end;
      Check ("Take empties the queue", Is_Empty (Q));
      declare
         Bytes : constant Stream_Element_Array (1 .. 4) :=
                   (16#04#, 16#03#, 16#02#, 16#01#);
      begin
         Check ("the bytes are in the machine's order (little-endian)",
                Word_At (Bytes, 1) = 16#0102_0304#);
      end;
   end;

   declare
      Q      : Event_Queue;
      Small  : Stream_Element_Array (1 .. Event_Bytes + 5);
      Tiny   : Stream_Element_Array (1 .. Event_Bytes - 1);
      Last   : Stream_Element_Offset;
   begin
      Add (Q, Key_Event, 1);
      Add (Q, Key_Event, 2);
      Take (Q, Tiny, Last);
      Check ("a buffer too small for one event takes none",
             Last = Tiny'First - 1 and then Length (Q) = 2);
      Take (Q, Small, Last);
      Check ("a buffer takes only whole events",
             Last = Event_Bytes and then Length (Q) = 1);
      Clear (Q);
      Check ("Clear empties the queue", Is_Empty (Q));
   end;

   declare
      Q : Event_Queue;
   begin
      for I in 1 .. Max_Events + 3 loop
         Add (Q, Key_Event, Word_32 (I));
      end loop;
      declare
         E : constant Stream_Element_Array := Take_All (Q);
      begin
         Check ("a full queue drops the oldest",
                E'Length = Max_Events * Event_Bytes
                and then Field (E, 1, 1) = 4
                and then Field (E, Max_Events, 1) = Max_Events + 3);
      end;
   end;

   declare
      Q : Event_Queue;
   begin
      Add_Pointer (Q, 1, 1, 0);
      Add_Pointer (Q, 2, 3, 0);
      Add_Pointer (Q, 4, 5, 0);
      Check ("movement with the same buttons overwrites",
             Length (Q) = 1);
      Add_Pointer (Q, 4, 5, 1);
      Check ("a change of buttons is a new event", Length (Q) = 2);
      Add (Q, Wheel_Event, 1);
      Add_Pointer (Q, 6, 7, 1);
      Check ("movement after another kind is a new event",
             Length (Q) = 4);
      declare
         E : constant Stream_Element_Array := Take_All (Q);
      begin
         Check ("the overwritten event has the newest position",
                Kind_Of (E, 1) = Pointer_Event
                and then Field (E, 1, 1) = 4
                and then Field (E, 1, 2) = 5
                and then Field (E, 1, 3) = 0);
      end;
      Add_Pointer (Q, 9, 9, 1);
      Check ("after a Take, movement is a new event", Length (Q) = 1);
   end;

   Ada.Text_IO.Put_Line
     (Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Surface_Events;
