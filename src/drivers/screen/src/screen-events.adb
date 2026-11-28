package body Screen.Events is

   use Rose.Words;
   use Ada.Streams;

   Max_Events : constant := 256;

   type Event is array (1 .. 4) of Word_32;

   Queue : array (0 .. Max_Events - 1) of Event;
   Head  : Natural := 0;
   Count : Natural := 0;
   --  The oldest event is Queue (Head), and Count are queued.

   ---------
   -- Add --
   ---------

   procedure Add
     (Kind              : Rose.Words.Word_32;
      First, Second     : Rose.Words.Word_32 := 0;
      Third             : Rose.Words.Word_32 := 0)
   is
   begin
      if Count = Max_Events then
         Head := (Head + 1) mod Max_Events;
         Count := Count - 1;
      end if;

      Queue ((Head + Count) mod Max_Events) := (Kind, First, Second, Third);
      Count := Count + 1;
   end Add;

   -----------------
   -- Add_Pointer --
   -----------------

   procedure Add_Pointer
     (X, Y    : Rose.Words.Word_32;
      Buttons : Rose.Words.Word_32)
   is
   begin
      if Count > 0 then
         declare
            Newest : Event renames
                       Queue ((Head + Count - 1) mod Max_Events);
         begin
            if Newest (1) = Pointer_Event and then Newest (4) = Buttons then
               Newest := (Pointer_Event, X, Y, Buttons);
               return;
            end if;
         end;
      end if;

      Add (Pointer_Event, X, Y, Buttons);
   end Add_Pointer;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty return Boolean is (Count = 0);

   ----------
   -- Take --
   ----------

   procedure Take
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Fits : constant Natural :=
               Natural'Min (Count, Natural (Buffer'Length / Event_Bytes));
   begin
      Buffer := (others => 0);
      Last := Buffer'First - 1;

      for I in 1 .. Fits loop
         for Value of Queue (Head) loop
            for Byte in 0 .. 3 loop
               Last := Last + 1;
               Buffer (Last) :=
                 Stream_Element (Value / 2 ** (8 * Byte) mod 256);
            end loop;
         end loop;
         Head := (Head + 1) mod Max_Events;
         Count := Count - 1;
      end loop;
   end Take;

end Screen.Events;
