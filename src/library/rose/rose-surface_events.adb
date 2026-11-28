package body Rose.Surface_Events is

   use Rose.Words;
   use Ada.Streams;

   ---------
   -- Add --
   ---------

   procedure Add
     (Queue         : in out Event_Queue;
      Kind          : Rose.Words.Word_32;
      First, Second : Rose.Words.Word_32 := 0;
      Third         : Rose.Words.Word_32 := 0)
   is
   begin
      if Queue.Count = Max_Events then
         Queue.Head := (Queue.Head + 1) mod Max_Events;
         Queue.Count := Queue.Count - 1;
      end if;

      Queue.Events ((Queue.Head + Queue.Count) mod Max_Events) :=
        (Kind, First, Second, Third);
      Queue.Count := Queue.Count + 1;
   end Add;

   -----------------
   -- Add_Pointer --
   -----------------

   procedure Add_Pointer
     (Queue   : in out Event_Queue;
      X, Y    : Rose.Words.Word_32;
      Buttons : Rose.Words.Word_32)
   is
   begin
      if Queue.Count > 0 then
         declare
            Newest : Event renames
                       Queue.Events
                         ((Queue.Head + Queue.Count - 1) mod Max_Events);
         begin
            if Newest (1) = Pointer_Event and then Newest (4) = Buttons then
               Newest := (Pointer_Event, X, Y, Buttons);
               return;
            end if;
         end;
      end if;

      Add (Queue, Pointer_Event, X, Y, Buttons);
   end Add_Pointer;

   -----------
   -- Clear --
   -----------

   procedure Clear (Queue : in out Event_Queue) is
   begin
      Queue.Head := 0;
      Queue.Count := 0;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Queue : Event_Queue) return Boolean
   is (Queue.Count = 0);

   ------------
   -- Length --
   ------------

   function Length (Queue : Event_Queue) return Natural
   is (Queue.Count);

   ----------
   -- Take --
   ----------

   procedure Take
     (Queue  : in out Event_Queue;
      Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Fits : constant Natural :=
               Natural'Min (Queue.Count,
                            Natural (Buffer'Length / Event_Bytes));
   begin
      Buffer := (others => 0);
      Last := Buffer'First - 1;

      for I in 1 .. Fits loop
         for Value of Queue.Events (Queue.Head) loop
            for Byte in 0 .. 3 loop
               Last := Last + 1;
               Buffer (Last) :=
                 Stream_Element (Value / 2 ** (8 * Byte) mod 256);
            end loop;
         end loop;
         Queue.Head := (Queue.Head + 1) mod Max_Events;
         Queue.Count := Queue.Count - 1;
      end loop;
   end Take;

   -------------
   -- Word_At --
   -------------

   function Word_At
     (Buffer : Ada.Streams.Stream_Element_Array;
      Offset : Ada.Streams.Stream_Element_Offset)
      return Rose.Words.Word_32
   is
      Result : Word_32 := 0;
   begin
      for I in reverse Offset .. Offset + 3 loop
         Result := Result * 256 + Word_32 (Buffer (I));
      end loop;
      return Result;
   end Word_At;

end Rose.Surface_Events;
