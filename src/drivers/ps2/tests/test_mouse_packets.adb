with Ada.Command_Line;
with Ada.Text_IO;

with Interfaces;                 use Interfaces;

with Mouse_Packets;              use Mouse_Packets;

procedure Test_Mouse_Packets is

   --  The tests of the PS/2 mouse's packet decoder and combiner (#588),
   --  run on the host. Each check is reported, and the exit status is 1
   --  if any failed.

   Failures : Natural := 0;
   Checks   : Natural := 0;

   type Byte_Array is array (Positive range <>) of Byte;
   type Result_Array is array (Positive range <>) of Pointer_Event;

   procedure Check (Name : String; Ok : Boolean);

   procedure Decode
     (Name     : String;
      Wheel    : Boolean;
      Input    : Byte_Array;
      Expected : Result_Array);
   --  Put Input into a new decoder, and check that the packets it
   --  completes are Expected

   function Image (Event : Pointer_Event) return String;

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

   ------------
   -- Decode --
   ------------

   procedure Decode
     (Name     : String;
      Wheel    : Boolean;
      Input    : Byte_Array;
      Expected : Result_Array)
   is
      D      : Decoder;
      Got    : Result_Array (1 .. Input'Length);
      Count  : Natural := 0;
      Done   : Boolean;
      Event  : Pointer_Event;
   begin
      Reset (D, Wheel);
      for Value of Input loop
         Put (D, Value, Done, Event);
         if Done then
            Count := Count + 1;
            Got (Count) := Event;
         end if;
      end loop;

      declare
         Same : Boolean := Count = Expected'Length;
      begin
         if Same then
            for I in 1 .. Count loop
               Same := Same
                 and then Got (I) = Expected (Expected'First + I - 1);
            end loop;
         end if;
         Check (Name, Same);
         if not Same then
            for I in 1 .. Count loop
               Ada.Text_IO.Put_Line ("      got " & Image (Got (I)));
            end loop;
         end if;
      end;
   end Decode;

   -----------
   -- Image --
   -----------

   function Image (Event : Pointer_Event) return String is
   begin
      return Event.DX'Image & Event.DY'Image & Event.DZ'Image
        & Event.Buttons'Image;
   end Image;

   function E
     (DX, DY : Integer_32;
      DZ     : Integer_32 := 0;
      B      : Unsigned_32 := 0)
      return Pointer_Event
   is (DX, DY, DZ, B);

begin

   --  Decoding.

   Decode ("still, no buttons", False,
           (16#08#, 0, 0), (1 => E (0, 0)));
   Decode ("right and up", False,
           (16#08#, 5, 3), (1 => E (5, -3)));
   Decode ("left and down: the signs", False,
           (16#38#, 16#FB#, 16#FD#), (1 => E (-5, 3)));
   Decode ("the largest movements", False,
           (16#28#, 16#FF#, 16#00#, 16#18#, 16#00#, 16#FF#),
           (E (255, 256), E (-256, -255)));
   Decode ("buttons: left, right, middle", False,
           (16#09#, 0, 0, 16#0A#, 0, 0, 16#0C#, 0, 0, 16#0F#, 0, 0),
           (E (0, 0, B => 1), E (0, 0, B => 2), E (0, 0, B => 4),
            E (0, 0, B => 7)));
   Decode ("an X overflow drops the packet", False,
           (16#48#, 16#FF#, 0, 16#08#, 1, 1), (1 => E (1, -1)));
   Decode ("a Y overflow drops the packet", False,
           (16#88#, 0, 16#FF#, 16#08#, 2, 2), (1 => E (2, -2)));
   Decode ("bytes without bit 3 are dropped until a packet starts", False,
           (16#05#, 16#03#, 16#08#, 1, 2), (1 => E (1, -2)));
   Decode ("starting in the middle of a packet, then in step", False,
           (16#02#, 16#09#, 7, 7, 16#08#, 1, 1),
           (E (7, -7, B => 1), E (1, -1)));
   Decode ("three-byte packets with no wheel", False,
           (16#08#, 1, 1, 16#08#, 2, 2), (E (1, -1), E (2, -2)));
   Decode ("wheel: towards the user", True,
           (16#08#, 0, 0, 16#01#), (1 => E (0, 0, 1)));
   Decode ("wheel: away from the user", True,
           (16#08#, 0, 0, 16#0F#), (1 => E (0, 0, -1)));
   Decode ("wheel: the largest steps, and the high bits ignored", True,
           (16#08#, 0, 0, 16#F7#, 16#08#, 0, 0, 16#08#),
           (E (0, 0, 7), E (0, 0, -8)));
   Decode ("wheel packets with movement", True,
           (16#19#, 16#FE#, 4, 16#02#), (1 => E (-2, -4, 2, 1)));

   --  Reset starts a new packet.

   declare
      D     : Decoder;
      Done  : Boolean;
      Event : Pointer_Event;
   begin
      Reset (D, False);
      Put (D, 16#08#, Done, Event);
      Put (D, 9, Done, Event);
      Reset (D, False);
      Put (D, 16#08#, Done, Event);
      Put (D, 3, Done, Event);
      Put (D, 4, Done, Event);
      Check ("Reset drops a part packet", Done and then Event = E (3, -4));
   end;

   --  Combining.

   declare
      C     : Combiner;
      Ready : Event_Array (1 .. 2);
      Last  : Natural;
      Held  : Boolean;
      Event : Pointer_Event;
   begin
      Add (C, E (1, 2), Ready, Last);
      Check ("movement is held", Last = 0);
      Add (C, E (3, -1, 1), Ready, Last);
      Check ("movement with the same buttons is added up",
             Last = 0);
      Add (C, E (-2, 4, 0, 1), Ready, Last);
      Check ("a button change sends what was held, then itself",
             Last = 2
             and then Ready (1) = E (4, 1, 1)
             and then Ready (2) = E (-2, 4, 0, 1));
      Flush (C, Event, Held);
      Check ("nothing is held after a button change", not Held);
      Add (C, E (5, 5, 0, 1), Ready, Last);
      Add (C, E (1, 0, 0, 1), Ready, Last);
      Check ("movement after a change is held", Last = 0);
      Flush (C, Event, Held);
      Check ("Flush sends the movement held",
             Held and then Event = E (6, 5, 0, 1));
      Flush (C, Event, Held);
      Check ("a second Flush sends nothing", not Held);
      Add (C, E (0, 0, 0, 0), Ready, Last);
      Check ("releasing the button is sent at once",
             Last = 1 and then Ready (1) = E (0, 0));
      Add (C, E (0, 0, 0, 1), Ready, Last);
      Add (C, E (0, 0, 0, 0), Ready, Last);
      Check ("a click is two events", Last = 1 and then Ready (1) = E (0, 0));
   end;

   Ada.Text_IO.Put_Line
     (Checks'Image & " checks," & Failures'Image & " failed");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Test_Mouse_Packets;
