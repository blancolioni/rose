with Ada.Streams;

with Rose.Capabilities;

with Text_Console;

package Surface_Console.Screen is

   --  The console's cells, drawn with the Spleen font into the surface's
   --  buffer, which the console and the surface's server share (ADR
   --  0025). Each Put reports the rows that it changed with Damage.
   --  After a restore the screen driver redraws the screen from the
   --  buffer, so the console has nothing to do.

   procedure Open;
   --  Open the surface, map its buffer, size the cells to it, and draw
   --  them.

   procedure Put (Text : String);
   --  Write Text, as Text_Console.Cells.Put does.

   procedure Operate (Operation : Text_Console.Screen_Operation);
   --  One of Terminal's screen operations (#734).

   function Columns return Positive;
   function Rows return Positive;

   procedure Notify (Signal : Rose.Capabilities.Capability);
   --  Give the surface Signal, to invoke when it has events.

   procedure Read_Events
     (Buffer : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  The events that are ready: once the surface has a signal cap, this
   --  never waits, and returns nothing if there are none.

end Surface_Console.Screen;
