with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

package body Rose.Trace is

   Max_Text : constant := 64;
   --  The kernel keeps only the first 64 characters of an event.

   procedure Append
     (Line : in out String;
      Last : in out Natural;
      Text : String);
   --  Add as much of Text to Line (1 .. Last) as fits. Events are built
   --  in a String of Max_Text, not by "&", whose result the stack would
   --  hold at a size that is not known until it runs (#1004).

   ------------
   -- Append --
   ------------

   procedure Append
     (Line : in out String;
      Last : in out Natural;
      Text : String)
   is
   begin
      for Ch of Text loop
         exit when Last = Line'Last;
         Last := Last + 1;
         Line (Last) := Ch;
      end loop;
   end Append;

   Trace_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

   procedure Put_Image
     (Value : Natural;
      Text  : out String;
      First : out Positive);
   --  Value in decimal, at the end of Text, from First.

   -----------
   -- Event --
   -----------

   procedure Event (Text : String) is
      use type Rose.Capabilities.Capability;
      use type Rose.Words.Word;
      Params : aliased Rose.Invocation.Invocation_Record;
      Word   : Rose.Words.Word := 0;
      Shift  : Rose.Words.Word := 1;
      Words  : Natural := 0;
   begin
      if Trace_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      --  The text goes in the data words, four characters to a word, the
      --  first in the low byte, as the kernel decodes names.

      Rose.System_Calls.Initialize_Send (Params, Trace_Cap);
      for Ch of Text loop
         exit when Words = Rose.Invocation.Max_Parameter_Words;
         Word := Word + Character'Pos (Ch) * Shift;
         if Shift = 2 ** 24 then
            Rose.System_Calls.Send_Word (Params, Word);
            Words := Words + 1;
            Word := 0;
            Shift := 1;
         else
            Shift := Shift * 256;
         end if;
      end loop;
      if Shift /= 1 and then Words < Rose.Invocation.Max_Parameter_Words then
         Rose.System_Calls.Send_Word (Params, Word);
      end if;
      Rose.System_Calls.Invoke_Capability (Params);
   end Event;

   -----------
   -- Event --
   -----------

   procedure Event
     (Text  : String;
      Value : Natural)
   is
      Digits_Text : String (1 .. 10);
      First       : Positive;
      Line        : String (1 .. Max_Text);
      Last        : Natural := 0;
   begin
      Put_Image (Value, Digits_Text, First);
      Append (Line, Last, Text);
      Append (Line, Last, Digits_Text (First .. Digits_Text'Last));
      Event (Line (1 .. Last));
   end Event;

   -----------
   -- Event --
   -----------

   procedure Event
     (Text    : String;
      Value   : Natural;
      Text_2  : String;
      Value_2 : Natural)
   is
      Digits_1 : String (1 .. 10);
      Digits_2 : String (1 .. 10);
      First_1  : Positive;
      First_2  : Positive;
      Line     : String (1 .. Max_Text);
      Last     : Natural := 0;
   begin
      Put_Image (Value, Digits_1, First_1);
      Put_Image (Value_2, Digits_2, First_2);
      Append (Line, Last, Text);
      Append (Line, Last, Digits_1 (First_1 .. Digits_1'Last));
      Append (Line, Last, Text_2);
      Append (Line, Last, Digits_2 (First_2 .. Digits_2'Last));
      Event (Line (1 .. Last));
   end Event;

   -----------
   -- Event --
   -----------

   procedure Event
     (Text   : String;
      Text_2 : String)
   is
      Line : String (1 .. Max_Text);
      Last : Natural := 0;
   begin
      Append (Line, Last, Text);
      Append (Line, Last, Text_2);
      Event (Line (1 .. Last));
   end Event;

   ---------------
   -- Put_Image --
   ---------------

   procedure Put_Image
     (Value : Natural;
      Text  : out String;
      First : out Positive)
   is
      Rest : Natural := Value;
   begin
      Text := (others => ' ');
      First := Text'Last + 1;
      loop
         First := First - 1;
         Text (First) := Character'Val (48 + Rest mod 10);
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
   end Put_Image;

   -------------------
   -- Set_Trace_Cap --
   -------------------

   procedure Set_Trace_Cap (Cap : Rose.Capabilities.Capability) is
   begin
      Trace_Cap := Cap;
   end Set_Trace_Cap;

   -----------
   -- Ticks --
   -----------

   function Ticks return Natural is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Trace_Cap = Rose.Capabilities.Null_Capability then
         return 0;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Trace_Cap);
      Rose.System_Calls.Receive_Words (Params, 1);
      Rose.System_Calls.Invoke_Capability (Params);
      return Natural (Params.Data (0));
   end Ticks;

end Rose.Trace;
