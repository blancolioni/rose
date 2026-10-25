with Rose.Invocation;
with Rose.System_Calls;
with Rose.Words;

package body Rose.Trace is

   Trace_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;

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
      First       : Positive := Digits_Text'Last + 1;
      Rest        : Natural := Value;
   begin
      loop
         First := First - 1;
         Digits_Text (First) := Character'Val (48 + Rest mod 10);
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
      Event (Text & Digits_Text (First .. Digits_Text'Last));
   end Event;

   -------------------
   -- Set_Trace_Cap --
   -------------------

   procedure Set_Trace_Cap (Cap : Rose.Capabilities.Capability) is
   begin
      Trace_Cap := Cap;
   end Set_Trace_Cap;

end Rose.Trace;
