package body Rose.Texts is

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : String;
      To   : out String;
      Last : out Natural)
   is
      Count : constant Natural := Natural'Min (From'Length, To'Length);
   begin
      To (To'First .. To'First + Count - 1) :=
        From (From'First .. From'First + Count - 1);
      Last := To'First + Count - 1;
   end Copy;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (From : Text;
      To   : out String;
      Last : out Natural)
   is
   begin
      Copy (From.Data (1 .. From.Length), To, Last);
   end Copy;

   -------------
   -- To_Text --
   -------------

   function To_Text (Item : String) return Text is
      Last : Natural;
   begin
      return Result : Text := Null_Text do
         Copy (Item, Result.Data, Last);
         Result.Length := Last;
      end return;
   end To_Text;

end Rose.Texts;
