package Rose.Texts is

   --  Strings of a bounded length. The user-mode IDL target passes out
   --  and in out strings as texts, since the userland run-time system has
   --  no Ada.Strings.Bounded. To_String returns its result on the
   --  secondary stack, so this package is apart from Rose.Strings, which
   --  programs without a secondary stack use.

   Max_Length : constant := 1024;

   type Text is private;

   Null_Text : constant Text;

   function To_Text (Item : String) return Text;
   --  Item, or as much of it as fits.

   function To_String (Item : Text) return String;

   function Length (Item : Text) return Natural;

   procedure Copy
     (From : String;
      To   : out String;
      Last : out Natural);
   --  Copy as much of From as fits to the start of To. Last is the index
   --  in To of the last character copied, or To'First - 1 if there are
   --  none.

   procedure Copy
     (From : Text;
      To   : out String;
      Last : out Natural);
   --  The same for the characters of From, without making a string.

private

   subtype Text_Length is Natural range 0 .. Max_Length;

   type Text is
      record
         Length : Text_Length := 0;
         Data   : String (1 .. Max_Length);
      end record;

   Null_Text : constant Text := (Length => 0, Data => (others => ' '));

   function To_String (Item : Text) return String
   is (Item.Data (1 .. Item.Length));

   function Length (Item : Text) return Natural
   is (Item.Length);

end Rose.Texts;
