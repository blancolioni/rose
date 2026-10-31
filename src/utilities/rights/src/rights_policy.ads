package Rights_Policy is

   --  Who may get which category of authority (ADR 0021, section 4). A
   --  policy is a mode for a user and a category, in the broker's memory,
   --  so it persists with the process.

   type Mode is (None, Elevate, Granted);
   --  None: the user can't get the category. Elevate: under sudo only.
   --  Granted: in an ordinary launch. Mode'Pos is the word that
   --  Rights_Admin.Allow takes.

   Max_User_Length     : constant := 64;
   Max_Category_Length : constant := 32;
   Max_Policies        : constant := 128;

   function Covers (Policy, Category : String) return Boolean;
   --  True if a policy for the category Policy applies to the category
   --  Category: they are the same, Category is below Policy (hardware
   --  covers hardware.ports), or Policy is "*", which covers every
   --  category.

   procedure Allow
     (User     : String;
      Category : String;
      Value    : Mode;
      Success  : out Boolean);
   --  Set the mode for User and Category, replacing the earlier one for
   --  the same pair. Success is False, and nothing changes, if a name is
   --  too long or the table is full.

   function Lookup (User, Category : String) return Mode;
   --  The mode for User and Category: that of the most specific policy
   --  that covers Category, or None if there is no policy for User that
   --  does.

end Rights_Policy;
