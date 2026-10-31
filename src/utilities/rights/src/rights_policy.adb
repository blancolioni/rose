package body Rights_Policy is

   type Policy_Record is
      record
         User          : String (1 .. Max_User_Length);
         User_Last     : Natural;
         Category      : String (1 .. Max_Category_Length);
         Category_Last : Natural;
         Value         : Mode;
      end record;

   Policies : array (1 .. Max_Policies) of Policy_Record;
   Count    : Natural := 0;

   -----------
   -- Allow --
   -----------

   procedure Allow
     (User     : String;
      Category : String;
      Value    : Mode;
      Success  : out Boolean)
   is
   begin
      Success := False;

      if User'Length = 0 or else User'Length > Max_User_Length
        or else Category'Length > Max_Category_Length
      then
         return;
      end if;

      for Index in 1 .. Count loop
         declare
            Item : Policy_Record renames Policies (Index);
         begin
            if Item.User (1 .. Item.User_Last) = User
              and then Item.Category (1 .. Item.Category_Last) = Category
            then
               Item.Value := Value;
               Success := True;
               return;
            end if;
         end;
      end loop;

      if Count = Max_Policies then
         return;
      end if;

      Count := Count + 1;
      Policies (Count) :=
        (User          => (others => ' '),
         User_Last     => User'Length,
         Category      => (others => ' '),
         Category_Last => Category'Length,
         Value         => Value);
      Policies (Count).User (1 .. User'Length) := User;
      Policies (Count).Category (1 .. Category'Length) := Category;
      Success := True;
   end Allow;

   ------------
   -- Covers --
   ------------

   function Covers (Policy, Category : String) return Boolean is
   begin
      return Policy = "*"
        or else Policy = Category
        or else (Category'Length > Policy'Length
                 and then Category (Category'First
                                    .. Category'First + Policy'Length - 1)
                            = Policy
                 and then Category (Category'First + Policy'Length) = '.');
   end Covers;

   ------------
   -- Lookup --
   ------------

   function Lookup (User, Category : String) return Mode is
      Result : Mode := None;
      Best   : Integer := -1;
   begin
      for Index in 1 .. Count loop
         declare
            Item : Policy_Record renames Policies (Index);
         begin
            if Item.User (1 .. Item.User_Last) = User
              and then Covers (Item.Category (1 .. Item.Category_Last),
                               Category)
            then
               declare
                  Specificity : constant Natural :=
                                  (if Item.Category (1 .. Item.Category_Last)
                                     = "*"
                                   then 0 else Item.Category_Last);
               begin
                  if Specificity > Best then
                     Best := Specificity;
                     Result := Item.Value;
                  end if;
               end;
            end if;
         end;
      end loop;

      return Result;
   end Lookup;

end Rights_Policy;
