with Rose.Multiboot;
with Rose.Boot.Console;

package body Rose.Kernel.Command_Line
  with SPARK_Mode,
       Refined_State => (Command_Line_State => Command_Line)
is

   Max_Command_Line_Arguments : constant := 20;
   Max_Command_Line_Length    : constant := 1000;

   type Text_Range is
      record
         First, Last : Natural;
      end record;
   --  The characters First .. Last of the text, none if Last is First - 1.

   No_Text : constant Text_Range := (First => 1, Last => 0);

   type Argument_Pointers is
     array (1 .. Max_Command_Line_Arguments) of Text_Range;

   type Command_Line_Record is
      record
         Names  : Argument_Pointers := (others => No_Text);
         Values : Argument_Pointers := (others => No_Text);
         Text   : String (1 .. Max_Command_Line_Length) := (others => ' ');
         Length : Natural := 0;
         Count  : Natural := 0;
      end record;

   Command_Line : Command_Line_Record;

   function In_Text (Item : Text_Range) return Boolean
   is (Item.First >= 1
       and then Item.First - 1 <= Item.Last
       and then Item.Last <= Command_Line.Length);

   function Valid return Boolean
   is (Command_Line.Count <= Max_Command_Line_Arguments
       and then Command_Line.Length <= Max_Command_Line_Length
       and then (for all I in 1 .. Command_Line.Count =>
                   In_Text (Command_Line.Names (I))
                   and then In_Text (Command_Line.Values (I))));

   procedure Add_And_Report (Argument : String)
   with SPARK_Mode => Off;
   --  Add_Argument, and print it.

   procedure Add_Argument (Argument : String)
   with Global => (In_Out => Command_Line),
        Pre    => Valid,
        Post   => Valid;

   function Find_Argument (Argument : String) return Natural
   with Global => (Input => Command_Line),
        Pre    => Valid,
        Post   => Find_Argument'Result <= Command_Line.Count;

   --------------------
   -- Add_And_Report --
   --------------------

   procedure Add_And_Report (Argument : String)
   with SPARK_Mode => Off
   is
      Old_Count : constant Natural := Command_Line.Count;
   begin
      Add_Argument (Argument);

      if Command_Line.Count > Old_Count then
         declare
            Name  : Text_Range renames Command_Line.Names (Command_Line.Count);
            Value : Text_Range renames
              Command_Line.Values (Command_Line.Count);
         begin
            Rose.Boot.Console.Put
              (Command_Line.Text (Name.First .. Name.Last));

            if Value.First <= Value.Last then
               Rose.Boot.Console.Put ("=");
               Rose.Boot.Console.Put
                 (Command_Line.Text (Value.First .. Value.Last));
            end if;
         end;

         Rose.Boot.Console.New_Line;
      end if;
   end Add_And_Report;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (Argument : String) is
      type Scan_State is (Start_Scan, Name_Scan, Equal_Scan, Value_Scan);
      State      : Scan_State := Start_Scan;
      Old_Length : constant Natural := Command_Line.Length;
      Old_Count  : constant Natural := Command_Line.Count;
      Text_Index : Natural := Old_Length;
      Start      : Natural := 0;
      Name       : Text_Range := No_Text;
      Value      : Text_Range := No_Text;
   begin
      if Old_Count >= Max_Command_Line_Arguments
        or else Argument'Length >= Max_Command_Line_Length - Old_Length
      then
         return;
      end if;

      for I in Argument'Range loop
         Text_Index := Text_Index + 1;
         Command_Line.Text (Text_Index) := Argument (I);

         if Argument (I) = '=' and then State = Name_Scan then
            State := Equal_Scan;
            Name := (Start, Text_Index - 1);
            Start := Text_Index + 1;
         elsif State = Start_Scan then
            State := Name_Scan;
            Start := Text_Index;
         elsif State = Equal_Scan then
            State := Value_Scan;
            Start := Text_Index;
         end if;

         pragma Loop_Invariant (Valid);
         pragma Loop_Invariant (Command_Line.Length = Old_Length);
         pragma Loop_Invariant (Command_Line.Count = Old_Count);
         pragma Loop_Invariant (Value = No_Text);
         pragma Loop_Invariant
           (if State in Start_Scan | Name_Scan then Name = No_Text);
         pragma Loop_Invariant
           (Text_Index = Old_Length + (I - Argument'First + 1));
         pragma Loop_Invariant
           (if State in Name_Scan | Value_Scan
            then Start > Old_Length and then Start <= Text_Index);
         pragma Loop_Invariant
           (if State in Equal_Scan | Value_Scan
            then Name.First > Old_Length
                 and then Name.First - 1 <= Name.Last
                 and then Name.Last <= Text_Index);
      end loop;

      --  An argument that ends in the middle of its name or before its
      --  value has the empty range for what it lacks.

      case State is
         when Start_Scan | Equal_Scan =>
            null;
         when Name_Scan =>
            Name := (Start, Text_Index);
         when Value_Scan =>
            Value := (Start, Text_Index);
      end case;

      --  The new length first: it only widens what the old arguments
      --  point into, and the new one is added to the end.

      Command_Line.Length := Text_Index;
      Command_Line.Names (Old_Count + 1) := Name;
      Command_Line.Values (Old_Count + 1) := Value;
      Command_Line.Count := Old_Count + 1;
   end Add_Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural
   is (Command_Line.Count);

   -------------------
   -- Find_Argument --
   -------------------

   function Find_Argument (Argument : String) return Natural is
   begin
      for I in 1 .. Command_Line.Count loop
         pragma Loop_Invariant (Valid);

         declare
            Name   : constant Text_Range := Command_Line.Names (I);
            Length : constant Natural := Name.Last - Name.First + 1;
            Same   : Boolean := Argument'Length = Length;
         begin
            --  string equality appears not to work

            if Same then
               for J in 0 .. Length - 1 loop
                  if Command_Line.Text (Name.First + J)
                    /= Argument (Argument'First + J)
                  then
                     Same := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Same then
               return I;
            end if;
         end;
      end loop;

      return 0;
   end Find_Argument;

   ------------------
   -- Get_Argument --
   ------------------

   procedure Get_Argument (Index     : in  Positive;
                           Text      : out String;
                           Last      : out Natural)
   is
   begin
      Text := (others => ' ');

      if Index > Command_Line.Count then
         Last := Text'First - 1;
         return;
      end if;

      declare
         Name   : constant Text_Range := Command_Line.Names (Index);
         Length : constant Natural :=
                    Natural'Min (Name.Last - Name.First + 1, Text'Length);
      begin
         Last := Text'First - 1 + Length;
         Text (Text'First .. Last) :=
           Command_Line.Text (Name.First .. Name.First + Length - 1);
      end;
   end Get_Argument;

   -------------------
   -- Have_Argument --
   -------------------

   function Have_Argument
     (Name : String)
      return Boolean
   is
   begin
      return Find_Argument (Name) > 0;
   end Have_Argument;

   -----------------------------
   -- Initialise_Command_Line --
   -----------------------------

   procedure Initialise_Command_Line
   with SPARK_Mode => Off
   is
   begin
      Rose.Multiboot.Scan_Kernel_Arguments (Add_And_Report'Access);
   end Initialise_Command_Line;

   ----------------------
   -- Integer_Argument --
   ----------------------

   function Integer_Argument
     (Name : String)
      return Integer
   is
      Index  : constant Natural := Find_Argument (Name);
      Result : Natural := 0;
   begin
      if Index = 0 then
         return 0;
      end if;

      declare
         Value : constant Text_Range := Command_Line.Values (Index);
      begin
         if Value.Last < Value.First then
            return 0;
         end if;

         for Value_Index in Value.First .. Value.Last loop
            declare
               Ch : constant Character := Command_Line.Text (Value_Index);
            begin
               if Ch in '0' .. '9' then
                  declare
                     Digit : constant Natural :=
                               Character'Pos (Ch) - Character'Pos ('0');
                  begin
                     if Result > (Integer'Last - Digit) / 10 then
                        return 0;
                     end if;

                     Result := Result * 10 + Digit;
                  end;
               else
                  return 0;
               end if;
            end;
         end loop;

         return Result;
      end;
   end Integer_Argument;

end Rose.Kernel.Command_Line;
