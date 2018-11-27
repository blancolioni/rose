with Rose.Multiboot;
with Rose.Boot.Console;

package body Rose.Kernel.Command_Line is

   Max_Command_Line_Arguments : constant := 20;
   Max_Command_Line_Length    : constant := 1000;

   type Text_Range is
      record
         First, Last : Natural;
      end record;

   type Argument_Pointers is
     array (1 .. Max_Command_Line_Arguments) of Text_Range;

   type Command_Line_Record is
      record
         Names  : Argument_Pointers;
         Values : Argument_Pointers;
         Text   : String (1 .. Max_Command_Line_Length);
         Length : Natural := 0;
         Count  : Natural := 0;
      end record;

   Command_Line : Command_Line_Record;

   procedure Add_Argument (Argument : String);

   function Find_Argument (Argument : String) return Natural;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (Argument : String) is
      type Scan_State is (Start_Scan, Name_Scan, Equal_Scan, Value_Scan);
      State       : Scan_State := Start_Scan;
      Text_Index  : Natural := Command_Line.Length;
      Start       : Natural := 0;
   begin
      if Command_Line.Count >= Max_Command_Line_Arguments
        or else Command_Line.Length
          + Argument'Length >= Max_Command_Line_Length
      then
         return;
      end if;

      Command_Line.Count := Command_Line.Count + 1;

      for Ch of Argument loop
         Text_Index := Text_Index + 1;
         Command_Line.Text (Text_Index) := Ch;
         if Ch = '=' and then State = Name_Scan then
            State := Equal_Scan;
            Command_Line.Names (Command_Line.Count) :=
              (Start, Text_Index - 1);
            Start := Text_Index + 1;
         else
            if State = Start_Scan then
               State := Name_Scan;
               Start := Text_Index;
            elsif State = Equal_Scan then
               State := Value_Scan;
               Start := Text_Index;
            end if;
         end if;
      end loop;

      if State = Name_Scan then
         Command_Line.Names (Command_Line.Count) :=
           (Start, Text_Index);
         Command_Line.Values (Command_Line.Count) := (0, 0);
         Rose.Boot.Console.Put
           (Command_Line.Text (Command_Line.Names (Command_Line.Count).First
            .. Command_Line.Names (Command_Line.Count).Last));

      elsif State = Value_Scan then
         Command_Line.Values (Command_Line.Count) :=
           (Start, Text_Index);
         Rose.Boot.Console.Put
           (Command_Line.Text (Command_Line.Names (Command_Line.Count).First
            .. Command_Line.Names (Command_Line.Count).Last));

         Rose.Boot.Console.Put ("=");
         Rose.Boot.Console.Put
           (Command_Line.Text (Command_Line.Values (Command_Line.Count).First
            .. Command_Line.Values (Command_Line.Count).Last));
      end if;
      Rose.Boot.Console.New_Line;
      Command_Line.Length := Text_Index;

   end Add_Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      return Command_Line.Count;
   end Argument_Count;

   -------------------
   -- Find_Argument --
   -------------------

   function Find_Argument (Argument : String) return Natural is
      Check : String (1 .. 50);
      Last  : Natural;
      Index : Positive;
   begin
      for I in 1 .. Command_Line.Count loop
         Get_Argument (I, Check, Last);
         if Argument'Length = Last then
            --  string equality appears not to work
            Index := Argument'First;
            for J in 1 .. Last loop
               exit when Check (J) /= Argument (Index);
               Index := Index + 1;
            end loop;
            if Index > Argument'Last then
               return I;
            end if;
         end if;
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
      Start  : Positive;
      Finish : Natural;
   begin
      if Index > Command_Line.Count then
         Last := 0;
         return;
      end if;

      Start := Command_Line.Names (Index).First;
      Finish := Command_Line.Names (Index).Last;

      if Finish - Start + 1 > Text'Length then
         Finish := Start + Text'Length - 1;
      end if;
      Last := Text'First + Finish - Start;
      Text (Text'First .. Last) := Command_Line.Text (Start .. Finish);
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

   procedure Initialise_Command_Line is
   begin
      Rose.Multiboot.Scan_Kernel_Arguments (Add_Argument'Access);
   end Initialise_Command_Line;

   ----------------------
   -- Integer_Argument --
   ----------------------

   function Integer_Argument
     (Name : String)
      return Integer
   is
      Index : constant Natural := Find_Argument (Name);
   begin
      if Index = 0
        or else Command_Line.Values (Index).First = 0
        or else Command_Line.Values (Index).Last = 0
      then
         return 0;
      end if;

      declare
         Rec    : Text_Range renames Command_Line.Values (Index);
         Result : Integer := 0;
      begin
         for Value_Index in Rec.First .. Rec.Last loop
            declare
               Ch : constant Character := Command_Line.Text (Value_Index);
            begin
               if Ch in '0' .. '9' then
                  Result := Result * 10 + Character'Pos (Ch) - 48;
               else
                  return 0;
               end if;
            end;
         end loop;
         return Result;
      end;
   end Integer_Argument;

end Rose.Kernel.Command_Line;
