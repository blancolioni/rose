package body Rose.Command_Arguments is

   Name_Text   : String (1 .. Max_Name_Length);
   Name_Length : Natural := 0;

   Text  : String (1 .. Max_Text_Length);
   Count : Natural := 0;
   Last  : array (0 .. Max_Arguments) of Natural := (others => 0);
   --  Argument N is Text (Last (N - 1) + 1 .. Last (N)).

   Status : Integer := 0;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument (Item : String) is
      First : constant Natural := Last (Count) + 1;
   begin
      if Count = Max_Arguments
        or else Item'Length > Max_Text_Length - Last (Count)
      then
         raise Storage_Error;
      end if;

      Text (First .. First + Item'Length - 1) := Item;
      Count := Count + 1;
      Last (Count) := First + Item'Length - 1;
   end Add_Argument;

   --------------
   -- Argument --
   --------------

   function Argument (Index : Positive) return String is
   begin
      if Index > Count then
         return "";
      end if;

      return Text (Last (Index - 1) + 1 .. Last (Index));
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is (Count);

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String is (Name_Text (1 .. Name_Length));

   -----------------
   -- Exit_Status --
   -----------------

   function Exit_Status return Integer is (Status);

   ----------------------
   -- Set_Command_Name --
   ----------------------

   procedure Set_Command_Name (Name : String) is
   begin
      Name_Length := Natural'Min (Name'Length, Max_Name_Length);
      Name_Text (1 .. Name_Length) :=
        Name (Name'First .. Name'First + Name_Length - 1);
   end Set_Command_Name;

   ---------------------
   -- Set_Exit_Status --
   ---------------------

   procedure Set_Exit_Status (Code : Integer) is
   begin
      Status := Code;
   end Set_Exit_Status;

end Rose.Command_Arguments;
