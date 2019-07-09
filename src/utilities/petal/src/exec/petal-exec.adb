package body Petal.Exec is

   Max_Installed_Commands : constant := 100;
   Max_Name_Length        : constant := 20;
   subtype Command_Name is String (1 .. Max_Name_Length);

   type Installed_Command is
      record
         Name    : Command_Name;
         Command : Petal.Commands.Petal_Command;
      end record;

   type Installed_Command_Array is
     array (1 .. Max_Installed_Commands) of Installed_Command;

   Installed_List         : Installed_Command_Array;
   Installed_Count        : Natural := 0;

   ----------
   -- Find --
   ----------

   function Find (Name : String) return Petal.Commands.Petal_Command is
      Key : Command_Name := (others => ' ');
   begin
      Key (1 .. Name'Length) := Name;
      for I in 1 .. Installed_Count loop
         if Installed_List (I).Name = Key then
            return Installed_List (I).Command;
         end if;
      end loop;

      return Installed_List (1).Command;
   end Find;

   -------------
   -- Install --
   -------------

   procedure Install (Name : String; Command : Petal.Commands.Petal_Command) is
      Key : Command_Name := (others => ' ');
   begin
      Key (1 .. Name'Length) := Name;
      Installed_Count := Installed_Count + 1;
      Installed_List (Installed_Count) := (Key, Command);
   end Install;

end Petal.Exec;
