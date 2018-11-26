with Rose.Environment;
with Rose.Strings;

package body Rose.Command_Line is

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
      Count : String (1 .. 10);
      Last  : Natural;
   begin
      Rose.Environment.Get_Environment_Value ("@arg-count", Count, Last);
      return Rose.Strings.To_Integer (Count (1 .. Last));
   end Argument_Count;

   ------------------
   -- Get_Argument --
   ------------------

   procedure Get_Argument
     (Index    : Positive;
      Argument : out String;
      Last     : out Natural)
   is
      Name : String := "arg00";
   begin
      Name (Name'Last) := Character'Val (Index mod 10 + 48);
      Name (Name'Last - 1) := Character'Val (Index / 10 mod 10 + 48);
      Rose.Environment.Get_Environment_Value (Name, Argument, Last);
   end Get_Argument;

end Rose.Command_Line;
