package Rose.Kernel.Command_Line is

   function Argument_Count return Natural;
   procedure Get_Argument (Index     : in  Positive;
                           Text      : out String;
                           Last      : out Natural);

   function Have_Argument
     (Name : String)
      return Boolean;

   function Integer_Argument
     (Name : String)
      return Integer;

   procedure Initialise_Command_Line;

end Rose.Kernel.Command_Line;
