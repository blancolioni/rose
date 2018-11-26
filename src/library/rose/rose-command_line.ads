package Rose.Command_Line is

   function Argument_Count return Natural;

   procedure Get_Argument
     (Index    : Positive;
      Argument : out String;
      Last     : out Natural);

end Rose.Command_Line;
