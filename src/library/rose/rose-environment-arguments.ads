package Rose.Environment.Arguments is

   --  Arguments from the environment page, for servers that mem starts
   --  with one. Commands get theirs from their launch invocation instead
   --  (see Rose.Command_Line.Launch).

   function Argument_Count return Natural;

   procedure Get_Argument
     (Index    : Positive;
      Argument : out String;
      Last     : out Natural);

end Rose.Environment.Arguments;
