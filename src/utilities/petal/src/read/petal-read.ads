package Petal.Read is

   procedure Reader_Loop;
   --  Show the prompt, read a command line from standard input, and run
   --  it, until the input ends.

   procedure Execute (Line : String);
   --  Run the command line Line (see Petal.Command_Launch.Run).

end Petal.Read;
