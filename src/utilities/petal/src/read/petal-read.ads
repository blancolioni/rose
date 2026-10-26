package Petal.Read is

   procedure Reader_Loop;

   procedure Execute (Line : String);
   --  Run the command line Line: a built-in command, or a command from
   --  the registry.

   procedure Exit_Reader_Loop;

end Petal.Read;
