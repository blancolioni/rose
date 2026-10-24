package Petal.Command_Launch is

   --  Running the commands that are installed in the command registry
   --  (ADR 0002). Petal reads a command's signature from its metadata,
   --  binds the words of the command line to its arguments, launches it,
   --  and calls its command endpoint with the arguments and the standard
   --  streams it declares.

   procedure Run (Line : String);
   --  Run the command line Line: a command name and its arguments, as
   --  words separated by spaces. A word in double quotes can contain
   --  spaces. --help and --version are answered from the metadata, and
   --  launch nothing.

end Petal.Command_Launch;
