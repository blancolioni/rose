with Rose.Capabilities;

package Petal.Command_Launch is

   --  Running the commands that are installed in the command registry
   --  (ADR 0002). Petal reads a command's signature from its metadata,
   --  binds the words of the command line to its arguments, launches it,
   --  and calls its command endpoint with the arguments and the standard
   --  streams it declares.

   procedure Set_Registry
     (Commands : Rose.Capabilities.Capability;
      Terminal : Rose.Capabilities.Capability;
      Clock    : Rose.Capabilities.Capability;
      Rights   : Rose.Capabilities.Capability);
   --  Use Commands, the command registry's Commands interface, to find
   --  commands, give Terminal, the console, to a command with the
   --  Console role (#454), give Clock, the clock's Clock interface, to
   --  one with the Clock_Service role (ADR 0020), and ask Rights, the
   --  rights broker's Rights interface, for the authority of a command's
   --  categorised roles (ADR 0021). Petal's command declaration gives
   --  all four (rose-core-petal.idl).

   procedure Run (Line : String);
   --  Run the command line Line: a command name and its arguments, as
   --  words separated by spaces. A word in double quotes can contain
   --  spaces. --help and --version are answered from the metadata, and
   --  launch nothing. "sudo CATEGORIES COMMAND [ARGUMENT]..." runs
   --  COMMAND with the authority of CATEGORIES, comma-separated: a
   --  category that the command does not need gets a warning, and a
   --  categorised role that none of them covers is refused (ADR 0021,
   --  section 2).

end Petal.Command_Launch;
