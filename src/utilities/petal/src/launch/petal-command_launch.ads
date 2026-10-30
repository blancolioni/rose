with Rose.Capabilities;

package Petal.Command_Launch is

   --  Running the commands that are installed in the command registry
   --  (ADR 0002). Petal reads a command's signature from its metadata,
   --  binds the words of the command line to its arguments, launches it,
   --  and calls its command endpoint with the arguments and the standard
   --  streams it declares.

   procedure Set_Registry
     (Commands : Rose.Capabilities.Capability;
      Terminal : Rose.Capabilities.Capability);
   --  Use Commands, the command registry's Commands interface, to find
   --  commands, and give Terminal, the console, to a command with the
   --  Console role (#454). Petal's command declaration gives both
   --  (rose-core-petal.idl).

   procedure Run (Line : String);
   --  Run the command line Line: a command name and its arguments, as
   --  words separated by spaces. A word in double quotes can contain
   --  spaces. --help and --version are answered from the metadata, and
   --  launch nothing.

end Petal.Command_Launch;
