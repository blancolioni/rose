generic
   with procedure Put (Text : String);
   with procedure Put_Line (Text : String);
package Rose.Command_Signatures.Help is

   --  The text that a command's --help and --version write, from its
   --  signature, through Put and Put_Line: petal's, and that of a command
   --  built for the host (#1101).

   procedure Put_Help (Item : Signature);
   --  The usage line, the summary, each option with its help, and the
   --  description and help footer, if the command has them.

   procedure Put_Version (Item : Signature);
   --  The name, title and version, then the copyright, license and
   --  authors, if the command has them.

end Rose.Command_Signatures.Help;
