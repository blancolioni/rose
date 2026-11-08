with Rose.Capabilities;

package Exec.Output is

   --  Exec's messages, written to the standard output that it gives the
   --  programs it launches: the boot console, until init gives it the
   --  persistent console (Set_Standard_Output).

   procedure Set (Writer : Rose.Capabilities.Capability);
   --  Write to Writer, a Writer interface cap, from now on.

   procedure Put_Line (Text : String);
   --  Write Text and a line break. A writer that fails loses the line.

end Exec.Output;
