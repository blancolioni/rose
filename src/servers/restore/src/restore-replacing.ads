with Rose.Capabilities;

package Restore.Replacing is

   --  The update boot's last step (ADR 0031 section 5, ADR 0032, #830):
   --  replacing each restored server that implements Replaceable with the
   --  version of its program on the install media, which Installer.Update
   --  has just installed again.

   procedure Add_Program
     (Name   : String;
      Launch : Rose.Capabilities.Capability);
   --  Name was installed again, and Launch is its new Launch interface.

   procedure Replace_Servers;
   --  Replace each restored process that answers Replaceable, and whose
   --  program was installed again, one at a time, in the order they were
   --  restored. Each is launched as a command with Previous alone, the
   --  old version's Replaceable: it takes its other caps from the old
   --  version (#901). A server that doesn't take over leaves the old
   --  version running, and the next is replaced. Write a line for each,
   --  and a summary.

end Restore.Replacing;
