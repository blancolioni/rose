--  The parts of init's boot sequence (Init.Boot, generated from
--  boot.yaml) that aren't wiring, and what Init.Boot uses to copy caps
--  from the servers it launches (#374).

with Rose.Capabilities;
with Rose.Objects;

package Init.Boot_Steps is

   function Copy
     (Copy_Cap   : Rose.Capabilities.Capability;
      Endpoint   : Rose.Objects.Endpoint_Id;
      Identifier : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability;
   --  A cap for Endpoint, copied from the process whose copy cap is
   --  Copy_Cap.  The process may not have made the endpoint yet, so this
   --  tries again a few times.  If it still fails, init reports it and
   --  exits.

   procedure Wait (Milliseconds : Natural);
   --  Wait for the timer, which must have been launched.

   procedure Set_Console;
   --  Make the boot console where Init.Calls.Send_String reports.

   procedure Load_Partitions;
   --  Launch partition servers for the swap and log partitions that scan
   --  found on hd0.  The install media's init also launches them for the
   --  BIOS boot partition, the boot partition, and block 0, which
   --  restore-install writes the boot loader to (#322).

end Init.Boot_Steps;
