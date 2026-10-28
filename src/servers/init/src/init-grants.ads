--  Caps for hardware, derived from init's root caps
--  (Rose.Init_Capabilities), and init's record of which program each
--  port, interrupt and device page went to.  Hardware that has gone to
--  one program is refused to any other (#115).

with Rose.Capabilities;
with Rose.Words;

package Init.Grants is

   type Owner_Id is new Natural;
   --  Who a grant is for: a boot server's object id, or an installed
   --  program's number from Next_Owner.

   No_Owner : constant Owner_Id := 0;
   --  Grants to No_Owner are not recorded, and can overlap any other.

   function Next_Owner return Owner_Id;
   --  A new owner for an installed program.

   function Ports
     (Owner    : Owner_Id;
      Endpoint : Rose.Words.Word;
      Size     : Rose.Words.Word;
      First    : Rose.Words.Word;
      Last     : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A port cap for First .. Last, or the null cap if another owner has
   --  any of those ports, or the kernel refuses.  Endpoint and Size are
   --  from Rose.Init_Capabilities.

   function Interrupt
     (Owner  : Owner_Id;
      Vector : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A cap that installs a handler for Vector, or the null cap if
   --  another owner has it, or the kernel refuses.

   function Device_Page
     (Owner : Owner_Id;
      Page  : Rose.Words.Word)
      return Rose.Capabilities.Capability;
   --  A cap that maps physical page Page at the same address, or the
   --  null cap if another owner has it, or the kernel refuses.

end Init.Grants;
