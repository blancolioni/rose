with Rose.Init_Capabilities;

with Init.Calls;

package body Init.Grants is

   use Rose.Words;

   Max_Grants : constant := 32;

   type Grant_Kind is (Port_Grant, Interrupt_Grant, Page_Grant);

   type Grant_Record is
      record
         Kind        : Grant_Kind;
         First, Last : Word;
         Owner       : Owner_Id;
      end record;

   Grants      : array (1 .. Max_Grants) of Grant_Record;
   Grant_Count : Natural := 0;

   Last_Owner : Owner_Id := 1000;

   function Claim
     (Kind        : Grant_Kind;
      Owner       : Owner_Id;
      First, Last : Word)
      return Boolean;
   --  Record that Owner has First .. Last of Kind, and return True, unless
   --  another owner has any of them, or there is no room.

   -----------
   -- Claim --
   -----------

   function Claim
     (Kind        : Grant_Kind;
      Owner       : Owner_Id;
      First, Last : Word)
      return Boolean
   is
   begin
      if Owner = No_Owner then
         return True;
      end if;

      for Grant of Grants (1 .. Grant_Count) loop
         if Grant.Kind = Kind
           and then Grant.Owner /= Owner
           and then First <= Grant.Last
           and then Last >= Grant.First
         then
            return False;
         end if;
      end loop;

      if Grant_Count = Max_Grants then
         return False;
      end if;

      Grant_Count := Grant_Count + 1;
      Grants (Grant_Count) := (Kind, First, Last, Owner);
      return True;
   end Claim;

   -----------------
   -- Device_Page --
   -----------------

   function Device_Page
     (Owner : Owner_Id;
      Page  : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if not Claim (Page_Grant, Owner, Page, Page) then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Device_Pages_Cap, (1 => Page));
   end Device_Page;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Owner  : Owner_Id;
      Vector : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if not Claim (Interrupt_Grant, Owner, Vector, Vector) then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Interrupts_Cap, (1 => Vector));
   end Interrupt;

   ----------------
   -- Next_Owner --
   ----------------

   function Next_Owner return Owner_Id is
   begin
      Last_Owner := Last_Owner + 1;
      return Last_Owner;
   end Next_Owner;

   -----------
   -- Ports --
   -----------

   function Ports
     (Owner    : Owner_Id;
      Endpoint : Rose.Words.Word;
      Size     : Rose.Words.Word;
      First    : Rose.Words.Word;
      Last     : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is
   begin
      if First > Last
        or else not Claim (Port_Grant, Owner, First, Last)
      then
         return Rose.Capabilities.Null_Capability;
      end if;

      return Init.Calls.Call
        (Rose.Init_Capabilities.Ports_Cap, (Endpoint, Size, First, Last));
   end Ports;

end Init.Grants;
