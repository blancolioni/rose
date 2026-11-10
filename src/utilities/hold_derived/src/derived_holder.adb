with Ada.Text_IO;

with Rose.Invocation;
with Rose.Server;
with Rose.System_Calls;

with Rose.Interfaces.Restorable.Server;

package body Derived_Holder is

   use type Rose.Capabilities.Capability;

   type Holder_Object is
     limited new Rose.Interfaces.Restorable.Server.Restorable_Interface
   with null record;

   overriding function Restorable_Cap
     (Item : in out Holder_Object)
      return Rose.Capabilities.Capability
   is (Rose.Interfaces.Restorable.Server.Cap_Of (Item));

   overriding procedure Restored (Item : in out Holder_Object);

   Context    : Rose.Server.Server_Context;
   The_Holder : aliased Holder_Object;

   Parent_Cap : Rose.Capabilities.Capability :=
                  Rose.Capabilities.Null_Capability;
   Child_Cap  : Rose.Capabilities.Capability :=
                  Rose.Capabilities.Null_Capability;

   Checked : Boolean := False;
   --  Set once the capabilities have been checked after a restore.  The
   --  check rescinds Parent, so it can only be made once; the variable
   --  is saved with the rest of the process.

   function Works (Cap : Rose.Capabilities.Capability) return Boolean;
   --  A call through Cap, which names the registry's Commands interface,
   --  is answered with its endpoints.

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server
     (Parent : Rose.Capabilities.Capability;
      Child  : Rose.Capabilities.Capability)
   is
   begin
      Parent_Cap := Parent;
      Child_Cap := Child;
      Rose.Interfaces.Restorable.Server.Attach (Context, The_Holder'Access);

      if Works (Parent_Cap) and then Works (Child_Cap) then
         Ada.Text_IO.Put_Line
           ("hold_derived: holding two derived capabilities");
      else
         Ada.Text_IO.Put_Line
           ("hold_derived: FAIL: the capabilities don't work at launch");
      end if;
   end Create_Server;

   --------------
   -- Restored --
   --------------

   overriding procedure Restored (Item : in out Holder_Object) is
      pragma Unreferenced (Item);
      Child_Worked   : Boolean;
      Child_Revoked  : Boolean;
      Parent_Remains : Boolean;
   begin
      if Checked then
         return;
      end if;
      Checked := True;

      Child_Worked := Works (Child_Cap);
      Rose.System_Calls.Rescind_Cap (Parent_Cap);
      Child_Revoked := not Works (Child_Cap);
      Parent_Remains := Works (Parent_Cap);

      if Child_Worked and then Child_Revoked and then Parent_Remains then
         Ada.Text_IO.Put_Line
           ("hold_derived: restored: the derived capability works, "
            & "and rescinding its parent revokes it");
      else
         Ada.Text_IO.Put_Line
           ("hold_derived: FAIL: restored: child works "
            & Boolean'Image (Child_Worked)
            & ", revoked " & Boolean'Image (Child_Revoked)
            & ", parent works " & Boolean'Image (Parent_Remains));
      end if;
   end Restored;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
   begin
      Rose.Server.Start_Server (Context);
   end Start_Server;

   -----------
   -- Works --
   -----------

   function Works (Cap : Rose.Capabilities.Capability) return Boolean is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Cap = Rose.Capabilities.Null_Capability then
         return False;
      end if;

      Rose.System_Calls.Initialize_Send (Params, Cap);
      Rose.System_Calls.Receive_Caps (Params, 4);
      Rose.System_Calls.Invoke_Capability (Params);

      if Params.Control.Flags (Rose.Invocation.Error) then
         return False;
      end if;

      --  The interface's endpoints, which aren't needed.

      if Params.Control.Flags (Rose.Invocation.Send_Caps) then
         for I in 0 .. Params.Control.Last_Recv_Cap loop
            Rose.System_Calls.Delete_Cap (Params.Caps (I));
         end loop;
      end if;

      return True;
   end Works;

end Derived_Holder;
