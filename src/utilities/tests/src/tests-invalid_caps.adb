with Ada.Text_IO;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls;

package body Tests.Invalid_Caps is

   ----------------------
   -- Invalid_Cap_Test --
   ----------------------

   function Invalid_Cap_Test return Boolean is
      use type Rose.Invocation.Invocation_Error;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Ada.Text_IO.Put ("Invalid cap test");

      Rose.System_Calls.Initialize_Send
        (Params, Rose.Capabilities.Null_Capability);
      Rose.System_Calls.Invoke_Capability (Params);

      return Params.Control.Flags (Rose.Invocation.Error)
        and then Params.Error = Rose.Invocation.Invalid_Capability;
   end Invalid_Cap_Test;

end Tests.Invalid_Caps;
