with Ada.Text_IO;

with System;
with System.Storage_Elements;

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

   --------------------
   -- No_Buffer_Test --
   --------------------

   function No_Buffer_Test return Boolean is
      use type Rose.Invocation.Invocation_Error;
      use System.Storage_Elements;

      Address : constant System.Address := Rose.System_Calls.Buffer_Address;
      Length  : constant Storage_Count := Rose.System_Calls.Buffer_Length;

      Text_Params    : aliased Rose.Invocation.Invocation_Record;
      Array_Params   : aliased Rose.Invocation.Invocation_Record;
      Receive_Params : aliased Rose.Invocation.Invocation_Record;

      function No_Buffer
        (Params : Rose.Invocation.Invocation_Record)
         return Boolean
      is (Params.Control.Flags (Rose.Invocation.Error)
          and then Params.Error = Rose.Invocation.No_Invoke_Buffer);

   begin
      Ada.Text_IO.Put ("No invoke buffer test");

      --  Nothing here may write to the console until the buffer is back.

      Rose.System_Calls.Use_Buffer (System.Null_Address, 0);

      Rose.System_Calls.Initialize_Send
        (Text_Params, Rose.Capabilities.Null_Capability);
      Rose.System_Calls.Send_Text (Text_Params, "text");
      Rose.System_Calls.Invoke_Capability (Text_Params);

      Rose.System_Calls.Initialize_Send
        (Array_Params, Rose.Capabilities.Null_Capability);
      Rose.System_Calls.Send_Storage_Array
        (Array_Params, (1, 2, 3), Writable => False);
      Rose.System_Calls.Invoke_Capability (Array_Params);

      Rose.System_Calls.Initialize_Receive
        (Receive_Params, Rose.Capabilities.Null_Capability);
      Rose.System_Calls.Receive_Buffer (Receive_Params);
      Rose.System_Calls.Invoke_Capability (Receive_Params);

      Rose.System_Calls.Use_Buffer (Address, Length);

      return No_Buffer (Text_Params)
        and then No_Buffer (Array_Params)
        and then No_Buffer (Receive_Params);
   end No_Buffer_Test;

end Tests.Invalid_Caps;
