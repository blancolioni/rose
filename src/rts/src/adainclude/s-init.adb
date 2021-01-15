with Rose.Capabilities;
with Rose.Invocation;
with Rose.System_Calls.Client;
with System.Standard_Caps;

package body System.Init is

   use Rose.Capabilities;

   Get_Cap : Rose.Capabilities.Capability :=
              Rose.Capabilities.Null_Capability;
   Params : aliased Rose.Invocation.Invocation_Record;

begin

   Rose.System_Calls.Initialize_Send
     (Params, System.Standard_Caps.Argument_Cap_Set);
   Rose.System_Calls.Receive_Caps (Params, 3);
   Rose.System_Calls.Invoke_Capability (Params);
   Get_Cap := Params.Caps (1);

   System.Standard_Caps.Standard_Input_Cap :=
     Rose.System_Calls.Client.Get_Capability (Get_Cap, (1 => 1));
   System.Standard_Caps.Standard_Output_Cap :=
     Rose.System_Calls.Client.Get_Capability (Get_Cap, (1 => 2));

end System.Init;
