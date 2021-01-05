with Rose.System_Calls.Client;
with System.Standard_Caps;

package body System.Init is

begin
   System.Standard_Caps.Standard_Input_Cap :=
     Rose.System_Calls.Client.Get_Capability
       (System.Standard_Caps.Take_Next_Cap);
   System.Standard_Caps.Standard_Output_Cap :=
     Rose.System_Calls.Client.Get_Capability
       (System.Standard_Caps.Take_Next_Cap);
end System.Init;
