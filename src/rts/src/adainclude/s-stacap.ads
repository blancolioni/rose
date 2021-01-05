with Rose.Capabilities;

private package System.Standard_Caps is

   Exit_Cap              : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap         : constant Rose.Capabilities.Capability := 2;
   Create_Endpoint_Cap   : constant Rose.Capabilities.Capability := 3;
   Memory_Cap            : constant Rose.Capabilities.Capability := 4;

   Standard_Streams_Cap  : Rose.Capabilities.Capability := 0;
   Standard_Input_Cap    : Rose.Capabilities.Capability := 0;
   Standard_Output_Cap   : Rose.Capabilities.Capability := 0;
   Standard_Error_Cap    : Rose.Capabilities.Capability := 0;

   Current_Directory_Cap : Rose.Capabilities.Capability := 0;
   Clock_Cap             : Rose.Capabilities.Capability := 0;
   Cap_Set_Constructor   : Rose.Capabilities.Capability := 0;

end System.Standard_Caps;
