with Rose.Capabilities;

private package System.Standard_Caps is

   Exit_Cap              : constant Rose.Capabilities.Capability := 1;
   Create_Endpoint_Cap   : constant Rose.Capabilities.Capability := 2;
   Memory_Cap            : constant Rose.Capabilities.Capability := 3;

   Standard_Streams_Cap  : constant Rose.Capabilities.Capability := 4;
   Standard_Input_Cap    : constant Rose.Capabilities.Capability := 5;
   Standard_Output_Cap   : constant Rose.Capabilities.Capability := 6;
   Standard_Error_Cap    : constant Rose.Capabilities.Capability := 7;

   Current_Directory_Cap : constant Rose.Capabilities.Capability := 8;
   Clock_Cap             : constant Rose.Capabilities.Capability := 9;
   Cap_Set_Constructor   : constant Rose.Capabilities.Capability := 10;

end System.Standard_Caps;
