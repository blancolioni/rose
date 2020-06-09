with Rose.Capabilities;

package Terminal is

   pragma Pure (Terminal);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Input_Device_Cap    : constant Rose.Capabilities.Capability := 2;
   Output_Device_Cap   : constant Rose.Capabilities.Capability := 3;

end Terminal;
