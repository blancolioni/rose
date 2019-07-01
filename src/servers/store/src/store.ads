with Rose.Capabilities;

package Store is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Take_Next_Cap       : constant Rose.Capabilities.Capability := 2;

   Delete_Endpoint_Cap : Rose.Capabilities.Capability;
   Console_Cap         : Rose.Capabilities.Capability;

end Store;
