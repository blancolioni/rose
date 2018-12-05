--  Root package for Store server

with Rose.Capabilities;

package Store is

   pragma Pure (Store);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Delete_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Console_Cap         : constant Rose.Capabilities.Capability := 3;

end Store;
