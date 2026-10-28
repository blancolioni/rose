with Rose.Capabilities;

package File is

   pragma Pure (File);

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 2;
   Delete_Cap_Cap      : constant Rose.Capabilities.Capability := 3;
   --  For deleting the caps of released readers.

end File;
