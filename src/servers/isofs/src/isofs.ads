with Rose.Capabilities;

package IsoFS is

   Create_Endpoint_Cap : constant Rose.Capabilities.Capability := 1;
   Get_Cap_From_Set    : constant Rose.Capabilities.Capability := 2;

   Console_Cap           : Rose.Capabilities.Capability;
   Device_Cap            : Rose.Capabilities.Capability;
   Delete_Cap            : Rose.Capabilities.Capability;
   --  For deleting the caps of released readers. Without it, asking for
   --  the delete cap would try to load it from an argument cap set,
   --  which a boot module does not have.

end IsoFS;
