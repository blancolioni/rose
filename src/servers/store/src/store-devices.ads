with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

package Store.Devices is

   procedure Add_Backing_Store
     (Id     : in Rose.Objects.Capability_Identifier;
      Device : in Rose.Capabilities.Capability);

   function Reserve_Storage
     (Size : Rose.Words.Word_64)
      return Rose.Capabilities.Capability;

   procedure Get_Range
     (Id    : Rose.Objects.Capability_Identifier;
      Base  : out Rose.Objects.Object_Id;
      Bound : out Rose.Objects.Object_Id);

end Store.Devices;
