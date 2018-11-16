with Rose.Words;

package Rose.Capabilities is

   pragma Pure (Rose.Capabilities);

   type Capability is new Rose.Words.Word_32;

   Null_Capability : constant Capability := 0;

   function Is_Null (Cap : Capability) return Boolean
   is (Cap = Null_Capability);

   type Capability_Array is array (Positive range <>) of Capability;

end Rose.Capabilities;
