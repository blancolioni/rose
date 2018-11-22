package Rose.System_Calls.Marshalling is

   procedure Copy_String_To_Buffer
     (S    : String;
      Addr : System.Address;
      Size : System.Storage_Elements.Storage_Count);

   function Copy_Buffer_To_String
     (Addr : System.Address;
      Size : System.Storage_Elements.Storage_Count)
      return String;

   function Copy_Memory_To_String
     (Memory_Cap : Rose.Capabilities.Capability;
      Size       : System.Storage_Elements.Storage_Count)
      return String;

   procedure Copy_String_To_Memory
     (Memory_Cap : Rose.Capabilities.Capability;
      S          : String);

end Rose.System_Calls.Marshalling;
