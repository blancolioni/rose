package body Rose.Capabilities.Layout is

   -------------------------
   -- Endpoint_Capability --
   -------------------------

   function Endpoint_Capability
     (Oid        : Rose.Objects.Object_Id;
      Endpoint   : Rose.Objects.Endpoint_Index;
      Identifier : Rose.Objects.Capability_Identifier)
      return Capability_Layout
   is
   begin
      return Capability_Layout'
        (Header  => Capability_Header'
           (Cap_Type    => Endpoint_Cap,
            Endpoint    => Endpoint,
            Identifier  => Identifier,
            others      => <>),
         Payload => Oid);
   end Endpoint_Capability;

   ------------------------
   -- Receive_Capability --
   ------------------------

   function Receive_Capability
     (Oid      : Rose.Objects.Object_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Capability_Layout
   is
   begin
      return Capability_Layout'
        (Header  => Capability_Header'
           (Cap_Type    => Receive_Cap,
            Endpoint    => Endpoint,
            others      => <>),
         Payload => Oid);
   end Receive_Capability;

end Rose.Capabilities.Layout;
