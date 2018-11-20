package body Rose.Capabilities.Layout is

   -------------------------
   -- Endpoint_Capability --
   -------------------------

   function Endpoint_Capability
     (Pid        : Rose.Objects.Process_Id;
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
         Payload => Rose.Objects.To_Object_Id (Pid));
   end Endpoint_Capability;

   ------------------------
   -- Receive_Capability --
   ------------------------

   function Receive_Capability
     (Pid      : Rose.Objects.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Capability_Layout
   is
   begin
      return Capability_Layout'
        (Header  => Capability_Header'
           (Cap_Type    => Receive_Cap,
            Endpoint    => Endpoint,
            others      => <>),
         Payload => Rose.Objects.To_Object_Id (Pid));
   end Receive_Capability;

end Rose.Capabilities.Layout;
