package body Rose.Capabilities.Layout is

   -------------------------
   -- Endpoint_Capability --
   -------------------------

   function Endpoint_Capability
     (Pid      : Rose.Objects.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Generic_Capability_Layout
   is
   begin
      return Generic_Capability_Layout'
        (Header  => Generic_Capability_Header'
           (Cap_Type    => Endpoint_Cap,
            Endpoint    => Endpoint,
            others      => <>),
         Payload => Rose.Objects.To_Object_Id (Pid));
   end Endpoint_Capability;

   ------------------------
   -- Receive_Capability --
   ------------------------

   function Receive_Capability
     (Pid      : Rose.Objects.Process_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Generic_Capability_Layout
   is
   begin
      return Generic_Capability_Layout'
        (Header  => Generic_Capability_Header'
           (Cap_Type    => Receive_Cap,
            Endpoint    => Endpoint,
            others      => <>),
         Payload => Rose.Objects.To_Object_Id (Pid));
   end Receive_Capability;

end Rose.Capabilities.Layout;
