with Rose.Objects;

package Rose.Capabilities.Layout is

   Capability_Header_Bytes : constant := 8;
   Capability_Layout_Bytes : constant := 16;

   type Capability_Type is
     (Null_Cap,
      Page_Object_Cap,
      Meta_Cap,
      Process_Cap,
      Endpoint_Cap,
      Receive_Cap,
      Interrupt_Cap,
      Kernel_Cap,
      Boot_Cap,
      Copy_Cap,
      Physical_Memory_Cap,
      Create_Cap,
      Page_Table_Cap,
      Reply_Cap,
      Arch_Cap,
      Other_Cap)
     with Size => 4;

   type Capability_Header is
      record
         Cap_Type         : Capability_Type                    := Null_Cap;
         Single_Use       : Boolean                            := False;
         Read_Access      : Boolean                            := False;
         Write_Access     : Boolean                            := False;
         Execution_Access : Boolean                            := False;
         Rescinded_Count  : Rose.Objects.Rescinded_Count       := 0;
         Endpoint         : Rose.Objects.Endpoint_Index        := 0;
         Identifier       : Rose.Objects.Capability_Identifier := 0;
      end record
     with Pack, Size => 64;

   type Capability_Layout is
      record
         Header   : Capability_Header;
         Payload  : Rose.Objects.Object_Id;
      end record
        with Pack, Size => 128;

   Empty_Capability : constant Capability_Layout := (others => <>);

   function Reply_Capability
     (Object_Id : Rose.Objects.Object_Id)
      return Capability_Layout
   is ((Reply_Cap, Single_Use => True, others => <>),
       Object_Id);

   function Receive_Capability
     (Oid      : Rose.Objects.Object_Id;
      Endpoint : Rose.Objects.Endpoint_Index)
      return Capability_Layout;

   function Endpoint_Capability
     (Oid        : Rose.Objects.Object_Id;
      Endpoint   : Rose.Objects.Endpoint_Index;
      Identifier : Rose.Objects.Capability_Identifier)
      return Capability_Layout;

end Rose.Capabilities.Layout;
