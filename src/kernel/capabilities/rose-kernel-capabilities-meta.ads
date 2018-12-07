package Rose.Kernel.Capabilities.Meta is

   use Rose.Objects;

   Create_Endpoint_Endpoint        : constant Endpoint_Index := 1;
   Create_Entry_Cap_Endpoint       : constant Endpoint_Index := 2;
   Create_Page_Object_Cap_Endpoint : constant Endpoint_Index := 3;
   Create_Cap_Send_Cap_Endpoint    : constant Endpoint_Index := 4;
   Create_Cap_Singleton_Receive    : constant Endpoint_Index := 5;
   Create_Cap_Set_Endpoint         : constant Endpoint_Index := 6;
   Add_Cap_To_Set_Endpoint         : constant Endpoint_Index := 7;
   Remove_Cap_From_Set_Endpoint    : constant Endpoint_Index := 8;
   Receive_On_Caps                 : constant Endpoint_Index := 16;
   Receive_On_Any_Cap              : constant Endpoint_Index := 17;
   Enable_Kernel_Debug             : constant Endpoint_Index := 28;
   Rescind_Cap                     : constant Endpoint_Index := 29;
   Delete_Cap                      : constant Endpoint_Index := 30;
   Exit_Process                    : constant Endpoint_Index := 31;

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access);

end Rose.Kernel.Capabilities.Meta;
