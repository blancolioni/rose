with Rose.Server;
with Rose.Objects;
with System.Storage_Elements;
with Rose.Capabilities;
with Rose.Words;

package Rose.Interfaces.Region.Server is

   type Get_Range_Handler is access
     procedure
       (Id         : in     Rose.Objects.Capability_Identifier;
        Base_Page  :    out Rose.Objects.Object_Id;
        Bound_Page :    out Rose.Objects.Object_Id);

   type Get_Handler is access
     procedure
       (Id   : in     Rose.Objects.Capability_Identifier;
        Page : in     Rose.Objects.Object_Id;
        Data :    out System.Storage_Elements.Storage_Array);

   type Put_Handler is access
     procedure
       (Id   : Rose.Objects.Capability_Identifier;
        Page : Rose.Objects.Object_Id;
        Data : System.Storage_Elements.Storage_Array);

   type Read_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   type Create_Subregion_Handler is access
     function
       (Id              : Rose.Objects.Capability_Identifier;
        Subregion_Base  : Rose.Objects.Object_Id;
        Subregion_Bound : Rose.Objects.Object_Id;
        Flags           : Rose.Words.Word)
     return Rose.Capabilities.Capability;

   procedure Create_Server
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler;
      Instanced        : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler;
      Instanced        : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context   : in out Rose.Server.Server_Context;
      Get_Range        : in     Get_Range_Handler;
      Get              : in     Get_Handler;
      Put              : in     Put_Handler;
      Read             : in     Read_Handler;
      Create_Subregion : in     Create_Subregion_Handler);

private

end Rose.Interfaces.Region.Server;
