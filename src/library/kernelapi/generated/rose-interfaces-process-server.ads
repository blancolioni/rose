with Rose.Server;
with Rose.Words;
with Rose.Capabilities;
with Rose.Objects;

package Rose.Interfaces.Process.Server is

   type Add_Segment_Handler is access
     procedure
       (Id            : Rose.Objects.Capability_Identifier;
        Virtual_Base  : Rose.Words.Word;
        Virtual_Bound : Rose.Words.Word;
        Region        : Rose.Capabilities.Capability;
        Region_Offset : Rose.Words.Word;
        Flags         : Rose.Words.Word);

   type Add_Nonpersistent_Segment_Handler is access
     procedure
       (Id            : Rose.Objects.Capability_Identifier;
        Virtual_Base  : Rose.Words.Word;
        Virtual_Bound : Rose.Words.Word;
        Flags         : Rose.Words.Word);

   type Published_Interface_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   type Destroy_Handler is access
     procedure (Id : Rose.Objects.Capability_Identifier);

   type Get_Object_Id_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Objects.Object_Id;

   type Heap_Interface_Handler is access
     function (Id : Rose.Objects.Capability_Identifier)
        return Rose.Capabilities.Capability;

   type Exit_Process_Handler is access
     procedure
       (Id          : Rose.Objects.Capability_Identifier;
        Exit_Status : Natural);

   type Publish_Interface_Handler is access
     procedure
       (Id            : Rose.Objects.Capability_Identifier;
        Interface_Cap : Rose.Capabilities.Capability);

   procedure Create_Server
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler;
      Instanced                 : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler;
      Instanced                 : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Published_Interface       : in     Published_Interface_Handler;
      Destroy                   : in     Destroy_Handler;
      Get_Object_Id             : in     Get_Object_Id_Handler;
      Heap_Interface            : in     Heap_Interface_Handler;
      Exit_Process              : in     Exit_Process_Handler;
      Publish_Interface         : in     Publish_Interface_Handler);

private

end Rose.Interfaces.Process.Server;
