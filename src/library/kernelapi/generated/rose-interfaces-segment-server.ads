with Rose.Server;
with Rose.Words;
with Rose.Capabilities;

package Rose.Interfaces.Segment.Server is

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

   procedure Create_Server
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Instanced                 : in     Boolean := False);

   procedure Attach_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler;
      Instanced                 : in     Boolean := False);

   procedure Publish_Interface
     (Server_Context            : in out Rose.Server.Server_Context;
      Add_Segment               : in     Add_Segment_Handler;
      Add_Nonpersistent_Segment : in     Add_Nonpersistent_Segment_Handler);

private

end Rose.Interfaces.Segment.Server;
