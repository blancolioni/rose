with Rose.Capabilities;
with Rose.Objects;

package Rose.System_Calls.Server is

   procedure Create_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0;
      Entry_Cap    : out Rose.Capabilities.Capability;
      Endpoint_Cap : out Rose.Capabilities.Capability);

   function Create_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability;

   procedure Rescind_Cap
     (Cap : Rose.Capabilities.Capability);

   procedure Delete_Cap
     (Cap : Rose.Capabilities.Capability);

   procedure Create_Anonymous_Endpoint
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id);

   function Create_Receive_Cap
     (Create_Cap   : Rose.Capabilities.Capability)
      return Rose.Capabilities.Capability;

   function Create_Receive_Cap
     (Create_Cap   : Rose.Capabilities.Capability;
      Endpoint_Id  : Rose.Objects.Endpoint_Id;
      Identifier   : Rose.Objects.Capability_Identifier := 0)
      return Rose.Capabilities.Capability;

   procedure Send_Reply
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words);

   procedure Send_Reply_Caps
     (Cap  : Rose.Capabilities.Capability;
      Caps : Sent_Caps_Array := No_Sent_Caps);

end Rose.System_Calls.Server;
