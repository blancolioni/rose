package Rose.System_Calls.Client is

   function Get_Value
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
      return Rose.Words.Word;

   function Get_Value
     (Cap  : Rose.Capabilities.Capability;
      Data : Rose.Words.Word)
      return Rose.Words.Word;

   function Get_Capability
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
      return Rose.Capabilities.Capability;

   procedure Send
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words);

   procedure Send
     (Cap  : Rose.Capabilities.Capability;
      Data : Rose.Words.Word);

   procedure Send_Caps
     (Cap  : Rose.Capabilities.Capability;
      Caps : Sent_Caps_Array := No_Sent_Caps);

   procedure Send_String
     (Cap     : Rose.Capabilities.Capability;
      Message : String);

end Rose.System_Calls.Client;
