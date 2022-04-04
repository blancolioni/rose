with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;

package Init.Calls is

   type Array_Of_Words is
     array (Positive range <>) of Rose.Words.Word;

   type Array_Of_Capabilities is
     array (Positive range <>) of Rose.Capabilities.Capability;

   function Call
     (Cap    : Rose.Capabilities.Capability;
      Data   : Array_Of_Words)
      return Rose.Capabilities.Capability;

   function Call
     (Cap      : Rose.Capabilities.Capability;
      Sent_Cap : Rose.Capabilities.Capability;
      Data     : Array_Of_Words)
      return Rose.Capabilities.Capability;

   function Call
     (Cap    : Rose.Capabilities.Capability;
      Data   : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is (Call (Cap, (1 => Data)));

   procedure Call
     (Cap         : Rose.Capabilities.Capability;
      Data        : Array_Of_Words;
      Result_Caps : out Array_Of_Capabilities);

   procedure Send
     (Cap    : Rose.Capabilities.Capability;
      Data   : Array_Of_Words);

   procedure Send
     (Cap    : Rose.Capabilities.Capability);

   procedure Receive
     (Cap : Rose.Capabilities.Capability);

   procedure Send_Cap
     (Cap      : Rose.Capabilities.Capability;
      Sent_Cap : Rose.Capabilities.Capability;
      Data     : Array_Of_Words);

   procedure Send_String
     (Cap     : Rose.Capabilities.Capability;
      Message : String);

   procedure Send_String
     (Message : String);

   function Find_In_Map
     (Find_Cap : Rose.Capabilities.Capability;
      Key      : String)
      return Rose.Capabilities.Capability;

   function Create_Cap_Set_With
     (Create_Cap_Set  : Rose.Capabilities.Capability;
      Caps            : Array_Of_Capabilities)
      return Rose.Capabilities.Capability;

   function Launch_Boot_Module
     (Cap            : Rose.Capabilities.Capability;
      Module_Index   : Rose.Words.Word;
      Priority       : Rose.Words.Word;
      Create_Cap     : Rose.Capabilities.Capability;
      Create_Cap_Set : Rose.Capabilities.Capability;
      Launch_Caps    : Array_Of_Capabilities)
      return Rose.Objects.Object_Id;

   function Launch_Boot_Module
     (Cap            : Rose.Capabilities.Capability;
      Module_Index   : Rose.Words.Word;
      Priority       : Rose.Words.Word;
      Create_Cap     : Rose.Capabilities.Capability;
      Create_Cap_Set : Rose.Capabilities.Capability;
      Launch_Caps    : Array_Of_Capabilities;
      Launch_Words   : Array_Of_Words)
      return Rose.Objects.Object_Id;

   function Launch
     (Launch_Cap  : Rose.Capabilities.Capability;
      Cap_Set     : Rose.Capabilities.Capability)
      return Rose.Objects.Object_Id;

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities);

end Init.Calls;
