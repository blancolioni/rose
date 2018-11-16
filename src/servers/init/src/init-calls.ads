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
     (Cap    : Rose.Capabilities.Capability;
      Data   : Rose.Words.Word)
      return Rose.Capabilities.Capability
   is (Call (Cap, (1 => Data)));

   procedure Send
     (Cap    : Rose.Capabilities.Capability;
      Data   : Array_Of_Words);

   procedure Send
     (Cap    : Rose.Capabilities.Capability);

   procedure Send_String
     (Cap     : Rose.Capabilities.Capability;
      Message : String);

   function Call
     (Cap       : Rose.Capabilities.Capability;
      Data      : Rose.Words.Word;
      Sent_Caps : Array_Of_Capabilities)
      return Rose.Objects.Object_Id;

end Init.Calls;
