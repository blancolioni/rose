with Rose.Capabilities;
with Rose.Invocation;
with Rose.Words;

package Rose.System_Calls is

   type Sent_Words_Array is array (Positive range <>) of Rose.Words.Word;
   No_Sent_Words : Sent_Words_Array (1 .. 0);

   type Sent_Caps_Array is
     array (Positive range <>) of Rose.Capabilities.Capability;
   No_Sent_Caps : Sent_Caps_Array (1 .. 0);

   type Invocation_Flag_Array is
     array (Positive range <>) of Rose.Invocation.Invocation_Flag;

   procedure Invoke_Capability
     (Item : aliased in out Rose.Invocation.Invocation_Record);

   procedure Invoke
     (Cap       : Rose.Capabilities.Capability;
      Flags     : Invocation_Flag_Array;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array);

   procedure Invoke_Blocking_Send
     (Cap       : Rose.Capabilities.Capability;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array);

   procedure Invoke_Reply
     (Cap       : Rose.Capabilities.Capability;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array);

end Rose.System_Calls;
