with System;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;
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

   procedure Send_Text
     (Params : in out Rose.Invocation.Invocation_Record;
      Text   : String);

   procedure Send_Storage_Array
     (Params   : in out Rose.Invocation.Invocation_Record;
      Storage  : System.Storage_Elements.Storage_Array;
      Writable : Boolean);

    procedure Send_Cap
      (Params : in out Rose.Invocation.Invocation_Record;
       Cap    : Rose.Capabilities.Capability);

   procedure Send_Buffer
      (Params   : in out Rose.Invocation.Invocation_Record;
       Bytes    : System.Storage_Elements.Storage_Count;
       Buffer   : System.Address;
       Writable : Boolean);

--     procedure Receive_Buffer
--       (Params   : in out Rose.Invocation.Invocation_Record;
--        Buffer   : System.Address;
--        Writable : Boolean);

   procedure Receive_Buffer
     (Params            : in out Rose.Invocation.Invocation_Record;
      Max_Storage_Units : System.Storage_Elements.Storage_Count);

   procedure Copy_Received_Buffer
     (Max_Bytes : System.Storage_Elements.Storage_Count;
      To        : System.Address);

   procedure Copy_Storage_Array
     (Params    : Rose.Invocation.Invocation_Record;
      To        : out System.Storage_Elements.Storage_Array;
      Last      : out System.Storage_Elements.Storage_Count);

   procedure Copy_Received_Caps
     (Params    : Rose.Invocation.Invocation_Record;
      To        : out Rose.Capabilities.Capability_Array;
      Last      : out Natural);

   procedure Copy_Text
     (Params   : Rose.Invocation.Invocation_Record;
      Count    : Natural;
      To       : out String;
      Last     : out Natural);

   function Get_Word_32
     (Params : Rose.Invocation.Invocation_Record;
      Index  : Rose.Invocation.Parameter_Word_Index)
     return Rose.Words.Word_32;

   function Get_Word_64
     (Params : Rose.Invocation.Invocation_Record;
      Index  : Rose.Invocation.Parameter_Word_Index)
      return Rose.Words.Word_64;

   procedure Initialize_Send
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability);

   procedure Initialize_Receive
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability);

   procedure Initialize_Reply
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability);

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Natural);

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word_32);

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word_64);

   procedure Send_Endpoint
     (Params   : in out Rose.Invocation.Invocation_Record;
      Endpoint : Rose.Objects.Endpoint_Id);

   procedure Send_Object_Id
     (Params : in out Rose.Invocation.Invocation_Record;
      Oid    : Rose.Objects.Object_Id);

   procedure Send_Error
     (Params : in out Rose.Invocation.Invocation_Record;
      Error  : Rose.Invocation.Invocation_Error);

   procedure Receive_Words
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural);

   procedure Receive_Caps
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural);

end Rose.System_Calls;
