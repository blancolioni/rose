with System;

with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Invocation;
with Rose.Objects;
with Rose.Words;

package Rose.System_Calls is

   type Sent_Words_Array is array (Positive range <>) of Rose.Words.Word;
   No_Sent_Words : Sent_Words_Array (1 .. 0);

   subtype Sent_Caps_Array is Rose.Capabilities.Capability_Array;
   No_Sent_Caps : Sent_Caps_Array (1 .. 0);

   type Invocation_Flag_Array is
     array (Positive range <>) of Rose.Invocation.Invocation_Flag;

   procedure Invoke_Capability
     (Item : aliased in out Rose.Invocation.Invocation_Record);
   --  If Item already has the error No_Invoke_Buffer, set by a send
   --  routine that had no invoke buffer to use, it returns without
   --  entering the kernel and Item keeps that error.

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

   procedure Expect_Buffer
     (Params : in out Rose.Invocation.Invocation_Record);
   --  Params, a receive, expects the sender's buffer, if it has one. The
   --  kernel gives a process the buffer of a message it receives only if
   --  it receives with Send_Buffer set (#2), and Params.Buffer_Address and
   --  Params.Buffer_Length then describe the buffer that arrived.

   procedure Receive_Buffer
     (Params : in out Rose.Invocation.Invocation_Record);

   procedure Receive_Buffer
     (Params     : in out Rose.Invocation.Invocation_Record;
      Max_Length : System.Storage_Elements.Storage_Count);

   procedure Copy_Received_Buffer
     (Max_Bytes : System.Storage_Elements.Storage_Count;
      To        : System.Address);

   procedure Copy_Storage_Array
     (Params    : Rose.Invocation.Invocation_Record;
      To        : out System.Storage_Elements.Storage_Array;
      Last      : out System.Storage_Elements.Storage_Count);

   function Copy_Received_Caps
     (Params    : Rose.Invocation.Invocation_Record;
      Start     : Rose.Invocation.Capability_Index;
      To        : in out Rose.Capabilities.Capability_Array)
      return Natural;

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

   function Get_Object_Id
     (Params : Rose.Invocation.Invocation_Record;
      Index  : Rose.Invocation.Parameter_Word_Index)
      return Rose.Objects.Object_Id
   is (Rose.Objects.Object_Id (Get_Word_64 (Params, Index)));

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

   procedure Send_Storage_Offset
     (Params : in out Rose.Invocation.Invocation_Record;
      Offset : System.Storage_Elements.Storage_Offset);

   procedure Send_Error
     (Params : in out Rose.Invocation.Invocation_Record;
      Error  : Rose.Invocation.Invocation_Error);

   procedure Receive_Words
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural);

   procedure Receive_Caps
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural);

   function Create_Endpoint_Capability return Rose.Capabilities.Capability;
   function Delete_Cap_Capability return Rose.Capabilities.Capability;
   function Rescind_Cap_Capability return Rose.Capabilities.Capability;
   --  The process's caps for creating endpoints, and for deleting and
   --  rescinding caps. A process that has called Use_Capabilities has the
   --  ones it gave, and a null cap for the others. One that has not gets
   --  them from its argument cap set the first time it asks, as a process
   --  that exec starts has one (Rose.Capabilities.Standard).

   procedure Delete_Cap (Cap : Rose.Capabilities.Capability);
   procedure Rescind_Cap (Cap : Rose.Capabilities.Capability);
   --  Delete or rescind Cap. Nothing happens if Cap is null, or if the
   --  process has no cap to delete or rescind with.

   procedure Use_Capabilities
     (Create_Endpoint, Delete_Cap, Rescind_Cap : Rose.Capabilities.Capability
      := Rose.Capabilities.Null_Capability);
   --  The process's caps for creating endpoints, and for deleting and
   --  rescinding caps. Only the caps that are not null are recorded, so
   --  a process can give them in more than one call. After the first
   --  call, a local cap that has not been given is null (#399): every
   --  process that is not started by exec calls this, since it has no
   --  argument cap set to find them in.

   procedure Use_Buffer
     (Buffer_Address : System.Address;
      Buffer_Size    : System.Storage_Elements.Storage_Count);

   function Have_Buffer return Boolean;

   function Buffer_Address return System.Address;
   function Buffer_Length return System.Storage_Elements.Storage_Count;
   --  The buffer given to Use_Buffer, so that it can be put back after
   --  being replaced.

end Rose.System_Calls;
