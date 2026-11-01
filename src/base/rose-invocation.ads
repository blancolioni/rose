with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;                       use Rose.Words;

package Rose.Invocation
  with SPARK_Mode
is

   pragma Preelaborate;

   pragma Assertion_Policy (Pre => Ignore, Post => Ignore);
   --  The contracts below are proved (make prove), not executed: the
   --  boot servers are built with assertions on against a run-time system
   --  that has no System.Assertions to raise Assert_Failure with.

   --  Invocation flags:
   --    Error         An error occurred while processing request.
   --                  The error type can be found in Words (0)
   --    Send          This is an unprompted send
   --    Receive       This is a request for messages
   --    Reply         This is a reply to an earlier message
   --    Block         Sender is willing to block for reply

   --  Receive + Block: sender will be block until a message is received

   type Invocation_Flag is
     (Writable_Buffer,
      Send_Buffer,
      No_Trace,
      Error,
      Send,
      Receive,
      Reply,
      Block,
      Create_Reply_Cap,
      Send_Words,
      Recv_Words,
      Send_Caps,
      Recv_Caps);

   type Invocation_Flags is array (Invocation_Flag) of Boolean;
   pragma Pack (Invocation_Flags);

   type Invocation_Error is
     (OK,
      Invalid_Capability,
      Out_Of_Capabilities,
      Out_Of_Endpoints,
      Unknown_Request,
      Request_Error,
      No_Access,
      Request_Would_Block,
      Closed,
      Invalid_Endpoint,
      Invalid_Operation,
      Invalid_Object,
      Operation_Not_Implemented,
      Missing_Data,
      Unknown_Error,
      Undefined_Error,
      No_Invoke_Buffer,
      Buffer_Too_Small,
      Exception_Raised);
   --  No_Invoke_Buffer is reported by the user library, and never by the
   --  kernel, when a process sends text or a storage array without having
   --  set up an invoke buffer (#239). Buffer_Too_Small is reported by the
   --  user library too, when the items of an invocation do not fit in the
   --  invoke buffer (idle#58). Exception_Raised is sent by a generated
   --  server when the handler of an operation raises an exception
   --  (idle#60).

   use type Rose.Capabilities.Capability;
   use type Rose.Objects.Object_Id;

   Max_Parameter_Words : constant := 16;
   Max_Capabilities    : constant := 16;

   type Parameter_Word_Index is mod Max_Parameter_Words;
   type Capability_Index is mod Max_Capabilities;

   type Parameter_Words is
     array (Parameter_Word_Index) of Rose.Words.Word;

   type Capability_Words is
     array (Capability_Index) of Rose.Capabilities.Capability;

   type Error_Index is mod 2 ** 4;

   type Control_Word_Header is new Word_8;

   type Word_3 is mod 2 ** 3;
   type Word_4 is mod 2 ** 4;

   type Control_Word is
      record
         Flags          : Invocation_Flags  := (others => False);
         Last_Sent_Word : Parameter_Word_Index := 0;
         Last_Recv_Word : Parameter_Word_Index := 0;
         Last_Sent_Cap  : Capability_Index     := 0;
         Last_Recv_Cap  : Capability_Index     := 0;
         Reserved       : Word_3               := 0;
      end record
     with Pack, Size => 32;

   type Invocation_Record is
      record
         Control        : Control_Word;
         Cap            : Rose.Capabilities.Capability := 0;
         Reply_Cap      : Rose.Capabilities.Capability := 0;
         Endpoint       : Rose.Objects.Endpoint_Id     := 0;
         Identifier     : Rose.Objects.Capability_Identifier := 0;
         Unused         : Word_8 := 0;
         Error          : Invocation_Error := OK;
         Extra_Errors   : Word_3 := 0;
         Data           : Parameter_Words := (others => 0);
         Caps           : Capability_Words := (others => 0);
         Buffer_Address : System.Address := System.Null_Address;
         Buffer_Length  : System.Storage_Elements.Storage_Count := 0;
      end record
     with Pack, Size => 40 * 4 * 8;

   type Invocation_Access is access all Invocation_Record;

   function Waiting (Invocation : Invocation_Record) return Boolean
   is (Invocation.Control.Flags (Block)
       or else Invocation.Control.Flags (Receive));

   Object_Fits_In_Word : constant Boolean :=
                           Rose.Objects.Object_Id'Size <= Word'Size;

   function Sent_Word_Count (Params : Invocation_Record) return Natural
   is (if Params.Control.Flags (Send_Words)
       then Natural (Params.Control.Last_Sent_Word) + 1
       else 0);
   --  The number of words that have been sent, which is also how many of
   --  Data are in use: Last_Sent_Word is the index of the last one, and is
   --  not meaningful until Send_Words is set.

   function Sent_Cap_Count (Params : Invocation_Record) return Natural
   is (if Params.Control.Flags (Send_Caps)
       then Natural (Params.Control.Last_Sent_Cap) + 1
       else 0);
   --  The same for the capabilities, and Caps.

   function Object_Id_Words return Positive
   is (if Object_Fits_In_Word then 1 else 2);
   --  How many words Send_Object_Id sends.

   --  The indices that count the words and capabilities sent are modular,
   --  so sending one past the end would wrap around and overwrite the
   --  first, instead of failing.  The preconditions say that there is room.

   procedure Send_Object_Id
     (Params : in out Invocation_Record;
      Oid    : Rose.Objects.Object_Id)
   with
     Pre  => Sent_Word_Count (Params) <= Max_Parameter_Words - Object_Id_Words,
     Post => Sent_Word_Count (Params) =
               Sent_Word_Count (Params'Old) + Object_Id_Words
             and then Params.Control.Flags (Send_Words);

   procedure Send_Word
     (Params : in out Invocation_Record;
      Value  : Rose.Words.Word)
   with
     Pre  => Sent_Word_Count (Params) < Max_Parameter_Words,
     Post => Sent_Word_Count (Params) = Sent_Word_Count (Params'Old) + 1
             and then Params.Data (Params.Control.Last_Sent_Word) = Value
             and then (for all I in Parameter_Word_Index =>
                         (if Natural (I) < Sent_Word_Count (Params'Old)
                          then Params.Data (I) = Params'Old.Data (I)));

   procedure Send_Cap
     (Params : in out Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   with
     Pre  => Sent_Cap_Count (Params) < Max_Capabilities,
     Post => Sent_Cap_Count (Params) = Sent_Cap_Count (Params'Old) + 1
             and then Params.Caps (Params.Control.Last_Sent_Cap) = Cap
             and then (for all I in Capability_Index =>
                         (if Natural (I) < Sent_Cap_Count (Params'Old)
                          then Params.Caps (I) = Params'Old.Caps (I)));

   function Get_Object_Id
     (Params : Invocation_Record;
      Index  : Parameter_Word_Index)
      return Rose.Objects.Object_Id
   with
     Pre  => Natural (Index) <= Max_Parameter_Words - Object_Id_Words,
     Post => Get_Object_Id'Result =
               Rose.Objects.Object_Id (Params.Data (Index))
               + (if Object_Fits_In_Word then 0
                  else Rose.Objects.Object_Id (Params.Data (Index + 1))
                         * 2 ** 32);
   --  The id in Data (Index) and, if it takes two words, the next.

   procedure Set_Error
     (Params  : in out Invocation_Record;
      Error   : Invocation_Error;
      X, Y, Z : Rose.Words.Word := 0)
   with
     Post => Params.Control.Flags (Rose.Invocation.Error)
             and then Sent_Word_Count (Params) = 4;

   procedure For_Each_Sent_Word
     (Params  : Invocation_Record;
      Process : not null access
        procedure (Value : Rose.Words.Word));

end Rose.Invocation;
