with System.Storage_Elements;

with Rose.Capabilities;
with Rose.Objects;
with Rose.Words;                       use Rose.Words;

package Rose.Invocation is

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
      Unknown_Error);

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
         Reserved       : Word_8 := 0;
         Error          : Invocation_Error := OK;
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

   procedure Send_Object_Id
     (Params : in out Invocation_Record;
      Oid    : Rose.Objects.Object_Id);

   procedure Send_Word
     (Params : in out Invocation_Record;
      Value  : Rose.Words.Word);

   procedure Send_Cap
     (Params : in out Invocation_Record;
      Cap    : Rose.Capabilities.Capability);

   function Get_Object_Id
     (Params : Invocation_Record;
      Index  : Parameter_Word_Index)
      return Rose.Objects.Object_Id;

   procedure Set_Error
     (Params  : in out Invocation_Record;
      Error   : Invocation_Error;
      X, Y, Z : Rose.Words.Word := 0);

   procedure For_Each_Sent_Word
     (Params  : Invocation_Record;
      Process : not null access
        procedure (Value : Rose.Words.Word));

end Rose.Invocation;
