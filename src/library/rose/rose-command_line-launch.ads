with Rose.Capabilities;

private with Rose.Invocation;
private with Rose.Words;

package Rose.Command_Line.Launch is

   --  The launch invocation of a command (ADR 0002). When the kernel
   --  creates a command's process, it gives it the receive cap of a
   --  command endpoint (Rose.Capabilities.Standard.Command_Endpoint_Cap),
   --  and gives the matching endpoint cap to whoever launched it. The
   --  shell calls that endpoint once, with the command's arguments. The
   --  main that the IDL compiler generates for the command receives the
   --  call, decodes the arguments, calls the command, and replies.
   --
   --  The IDL compiler gives each argument its place in the call
   --  (blancolioni/idle#2):
   --
   --    caps        the members of the roles the command inherits, in
   --                role order
   --    data words  Boolean (zero is False), Natural and Positive
   --                arguments, and the number of strings in a
   --                String_List argument, in declaration order
   --    buffer      String arguments, in declaration order, then the
   --                strings of a String_List argument
   --
   --  The buffer is a sequence of items, numbered from 1. Each is a
   --  32-bit length, then that many characters, then padding to a
   --  multiple of four bytes.
   --
   --  The reply's first data word is zero if the command completed, and
   --  one if it failed, in which case the buffer holds a message. A reply
   --  that completes the call also holds the command's results: its out
   --  and in out scalars in data words 1 onwards, and its out and in out
   --  strings as buffer items, in the format of the launch buffer, each
   --  in declaration order.

   Max_Buffer_Length : constant := 4096;
   --  The most argument text a call can carry, and the most result text
   --  a reply can carry.

   Max_Result_Length : constant := 1024;
   --  The longest string result.

   Max_Result_Words : constant := 15;
   --  The most scalar results.

   type Launch_Call is limited private;

   procedure Receive (Call : in out Launch_Call);
   --  Wait for the launch invocation. From now on, the program has no
   --  standard streams but those given to it by the Use_ procedures, and
   --  an unhandled exception fails the call.

   function Boolean_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Boolean;

   function Natural_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Natural;

   function Cap_Argument
     (Call  : Launch_Call;
      Index : Natural)
      return Rose.Capabilities.Capability;

   function String_Argument
     (Call : Launch_Call;
      Item : Positive)
      return String;

   function String_List_Argument
     (Call       : Launch_Call;
      First_Item : Positive;
      Count_Word : Natural)
      return String_List;
   --  The list of the strings in items First_Item onwards, whose number is
   --  in data word Count_Word.

   procedure Use_Standard_Input
     (Call      : Launch_Call;
      Cap_Index : Natural);

   procedure Use_Standard_Output
     (Call      : Launch_Call;
      Cap_Index : Natural);

   procedure Use_Standard_Error
     (Call      : Launch_Call;
      Cap_Index : Natural);
   --  Give the stream in cap Cap_Index of Call to Ada.Text_IO. For now, the
   --  shell sends the stream's Read or Write cap, not its interface.

   procedure Boolean_Result
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Boolean);

   procedure Natural_Result
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Natural);

   procedure String_Result
     (Call  : in out Launch_Call;
      Item  : Positive;
      Value : String);
   --  Set a result that Complete sends: data word Word, or buffer item
   --  Item. String results are set in the order of their items.

   procedure Complete (Call : in out Launch_Call);
   --  Reply that the command completed, with its results.

   procedure Fail
     (Call    : in out Launch_Call;
      Message : String);
   --  Reply that the command failed, with Message.

private

   subtype Launch_Buffer is
     System.Storage_Elements.Storage_Array (1 .. Max_Buffer_Length);

   type Result_Word_Array is
     array (1 .. Max_Result_Words) of Rose.Words.Word;

   type Launch_Call is limited
      record
         Params        : aliased Rose.Invocation.Invocation_Record;
         Buffer        : Launch_Buffer;
         Buffer_Length : System.Storage_Elements.Storage_Count := 0;
         Result_Words  : Result_Word_Array := (others => 0);
         Word_Count    : Natural := 0;
         Results       : Launch_Buffer;
         Results_Length : System.Storage_Elements.Storage_Count := 0;
      end record;
   --  The buffer is copied out of the invocation, because the kernel maps
   --  the buffer of each message the process receives, replies included,
   --  at the same place.

end Rose.Command_Line.Launch;
