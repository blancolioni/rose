with Rose.Capabilities;

private with Rose.Invocation;
private with Rose.Words;

package Rose.Command_Line.Launch_Client is

   --  The launch call of a command (ADR 0002), as the program that
   --  launched it makes it: the caller's half of Rose.Command_Line.Launch,
   --  whose description gives each argument's place in the call. The
   --  client that the user-mode IDL target generates for a command,
   --  <Command>_Client (blancolioni/idle#103), is made of these calls, so a
   --  program calls that client, and not these.
   --
   --  The arguments are set by their places, and the call is made with
   --  Invoke. Strings are buffer items, and are added in the order of
   --  their item numbers. The results are read after Invoke. Out strings
   --  come back in the buffer, which is the package's own, so they must
   --  be read before the next call.

   type Launch_Call is limited private;

   procedure Start
     (Call     : in out Launch_Call;
      Endpoint : Rose.Capabilities.Capability);
   --  Start a launch call on Endpoint, the command endpoint that
   --  Launch.Launch_Command returned.

   procedure Cap_Argument
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability);
   --  Send Cap as cap Index, even if it is the null cap. A cap before it
   --  that isn't set is sent as the null cap.

   procedure Optional_Cap_Argument
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability);
   --  Send Cap as cap Index, unless it is the null cap and no later cap is
   --  sent: as a replaceable server's Previous and Saved are, so that a
   --  fresh start sends neither (Rose ADR 0032).

   procedure Boolean_Argument
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Boolean);

   procedure Natural_Argument
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Natural);

   procedure String_Argument
     (Call  : in out Launch_Call;
      Value : String);
   --  Add Value as the next buffer item.

   procedure String_List_Argument
     (Call       : in out Launch_Call;
      Count_Word : Natural;
      Value      : String_List);
   --  Add the strings of Value as the next buffer items, and send their
   --  number as data word Count_Word.

   procedure Invoke (Call : in out Launch_Call);
   --  Make the call. Raises Rose.Interfaces.Objects.Invocation_Error if the
   --  call fails, or if the command fails, with the command's message, if
   --  it sent one.

   function Cap_Result
     (Call  : Launch_Call;
      Index : Natural)
      return Rose.Capabilities.Capability;
   --  The command's cap result Index, or the null cap if it sent none.

   function Boolean_Result
     (Call : Launch_Call;
      Word : Positive)
      return Boolean;

   function Natural_Result
     (Call : Launch_Call;
      Word : Positive)
      return Natural;

   function String_Result
     (Call : Launch_Call;
      Item : Positive)
      return String;
   --  The command's string result Item, or "" if it sent none.

private

   Max_Caps : constant := 16;

   type Cap_Array is
     array (0 .. Max_Caps - 1) of Rose.Capabilities.Capability;

   type Word_Array is
     array (0 .. Rose.Invocation.Max_Parameter_Words - 1) of
       Rose.Words.Word;

   type Launch_Call is limited
      record
         Endpoint   : Rose.Capabilities.Capability :=
           Rose.Capabilities.Null_Capability;
         Caps       : Cap_Array :=
           (others => Rose.Capabilities.Null_Capability);
         Cap_Count  : Natural := 0;
         Words      : Word_Array := (others => 0);
         Word_Count : Natural := 0;
         Params     : aliased Rose.Invocation.Invocation_Record;
      end record;

end Rose.Command_Line.Launch_Client;
