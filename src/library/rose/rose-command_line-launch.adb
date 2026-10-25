with Rose.Capabilities.Standard;
with Rose.Last_Chance;
with Rose.Standard_Streams;
with Rose.System_Calls;

package body Rose.Command_Line.Launch is

   use System.Storage_Elements;

   Reply_Cap : Rose.Capabilities.Capability :=
     Rose.Capabilities.Null_Capability;
   --  The reply cap of the launch invocation, until the command replies.

   Completed : constant Rose.Words.Word := 0;
   Failed    : constant Rose.Words.Word := 1;

   procedure Reply
     (Status  : Rose.Words.Word;
      Message : String);
   --  Reply to the launch invocation, if that has not been done.

   procedure Set_Word
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Rose.Words.Word);

   procedure Report_Failure (Message : String);
   --  Fail the launch invocation with Message: the handler of unhandled
   --  exceptions.

   ----------------------
   -- Boolean_Argument --
   ----------------------

   function Boolean_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Boolean
   is
      use type Rose.Words.Word;
   begin
      return Call.Params.Data (Rose.Invocation.Parameter_Word_Index (Word))
        /= 0;
   end Boolean_Argument;

   --------------------
   -- Boolean_Result --
   --------------------

   procedure Boolean_Result
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Boolean)
   is
   begin
      Set_Word (Call, Word, (if Value then 1 else 0));
   end Boolean_Result;

   ------------------
   -- Cap_Argument --
   ------------------

   function Cap_Argument
     (Call  : Launch_Call;
      Index : Natural)
      return Rose.Capabilities.Capability
   is
   begin
      return Call.Params.Caps (Rose.Invocation.Capability_Index (Index));
   end Cap_Argument;

   --------------
   -- Complete --
   --------------

   procedure Complete (Call : in out Launch_Call) is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Reply_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Reply (Params, Reply_Cap);
      Rose.System_Calls.Send_Word (Params, Completed);
      for Value of Call.Result_Words (1 .. Call.Word_Count) loop
         Rose.System_Calls.Send_Word (Params, Value);
      end loop;
      if Call.Results_Length > 0 then
         Rose.System_Calls.Send_Storage_Array
           (Params, Call.Results (1 .. Call.Results_Length), False);
      end if;
      Reply_Cap := Rose.Capabilities.Null_Capability;
      Rose.System_Calls.Invoke_Capability (Params);
   end Complete;

   ----------
   -- Fail --
   ----------

   procedure Fail
     (Call    : in out Launch_Call;
      Message : String)
   is
      pragma Unreferenced (Call);
   begin
      Reply (Failed, Message);
   end Fail;

   ----------------------
   -- Natural_Argument --
   ----------------------

   function Natural_Argument
     (Call : Launch_Call;
      Word : Natural)
      return Natural
   is
   begin
      return Natural
        (Call.Params.Data (Rose.Invocation.Parameter_Word_Index (Word)));
   end Natural_Argument;

   --------------------
   -- Natural_Result --
   --------------------

   procedure Natural_Result
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Natural)
   is
   begin
      Set_Word (Call, Word, Rose.Words.Word (Value));
   end Natural_Result;

   -------------
   -- Receive --
   -------------

   procedure Receive (Call : in out Launch_Call) is
      use type Rose.Capabilities.Capability;
   begin
      Rose.Standard_Streams.Set;

      Rose.System_Calls.Initialize_Receive
        (Call.Params, Rose.Capabilities.Standard.Command_Endpoint_Cap);
      Rose.System_Calls.Receive_Words
        (Call.Params,
         Natural (Rose.Invocation.Parameter_Word_Index'Last) + 1);
      Rose.System_Calls.Receive_Caps
        (Call.Params, Natural (Rose.Invocation.Capability_Index'Last) + 1);
      Rose.System_Calls.Invoke_Capability (Call.Params);

      Call.Buffer_Length := 0;
      if Call.Params.Control.Flags (Rose.Invocation.Send_Buffer) then
         Call.Buffer_Length :=
           Storage_Count'Min (Call.Params.Buffer_Length, Max_Buffer_Length);
         declare
            Received : constant Storage_Array (1 .. Call.Buffer_Length)
              with Import, Address => Call.Params.Buffer_Address;
         begin
            Call.Buffer (1 .. Call.Buffer_Length) := Received;
         end;
      end if;

      Reply_Cap := Call.Params.Reply_Cap;
      if Reply_Cap /= Rose.Capabilities.Null_Capability then
         Rose.Last_Chance.Set_Handler (Report_Failure'Access);
      end if;
   end Receive;

   -----------
   -- Reply --
   -----------

   procedure Reply
     (Status  : Rose.Words.Word;
      Message : String)
   is
      use type Rose.Capabilities.Capability;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      if Reply_Cap = Rose.Capabilities.Null_Capability then
         return;
      end if;

      Rose.System_Calls.Initialize_Reply (Params, Reply_Cap);
      Rose.System_Calls.Send_Word (Params, Status);
      if Message'Length > 0 then
         Rose.System_Calls.Send_Text (Params, Message);
      end if;
      Reply_Cap := Rose.Capabilities.Null_Capability;
      Rose.System_Calls.Invoke_Capability (Params);
   end Reply;

   --------------------
   -- Report_Failure --
   --------------------

   procedure Report_Failure (Message : String) is
   begin
      Reply (Failed, Message);
   end Report_Failure;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Call  : in out Launch_Call;
      Word  : Positive;
      Value : Rose.Words.Word)
   is
   begin
      if Word <= Max_Result_Words then
         Call.Result_Words (Word) := Value;
         Call.Word_Count := Natural'Max (Call.Word_Count, Word);
      end if;
   end Set_Word;

   ---------------------
   -- String_Argument --
   ---------------------

   function String_Argument
     (Call : Launch_Call;
      Item : Positive)
      return String
   is
      All_Items : constant String_List :=
        (Buffer        => Call.Buffer'Address,
         Buffer_Length => Call.Buffer_Length,
         First_Item    => 1,
         Count         => Item);
   begin
      return Element (All_Items, Item);
   end String_Argument;

   --------------------------
   -- String_List_Argument --
   --------------------------

   function String_List_Argument
     (Call       : Launch_Call;
      First_Item : Positive;
      Count_Word : Natural)
      return String_List
   is
   begin
      return (Buffer        => Call.Buffer'Address,
              Buffer_Length => Call.Buffer_Length,
              First_Item    => First_Item,
              Count         => Natural_Argument (Call, Count_Word));
   end String_List_Argument;

   -------------------
   -- String_Result --
   -------------------

   procedure String_Result
     (Call  : in out Launch_Call;
      Item  : Positive;
      Value : String)
   is
      pragma Unreferenced (Item);
      Length : constant Storage_Count := Value'Length;
      Size   : constant Storage_Count := 4 + (Length + 3) / 4 * 4;
      Rest   : Storage_Count := Length;
      First  : constant Storage_Offset := Call.Results_Length;
   begin
      if First + Size > Max_Buffer_Length then
         return;
      end if;

      --  The length, little-endian, then the text, then padding.

      for I in Storage_Offset range 1 .. 4 loop
         Call.Results (First + I) := Storage_Element (Rest mod 256);
         Rest := Rest / 256;
      end loop;
      for I in Value'Range loop
         Call.Results (First + 5 + Storage_Offset (I - Value'First)) :=
           Character'Pos (Value (I));
      end loop;
      Call.Results (First + 5 + Length .. First + Size) := (others => 0);
      Call.Results_Length := First + Size;
   end String_Result;

   ------------------------
   -- Use_Standard_Error --
   ------------------------

   procedure Use_Standard_Error
     (Call      : Launch_Call;
      Cap_Index : Natural)
   is
   begin
      Rose.Standard_Streams.Set_Standard_Error
        (Cap_Argument (Call, Cap_Index));
   end Use_Standard_Error;

   ------------------------
   -- Use_Standard_Input --
   ------------------------

   procedure Use_Standard_Input
     (Call      : Launch_Call;
      Cap_Index : Natural)
   is
   begin
      Rose.Standard_Streams.Set_Standard_Input
        (Cap_Argument (Call, Cap_Index));
   end Use_Standard_Input;

   -------------------------
   -- Use_Standard_Output --
   -------------------------

   procedure Use_Standard_Output
     (Call      : Launch_Call;
      Cap_Index : Natural)
   is
   begin
      Rose.Standard_Streams.Set_Standard_Output
        (Cap_Argument (Call, Cap_Index));
   end Use_Standard_Output;

end Rose.Command_Line.Launch;
