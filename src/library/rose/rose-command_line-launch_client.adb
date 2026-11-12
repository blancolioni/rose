with System.Storage_Elements;

with Rose.Command_Line.Items;
with Rose.Interfaces.Objects;
with Rose.System_Calls;

package body Rose.Command_Line.Launch_Client is

   use System.Storage_Elements;
   use type Rose.Words.Word;

   Max_Buffer_Length : constant := 4096;
   --  As Rose.Command_Line.Launch.Max_Buffer_Length.

   Buffer : Storage_Array (1 .. Max_Buffer_Length)
     with Alignment => 4096;
   --  The buffer of the launch call, which the kernel shares with the
   --  command page by page. It is lent writable, so that the command can
   --  write its string results, or its failure message, into it (#431).

   Buffer_Last : Storage_Count := 0;
   --  The end of the items added since the last Start.

   Max_Result_Caps : constant := 4;
   --  As Rose.Command_Line.Launch.Max_Result_Caps.

   procedure Add_Item (Value : String);
   --  Add Value to the buffer as the next item. Raises Constraint_Error if
   --  it doesn't fit.

   procedure Set_Cap
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability);

   procedure Set_Word
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Rose.Words.Word);

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item (Value : String) is
      Length : constant Storage_Count := Value'Length;
      Size   : constant Storage_Count := 4 + (Length + 3) / 4 * 4;
      Rest   : Storage_Count := Length;
   begin
      if Buffer_Last + Size > Buffer'Last then
         raise Constraint_Error with "launch arguments too long";
      end if;

      --  The length, little-endian, then the text, then padding.

      for I in Storage_Offset range 1 .. 4 loop
         Buffer (Buffer_Last + I) := Storage_Element (Rest mod 256);
         Rest := Rest / 256;
      end loop;
      for I in Value'Range loop
         Buffer (Buffer_Last + 5 + Storage_Offset (I - Value'First)) :=
           Character'Pos (Value (I));
      end loop;
      Buffer (Buffer_Last + 5 + Length .. Buffer_Last + Size) :=
        (others => 0);
      Buffer_Last := Buffer_Last + Size;
   end Add_Item;

   ----------------------
   -- Boolean_Argument --
   ----------------------

   procedure Boolean_Argument
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Boolean)
   is
   begin
      Set_Word (Call, Word, (if Value then 1 else 0));
   end Boolean_Argument;

   --------------------
   -- Boolean_Result --
   --------------------

   function Boolean_Result
     (Call : Launch_Call;
      Word : Positive)
      return Boolean
   is (Natural_Result (Call, Word) /= 0);

   ------------------
   -- Cap_Argument --
   ------------------

   procedure Cap_Argument
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      Set_Cap (Call, Index, Cap);
   end Cap_Argument;

   ----------------
   -- Cap_Result --
   ----------------

   function Cap_Result
     (Call  : Launch_Call;
      Index : Natural)
      return Rose.Capabilities.Capability
   is
   begin
      if Index >= Rose.Invocation.Sent_Cap_Count (Call.Params) then
         return Rose.Capabilities.Null_Capability;
      end if;
      return Call.Params.Caps (Rose.Invocation.Capability_Index (Index));
   end Cap_Result;

   ------------
   -- Invoke --
   ------------

   procedure Invoke (Call : in out Launch_Call) is
      Completed : constant Rose.Words.Word := 0;
   begin
      Rose.System_Calls.Initialize_Send (Call.Params, Call.Endpoint);
      for Value of Call.Words (0 .. Call.Word_Count - 1) loop
         Rose.System_Calls.Send_Word (Call.Params, Value);
      end loop;
      for Cap of Call.Caps (0 .. Call.Cap_Count - 1) loop
         Rose.System_Calls.Send_Cap (Call.Params, Cap);
      end loop;
      Rose.System_Calls.Send_Buffer
        (Call.Params, Buffer'Length, Buffer'Address, True);
      Rose.System_Calls.Receive_Words
        (Call.Params, Rose.Invocation.Max_Parameter_Words);
      Rose.System_Calls.Receive_Caps (Call.Params, Max_Result_Caps);
      Rose.System_Calls.Invoke_Capability (Call.Params);

      if Call.Params.Control.Flags (Rose.Invocation.Error) then
         raise Rose.Interfaces.Objects.Invocation_Error
           with "launch call failed";
      end if;

      if Rose.Invocation.Sent_Word_Count (Call.Params) = 0 then
         raise Rose.Interfaces.Objects.Invocation_Error
           with "not a launch reply";
      end if;

      if Call.Params.Data (0) /= Completed then
         if Rose.Invocation.Sent_Word_Count (Call.Params) > 1 then
            declare
               Length  : constant Storage_Count :=
                 Storage_Count'Min
                   (Storage_Count (Call.Params.Data (1)), Buffer'Length);
               Message : String (1 .. Natural (Length));
            begin
               for I in Message'Range loop
                  Message (I) :=
                    Character'Val (Buffer (Storage_Offset (I)));
               end loop;
               raise Rose.Interfaces.Objects.Invocation_Error with Message;
            end;
         end if;
         raise Rose.Interfaces.Objects.Invocation_Error
           with "command failed";
      end if;
   end Invoke;

   ----------------------
   -- Natural_Argument --
   ----------------------

   procedure Natural_Argument
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Natural)
   is
   begin
      Set_Word (Call, Word, Rose.Words.Word (Value));
   end Natural_Argument;

   --------------------
   -- Natural_Result --
   --------------------

   function Natural_Result
     (Call : Launch_Call;
      Word : Positive)
      return Natural
   is
   begin
      if Word >= Rose.Invocation.Sent_Word_Count (Call.Params) then
         return 0;
      end if;
      return Natural
        (Call.Params.Data (Rose.Invocation.Parameter_Word_Index (Word)));
   end Natural_Result;

   ---------------------------
   -- Optional_Cap_Argument --
   ---------------------------

   procedure Optional_Cap_Argument
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability)
   is
      use type Rose.Capabilities.Capability;
   begin
      if Cap /= Rose.Capabilities.Null_Capability then
         Set_Cap (Call, Index, Cap);
      end if;
   end Optional_Cap_Argument;

   -------------
   -- Set_Cap --
   -------------

   procedure Set_Cap
     (Call  : in out Launch_Call;
      Index : Natural;
      Cap   : Rose.Capabilities.Capability)
   is
   begin
      Call.Caps (Index) := Cap;
      Call.Cap_Count := Natural'Max (Call.Cap_Count, Index + 1);
   end Set_Cap;

   --------------
   -- Set_Word --
   --------------

   procedure Set_Word
     (Call  : in out Launch_Call;
      Word  : Natural;
      Value : Rose.Words.Word)
   is
   begin
      Call.Words (Word) := Value;
      Call.Word_Count := Natural'Max (Call.Word_Count, Word + 1);
   end Set_Word;

   -----------
   -- Start --
   -----------

   procedure Start
     (Call     : in out Launch_Call;
      Endpoint : Rose.Capabilities.Capability)
   is
   begin
      Call.Endpoint := Endpoint;
      Call.Caps := (others => Rose.Capabilities.Null_Capability);
      Call.Cap_Count := 0;
      Call.Words := (others => 0);
      Call.Word_Count := 0;
      Buffer_Last := 0;
   end Start;

   ---------------------
   -- String_Argument --
   ---------------------

   procedure String_Argument
     (Call  : in out Launch_Call;
      Value : String)
   is
      pragma Unreferenced (Call);
   begin
      Add_Item (Value);
   end String_Argument;

   --------------------------
   -- String_List_Argument --
   --------------------------

   procedure String_List_Argument
     (Call       : in out Launch_Call;
      Count_Word : Natural;
      Value      : String_List)
   is
   begin
      Set_Word (Call, Count_Word, Rose.Words.Word (Length (Value)));
      for Index in 1 .. Length (Value) loop
         Add_Item (Element (Value, Index));
      end loop;
   end String_List_Argument;

   -------------------
   -- String_Result --
   -------------------

   function String_Result
     (Call : Launch_Call;
      Item : Positive)
      return String
   is
      pragma Unreferenced (Call);
   begin
      return Rose.Command_Line.Items.Element
        (Buffer, First_Item => 1, Count => Item, Index => Item);
   end String_Result;

end Rose.Command_Line.Launch_Client;
