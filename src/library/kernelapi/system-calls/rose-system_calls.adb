package body Rose.System_Calls is

   Default_32_Bit : constant Boolean :=
                      Rose.Words.Word'Size = 32;

   Local_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   procedure Invoke_Capability_Wrapper
     (Item : aliased in out Rose.Invocation.Invocation_Record);

   procedure Send_Native_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word);

   --------------------------
   -- Copy_Received_Buffer --
   --------------------------

   procedure Copy_Received_Buffer
     (Max_Bytes : System.Storage_Elements.Storage_Count;
      To        : System.Address)
   is
      use System.Storage_Elements;
      Last   : constant Storage_Count :=
                 Storage_Count'Min (Max_Bytes, Local_Buffer'Length);
      Dest   : System.Storage_Elements.Storage_Array (1 .. Last);
      pragma Import (Ada, Dest);
      for Dest'Address use To;
   begin
      Dest := Local_Buffer (1 .. Last);
   end Copy_Received_Buffer;

   ------------------------
   -- Copy_Received_Caps --
   ------------------------

   procedure Copy_Received_Caps
     (Params    : Rose.Invocation.Invocation_Record;
      To        : out Rose.Capabilities.Capability_Array;
      Last      : out Natural)
   is
   begin
      Last := To'First - 1;
      if not Params.Control.Flags (Rose.Invocation.Send_Caps) then
         return;
      end if;

      for Index in 0 .. Params.Control.Last_Recv_Cap loop
         exit when Last >= To'Last;
         Last := Last + 1;
         To (Last) := Params.Caps (Index);
      end loop;
   end Copy_Received_Caps;

   ------------------------
   -- Copy_Storage_Array --
   ------------------------

   procedure Copy_Storage_Array
     (Params    : Rose.Invocation.Invocation_Record;
      To        : out System.Storage_Elements.Storage_Array;
      Last      : out System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;
   begin
      Last := Storage_Count'Min (To'Length, Params.Buffer_Length);

      declare
         Source : Storage_Array (1 .. Last);
         pragma Import (Ada, Source);
         for Source'Address use Params.Buffer_Address;
      begin
         To := Source;
      end;
   end Copy_Storage_Array;

   ---------------
   -- Copy_Text --
   ---------------

   procedure Copy_Text
     (Params   : Rose.Invocation.Invocation_Record;
      Count    : Natural;
      To       : out String;
      Last     : out Natural)
   is
   begin
      if Params.Control.Flags (Rose.Invocation.Send_Buffer) then
         declare
            Text : String (1 .. Natural (Params.Buffer_Length));
            pragma Import (Ada, Text);
            for Text'Address use Params.Buffer_Address;
         begin
            Last := 0;
            for Ch of Text loop
               exit when Last >= To'Last;
               Last := Last + 1;
               To (Last) := Ch;
            end loop;
         end;
      else
         declare
            use System.Storage_Elements;
            Index : Storage_Offset := 0;
         begin
            Last := To'First - 1;
            for I in 1 .. Count loop
               exit when Index >= Local_Buffer'Last;
               exit when Last >= To'Last;
               Index := Index + 1;
               Last := Last + 1;
               To (Last) := Character'Val (Local_Buffer (Index));
            end loop;
         end;
      end if;
   end Copy_Text;

   -----------------
   -- Get_Word_32 --
   -----------------

   function Get_Word_32
     (Params : Rose.Invocation.Invocation_Record;
      Index  : Rose.Invocation.Parameter_Word_Index)
      return Rose.Words.Word_32
   is
   begin
      return Rose.Words.Word_32 (Params.Data (Index));
   end Get_Word_32;

   -----------------
   -- Get_Word_64 --
   -----------------

   function Get_Word_64
     (Params : Rose.Invocation.Invocation_Record;
      Index  : Rose.Invocation.Parameter_Word_Index)
      return Rose.Words.Word_64
   is
      use Rose.Invocation;
      use Rose.Words;
   begin
      if Default_32_Bit then
         return Word_64 (Params.Data (Index))
           + 2 ** 32 * Word_64 (Params.Data (Index + 1));
      else
         return Word_64 (Params.Data (Index));
      end if;
   end Get_Word_64;

   ------------------------
   -- Initialize_Receive --
   ------------------------

   procedure Initialize_Receive
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   is
      use Rose.Invocation;
   begin
      Params := (others => <>);
      Params.Control.Flags (Receive) := True;
      Params.Control.Flags (Block) := True;
      Params.Cap := Cap;
   end Initialize_Receive;

   ----------------------
   -- Initialize_Reply --
   ----------------------

   procedure Initialize_Reply
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   is
      use Rose.Invocation;
   begin
      Params := (others => <>);
      Params.Control.Flags (Reply) := True;
      Params.Cap := Cap;
   end Initialize_Reply;

   ---------------------
   -- Initialize_Send --
   ---------------------

   procedure Initialize_Send
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   is
      use Rose.Invocation;
   begin
      Params := (others => <>);
      Params.Control.Flags (Send) := True;
      Params.Control.Flags (Block) := True;
      Params.Control.Flags (Create_Reply_Cap) := True;
      Params.Cap := Cap;
   end Initialize_Send;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Cap       : Rose.Capabilities.Capability;
      Flags     : Invocation_Flag_Array;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array)
   is
      use Rose.Invocation;
      Params         : aliased Rose.Invocation.Invocation_Record;
      Has_Send_Words : constant Boolean := In_Words'Length > 0;
      Has_Send_Caps  : constant Boolean := In_Caps'Length > 0;
      Has_Recv_Words : constant Boolean := Out_Words'Length > 0;
      Has_Recv_Caps  : constant Boolean := Out_Caps'Length > 0;

      Last_Sent_Word : Parameter_Word_Index renames
                         Params.Control.Last_Sent_Word;
      Last_Sent_Cap  : Capability_Index renames
                         Params.Control.Last_Sent_Cap;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send_Words => Has_Send_Words,
                               Send_Caps  => Has_Send_Caps,
                               Recv_Words => Has_Recv_Words,
                               Recv_Caps  => Has_Recv_Caps,
                               others     => False);

      for Flag of Flags loop
         Params.Control.Flags (Flag) := True;
      end loop;

      if Has_Send_Words then
         Last_Sent_Word := Parameter_Word_Index'Last;
         for Value of In_Words loop
            Last_Sent_Word := Last_Sent_Word + 1;
            Params.Data (Last_Sent_Word) := Value;
         end loop;
      end if;

      if Has_Send_Caps then
         Last_Sent_Cap := Capability_Index'Last;
         for Value of In_Caps loop
            Last_Sent_Cap := Last_Sent_Cap + 1;
            Params.Caps (Last_Sent_Cap) := Value;
         end loop;
      end if;

      if Out_Words'Length > 0 then
         Params.Control.Last_Recv_Word :=
           Parameter_Word_Index (Out_Words'Length - 1);
      end if;

      if Out_Caps'Length > 0 then
         Params.Control.Last_Recv_Cap :=
           Capability_Index (Out_Caps'Length - 1);
      end if;

      Rose.System_Calls.Invoke_Capability (Params);

      if Out_Words'Length > 0 and then Params.Control.Flags (Send_Words) then
         declare
            Word_Index : Parameter_Word_Index := 0;
         begin
            for I in Out_Words'Range loop
               Out_Words (I) := Params.Data (Word_Index);
               Word_Index := Word_Index + 1;
               exit when Word_Index > Params.Control.Last_Sent_Word;
            end loop;
         end;
      end if;

      if Out_Caps'Length > 0 and then Params.Control.Flags (Send_Caps) then
         declare
            Cap_Index : Capability_Index := 0;
         begin
            for Cap of Out_Caps loop
               Cap := Params.Caps (Cap_Index);
               Cap_Index := Cap_Index + 1;
               exit when Cap_Index > Params.Control.Last_Sent_Cap;
            end loop;
         end;
      end if;

   end Invoke;

   --------------------------
   -- Invoke_Blocking_Send --
   --------------------------

   procedure Invoke_Blocking_Send
     (Cap       : Rose.Capabilities.Capability;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array)
   is
      use Rose.Invocation;
   begin
      Invoke (Cap, (Send, Block, Create_Reply_Cap),
              In_Words, In_Caps, Out_Words, Out_Caps);
   end Invoke_Blocking_Send;

   -----------------------
   -- Invoke_Capability --
   -----------------------

   procedure Invoke_Capability
     (Item : aliased in out Rose.Invocation.Invocation_Record)
   is
      use Rose.Words;
      Check_Stack : array (Word_8) of Word_8;
   begin
      for I in Check_Stack'Range loop
         Check_Stack (I) := I;
      end loop;
      Invoke_Capability_Wrapper (Item);
      for I in Check_Stack'Range loop
         if Check_Stack (I) /= I then
            Check_Stack (0) := 1 / Check_Stack (0);
         end if;
      end loop;
   end Invoke_Capability;

   -------------------------------
   -- Invoke_Capability_Wrapper --
   -------------------------------

   procedure Invoke_Capability_Wrapper
     (Item : aliased in out Rose.Invocation.Invocation_Record)
   is separate;

   ------------------
   -- Invoke_Reply --
   ------------------

   procedure Invoke_Reply
     (Cap       : Rose.Capabilities.Capability;
      In_Words  : Sent_Words_Array;
      In_Caps   : Sent_Caps_Array;
      Out_Words : out Sent_Words_Array;
      Out_Caps  : out Sent_Caps_Array)
   is
      use Rose.Invocation;
   begin
      Invoke (Cap, (1 => Reply), In_Words, In_Caps, Out_Words, Out_Caps);
   end Invoke_Reply;

   --------------------
   -- Receive_Buffer --
   --------------------

   procedure Receive_Buffer
     (Params            : in out Rose.Invocation.Invocation_Record;
      Max_Storage_Units : System.Storage_Elements.Storage_Count)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Send_Buffer) := True;
      Params.Control.Flags (Rose.Invocation.Writable_Buffer) := True;
      Params.Buffer_Address := Local_Buffer'Address;
      Params.Buffer_Length :=
        System.Storage_Elements.Storage_Count'Min
          (Local_Buffer'Last, Max_Storage_Units);
      Local_Buffer := (others => 0);
   end Receive_Buffer;

   ------------------
   -- Receive_Caps --
   ------------------

   procedure Receive_Caps
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Recv_Caps) := Count > 0;
      if Count > 0 then
         Params.Control.Last_Recv_Cap :=
           Rose.Invocation.Capability_Index (Count - 1);
      end if;
   end Receive_Caps;

   -------------------
   -- Receive_Words --
   -------------------

   procedure Receive_Words
     (Params : in out Rose.Invocation.Invocation_Record;
      Count  : Natural)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Recv_Words) := Count > 0;
      if Count > 0 then
         Params.Control.Last_Recv_Word :=
           Rose.Invocation.Parameter_Word_Index (Count - 1);
      end if;
   end Receive_Words;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer
     (Params   : in out Rose.Invocation.Invocation_Record;
      Bytes    : System.Storage_Elements.Storage_Count;
      Buffer   : System.Address;
      Writable : Boolean)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Send_Buffer) := True;
      Params.Control.Flags (Rose.Invocation.Writable_Buffer) := Writable;
      Params.Buffer_Address := Buffer;
      Params.Buffer_Length := Bytes;
   end Send_Buffer;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (Params : in out Rose.Invocation.Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   is
      use type Rose.Invocation.Capability_Index;
   begin
      if not Params.Control.Flags (Rose.Invocation.Send_Caps) then
         Params.Control.Flags (Rose.Invocation.Send_Caps) := True;
         Params.Control.Last_Sent_Cap := 0;
      else
         Params.Control.Last_Sent_Cap :=
           Params.Control.Last_Sent_Cap + 1;
      end if;
      Params.Caps (Params.Control.Last_Sent_Cap) := Cap;
   end Send_Cap;

   -------------------
   -- Send_Endpoint --
   -------------------

   procedure Send_Endpoint
     (Params   : in out Rose.Invocation.Invocation_Record;
      Endpoint : Rose.Objects.Endpoint_Id)
   is
   begin
      Rose.Invocation.Send_Object_Id
        (Params, Rose.Objects.Object_Id (Endpoint));
   end Send_Endpoint;

   ----------------
   -- Send_Error --
   ----------------

   procedure Send_Error
     (Params : in out Rose.Invocation.Invocation_Record;
      Error  : Rose.Invocation.Invocation_Error)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Error) := True;
      Params.Error := Error;
   end Send_Error;

   ----------------------
   -- Send_Native_Word --
   ----------------------

   procedure Send_Native_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word)
   is
      use type Rose.Invocation.Parameter_Word_Index;
   begin
      if not Params.Control.Flags (Rose.Invocation.Send_Words) then
         Params.Control.Flags (Rose.Invocation.Send_Words) := True;
         Params.Control.Last_Sent_Word := 0;
      else
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
      end if;

      Params.Data (Params.Control.Last_Sent_Word) := Value;
   end Send_Native_Word;

   --------------------
   -- Send_Object_Id --
   --------------------

   procedure Send_Object_Id
     (Params : in out Rose.Invocation.Invocation_Record;
      Oid    : Rose.Objects.Object_Id)
   is
   begin
      Rose.Invocation.Send_Object_Id (Params, Oid);
   end Send_Object_Id;

   ------------------------
   -- Send_Storage_Array --
   ------------------------

   procedure Send_Storage_Array
     (Params   : in out Rose.Invocation.Invocation_Record;
      Storage  : System.Storage_Elements.Storage_Array;
      Writable : Boolean)
   is
   begin
      Local_Buffer := (others => 0);
      Local_Buffer (1 .. Storage'Length) := Storage;
      Send_Buffer (Params, Storage'Length, Local_Buffer'Address, Writable);
   end Send_Storage_Array;

   ---------------
   -- Send_Text --
   ---------------

   procedure Send_Text
     (Params : in out Rose.Invocation.Invocation_Record;
      Text   : String)
   is
   begin
      Params.Control.Flags (Rose.Invocation.Send_Buffer) := True;
      Params.Buffer_Address := Local_Buffer'Address;
      Params.Buffer_Length :=
        System.Storage_Elements.Storage_Count (Text'Length);

      Local_Buffer := (others => 0);

      declare
         use System.Storage_Elements;
         Last : Storage_Count := Local_Buffer'First - 1;
      begin
         for Ch of Text loop
            Last := Last + 1;
            Local_Buffer (Last) := Character'Pos (Ch);
         end loop;
      end;
   end Send_Text;

   ---------------
   -- Send_Word --
   ---------------

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Natural)
   is
   begin
      Send_Native_Word (Params, Rose.Words.Word (Value));
   end Send_Word;

   ---------------
   -- Send_Word --
   ---------------

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word_64)
   is
      use Rose.Words;
   begin
      if Default_32_Bit then
         Send_Native_Word (Params, Word_32 (Value mod 2 ** 32));
         Send_Native_Word (Params, Word_32 (Value / 2 ** 32));
      else
         Send_Native_Word (Params, Rose.Words.Word (Value));
      end if;
   end Send_Word;

   ---------------
   -- Send_Word --
   ---------------

   procedure Send_Word
     (Params : in out Rose.Invocation.Invocation_Record;
      Value  : Rose.Words.Word_32)
   is
      use type Rose.Invocation.Parameter_Word_Index;
   begin
      if Default_32_Bit then
         if not Params.Control.Flags (Rose.Invocation.Send_Words) then
            Params.Control.Flags (Rose.Invocation.Send_Words) := True;
            Params.Control.Last_Sent_Word := 0;
         else
            Params.Control.Last_Sent_Word :=
              Params.Control.Last_Sent_Word + 1;
         end if;

         Params.Data (Params.Control.Last_Sent_Word) := Value;
      else
         Send_Word (Params, Rose.Words.Word_64 (Value));
      end if;
   end Send_Word;

end Rose.System_Calls;
