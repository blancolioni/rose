with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls;

package body Init.Calls is

   Local_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   Console_Cap : Rose.Capabilities.Capability := 0;

   procedure Append_Each
     (Append : Rose.Capabilities.Capability;
      Caps   : Array_Of_Capabilities);
   --  Append each of Caps to a cap set, through the set's Append cap,
   --  one cap to a call (#234).

   -----------------
   -- Append_Each --
   -----------------

   procedure Append_Each
     (Append : Rose.Capabilities.Capability;
      Caps   : Array_Of_Capabilities)
   is
   begin
      for Cap of Caps loop
         declare
            Params : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Params, Append);
            Rose.System_Calls.Send_Cap (Params, Cap);
            Rose.System_Calls.Invoke_Capability (Params);
         end;
      end loop;
   end Append_Each;

   ----------
   -- Call --
   ----------

   function Call
     (Cap    : Rose.Capabilities.Capability;
      Data   : Array_Of_Words)
      return Rose.Capabilities.Capability
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Words       => Data'Length > 0,
                               Block            => True,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      for Value of Data loop
         Params.Data (Params.Control.Last_Sent_Word) := Value;
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
      end loop;
      Params.Control.Last_Sent_Word :=
        Params.Control.Last_Sent_Word - 1;
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Error)
        or else not Params.Control.Flags (Send_Caps)
      then
         return 0;
      else
         return Params.Caps (0);
      end if;
   end Call;

   ----------
   -- Call --
   ----------

   procedure Call
     (Cap         : Rose.Capabilities.Capability;
      Data        : Array_Of_Words;
      Result_Caps : out Array_Of_Capabilities)
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Cap);
      for W of Data loop
         Rose.System_Calls.Send_Word (Params, W);
      end loop;
      Params.Control.Flags (Rose.Invocation.Recv_Caps) := True;
      Params.Control.Last_Recv_Cap :=
        Rose.Invocation.Capability_Index (Result_Caps'Length - 1);
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         for Cap of Result_Caps loop
            Cap := 0;
         end loop;
      else
         for I in 0 .. Params.Control.Last_Sent_Cap loop
            Result_Caps (Natural (I) + Result_Caps'First) :=
              Params.Caps (I);
         end loop;
      end if;
   end Call;

   ----------
   -- Call --
   ----------

   function Call
     (Cap      : Rose.Capabilities.Capability;
      Sent_Cap : Rose.Capabilities.Capability;
      Data     : Array_Of_Words)
      return Rose.Capabilities.Capability
   is
      use Rose.System_Calls;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Initialize_Send (Params, Cap);
      for W of Data loop
         Rose.System_Calls.Send_Word (Params, W);
      end loop;
      Params.Control.Flags (Rose.Invocation.Send_Caps) := True;
      Params.Control.Flags (Rose.Invocation.Recv_Caps) := True;
      Params.Control.Last_Recv_Cap := 0;
      Params.Control.Last_Sent_Cap := 0;
      Params.Caps (Params.Caps'First) := Sent_Cap;
      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Rose.Invocation.Error) then
         return 0;
      else
         return Params.Caps (Params.Caps'First);
      end if;
   end Call;

   -------------------------
   -- Create_Cap_Set_With --
   -------------------------

   function Create_Cap_Set_With
     (Create_Cap_Set  : Rose.Capabilities.Capability;
      Caps            : Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
      No_Arguments : Array_Of_Words (1 .. 0);
      Cap_Set      : constant Rose.Capabilities.Capability :=
                       Call (Create_Cap_Set, No_Arguments);
      Append       : Rose.Capabilities.Capability;
      Get_Cap      : Rose.Capabilities.Capability;
      Length       : Rose.Capabilities.Capability;
      Cap_Params   : aliased Rose.Invocation.Invocation_Record;
   begin

      --  The set is a Releasable_Cap_Set (#825), whose interface gives
      --  Release as well, after Cap_Set's three.

      Rose.System_Calls.Initialize_Send (Cap_Params, Cap_Set);
      Rose.System_Calls.Receive_Caps (Cap_Params, 4);
      Rose.System_Calls.Invoke_Capability (Cap_Params);
      Append := Cap_Params.Caps (0);
      Get_Cap := Cap_Params.Caps (1);
      Length  := Cap_Params.Caps (2);

      Append_Each (Append, Caps);
      Rose.System_Calls.Delete_Cap (Append);
      Rose.System_Calls.Delete_Cap (Get_Cap);
      Rose.System_Calls.Delete_Cap (Length);

      return Cap_Set;

   end Create_Cap_Set_With;

   -----------------
   -- Find_In_Map --
   -----------------

   function Find_In_Map
     (Find_Cap : Rose.Capabilities.Capability;
      Key      : String)
      return Rose.Capabilities.Capability
   is
      use System.Storage_Elements;
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Find_Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Buffer      => True,
                               Block            => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Local_Buffer := (others => 0);

      declare
         To : Storage_Offset := Local_Buffer'First;
      begin
         for Ch of Key loop
            Local_Buffer (To) := Character'Pos (Ch);
            To := To + 1;
         end loop;
      end;

      Params.Buffer_Address := Local_Buffer'Address;
      Params.Buffer_Length :=
        System.Storage_Elements.Storage_Count (Key'Length);

      Rose.System_Calls.Invoke_Capability (Params);
      if Params.Control.Flags (Send_Caps) then
         return Params.Caps (0);
      else
         return Rose.Capabilities.Null_Capability;
      end if;
   end Find_In_Map;

   -------------------
   -- Get_Interface --
   -------------------

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Block            => True,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Recv_Cap :=
        Capability_Index (Interface_Caps'Length - 1);

      Rose.System_Calls.Invoke_Capability (Params);

      Interface_Caps := (others => 0);

      if not (Params.Control.Flags (Error)
              or else not Params.Control.Flags (Send_Caps))
      then
         declare
            Last : Natural := Interface_Caps'First - 1;
         begin
            for Index in 0 .. Params.Control.Last_Sent_Cap loop
               Last := Last + 1;
               exit when Last > Interface_Caps'Length;
               Interface_Caps (Last) := Params.Caps (Index);
            end loop;
         end;
      end if;
   end Get_Interface;

   ------------------------
   -- Launch_Boot_Module --
   ------------------------

   function Launch_Boot_Module
     (Cap             : Rose.Capabilities.Capability;
      Module_Index    : Rose.Words.Word;
      Oid             : Rose.Objects.Object_Id;
      Priority        : Rose.Words.Word;
      Create_Endpoint : Rose.Capabilities.Capability;
      Create_Cap_Set  : Rose.Capabilities.Capability;
      Launch_Caps     : Array_Of_Capabilities)
      return Rose.Capabilities.Capability
   is
      use Rose.Capabilities;
      use Rose.Invocation;
      Params   : aliased Rose.Invocation.Invocation_Record;
      Set_Caps : Array_Of_Capabilities (1 .. 4) := (others => 0);
      --  Init's caps to the module's set, and to its Append, Get_Cap and
      --  Length, which it deletes once the module has its copy (#826).
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Words       => True,
                               Recv_Words       => True,
                               Recv_Caps        => True,
                               Send_Caps        => False,
                               Block            => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 2;
      Params.Data (0) := Module_Index;
      Params.Data (1) := Priority;
      Params.Data (2) := Rose.Words.Word (Oid);

      Rose.System_Calls.Send_Cap (Params, Create_Endpoint);

      --  Without a cap set, the caps go in the launch itself, which
      --  carries at most four with Create_Endpoint (#234); scripts/make-init
      --  checks boot.yaml for that.

      if Create_Cap_Set = Null_Capability then
         for Launch_Cap of Launch_Caps loop
            Rose.System_Calls.Send_Cap (Params, Launch_Cap);
         end loop;
      else
         declare
            No_Arguments : Array_Of_Words (1 .. 0);
            Cap_Set : constant Rose.Capabilities.Capability :=
                             Call (Create_Cap_Set, No_Arguments);
            Append       : Rose.Capabilities.Capability;
            Get_Cap      : Rose.Capabilities.Capability;
            Cap_Params   : aliased Rose.Invocation.Invocation_Record;
         begin
            Rose.System_Calls.Initialize_Send (Cap_Params, Cap_Set);
            Rose.System_Calls.Receive_Caps (Cap_Params, 3);
            Rose.System_Calls.Invoke_Capability (Cap_Params);
            Append := Cap_Params.Caps (0);
            Get_Cap := Cap_Params.Caps (1);
            Set_Caps := (Cap_Set, Append, Get_Cap, Cap_Params.Caps (2));
            Append_Each (Append, Launch_Caps);
            Rose.System_Calls.Send_Cap (Params, Get_Cap);

         end;
      end if;

      Rose.System_Calls.Invoke_Capability (Params);

      for Set_Cap of Set_Caps loop
         Rose.System_Calls.Delete_Cap (Set_Cap);
      end loop;

      --  The kernel's reply has a copy cap for the new process (#115).

      if Params.Control.Flags (Error)
        or else not Params.Control.Flags (Send_Caps)
      then
         return Null_Capability;
      else
         return Params.Caps (0);
      end if;

   end Launch_Boot_Module;

   --------------------
   -- Open_Interface --
   --------------------

   procedure Open_Interface
     (Cap            : Rose.Capabilities.Capability;
      Interface_Caps : out Array_Of_Capabilities)
   is
      Caps : Rose.Capabilities.Capability_Array (Interface_Caps'Range);
   begin
      Rose.System_Calls.Get_Interface_Caps (Cap, Caps);
      Interface_Caps := Array_Of_Capabilities (Caps);
   end Open_Interface;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Cap : Rose.Capabilities.Capability)
   is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Receive (Params, Cap);
      Rose.System_Calls.Receive_Words
        (Params, Natural (Rose.Invocation.Parameter_Word_Index'Last) + 1);
      Rose.System_Calls.Receive_Caps
        (Params, Natural (Rose.Invocation.Capability_Index'Last) + 1);
      Rose.System_Calls.Invoke_Capability (Params);
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Cap    : Rose.Capabilities.Capability;
      Data   : Array_Of_Words)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send       => True,
                               Send_Words => Data'Length > 0,
                               Block      => False,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      for Value of Data loop
         Params.Data (Params.Control.Last_Sent_Word) := Value;
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
      end loop;
      Params.Control.Last_Sent_Word :=
        Params.Control.Last_Sent_Word - 1;
      Rose.System_Calls.Invoke_Capability (Params);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Cap    : Rose.Capabilities.Capability)
   is
      Data : Array_Of_Words (1 .. 0);
   begin
      Send (Cap, Data);
   end Send;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (Cap      : Rose.Capabilities.Capability;
      Sent_Cap : Rose.Capabilities.Capability;
      Data     : Array_Of_Words)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send       => True,
                               Send_Words => Data'Length > 0,
                               Send_Caps  => True,
                               Block      => False,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      for Value of Data loop
         Params.Data (Params.Control.Last_Sent_Word) := Value;
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
      end loop;
      Params.Control.Last_Sent_Word :=
        Params.Control.Last_Sent_Word - 1;
      Params.Control.Last_Sent_Cap := 0;
      Params.Caps (Params.Caps'First) := Sent_Cap;
      Rose.System_Calls.Invoke_Capability (Params);
   end Send_Cap;

   -----------------
   -- Send_String --
   -----------------

   procedure Send_String
     (Message : String)
   is
      use type Rose.Capabilities.Capability;
   begin
      if Console_Cap /= Rose.Capabilities.Null_Capability then
         Send_String (Console_Cap, Message);
      end if;
   end Send_String;

   -----------------
   -- Send_String --
   -----------------

   procedure Send_String
     (Cap     : Rose.Capabilities.Capability;
      Message : String)
   is
      use System.Storage_Elements;
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Console_Cap := Cap;
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Buffer      => True,
                               Block            => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Local_Buffer := (others => 0);
      for I in Message'Range loop
         Local_Buffer (Storage_Offset (I - Message'First + 1)) :=
           Character'Pos (Message (I));
      end loop;

      Params.Buffer_Address := Local_Buffer'Address;
      Params.Buffer_Length :=
        System.Storage_Elements.Storage_Count (Message'Length);

      Rose.System_Calls.Invoke_Capability (Params);
   end Send_String;

   -----------------
   -- Set_Console --
   -----------------

   procedure Set_Console
     (Cap : Rose.Capabilities.Capability)
   is
   begin
      Console_Cap := Cap;
   end Set_Console;

end Init.Calls;
