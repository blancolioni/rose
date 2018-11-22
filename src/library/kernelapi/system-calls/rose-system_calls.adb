package body Rose.System_Calls is

   procedure Invoke_Capability_Wrapper
     (Item : aliased in out Rose.Invocation.Invocation_Record);

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

end Rose.System_Calls;
