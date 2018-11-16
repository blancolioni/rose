package body Rose.Invocation is

   ------------------------
   -- For_Each_Sent_Word --
   ------------------------

   procedure For_Each_Sent_Word
     (Params  : Invocation_Record;
      Process : not null access
        procedure (Value : Rose.Words.Word))
   is
   begin
      if Params.Control.Flags (Send_Words) then
         for I in 0 .. Params.Control.Last_Sent_Word loop
            Process (Params.Data (I));
         end loop;
      end if;
   end For_Each_Sent_Word;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Params  : in out Invocation_Record;
      Error   : Invocation_Error;
      X, Y, Z : Rose.Words.Word := 0)
   is
   begin
      Params.Control.Flags :=
        (Rose.Invocation.Error => True, Send_Words => True, others => False);
      Params.Data (1) := Invocation_Error'Pos (Error);
      Params.Data (2) := X;
      Params.Data (3) := Y;
      Params.Data (4) := Z;
      Params.Control.Last_Sent_Word := 3;
   end Set_Error;

end Rose.Invocation;
