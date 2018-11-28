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

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Params : Invocation_Record;
      Index  : Parameter_Word_Index)
      return Rose.Objects.Object_Id
   is
      use type Rose.Objects.Object_Id;
   begin
      return Oid : Rose.Objects.Object_Id :=
        Rose.Objects.Object_Id (Params.Data (Index))
      do
         if not Object_Fits_In_Word then
            Oid := Oid +
              Rose.Objects.Object_Id (Params.Data (Index + 1)) * 2 ** 32;
         end if;
      end return;
   end Get_Object_Id;

   --------------
   -- Send_Cap --
   --------------

   procedure Send_Cap
     (Params : in out Invocation_Record;
      Cap    : Rose.Capabilities.Capability)
   is
   begin
      if not Params.Control.Flags (Send_Caps) then
         Params.Control.Flags (Send_Caps) := True;
         Params.Control.Last_Sent_Cap := 0;
      else
         Params.Control.Last_Sent_Cap :=
           Params.Control.Last_Sent_Cap + 1;
      end if;

      Params.Caps (Params.Control.Last_Sent_Cap) := Cap;
   end Send_Cap;

   --------------------
   -- Send_Object_Id --
   --------------------

   procedure Send_Object_Id
     (Params : in out Invocation_Record;
      Oid    : Rose.Objects.Object_Id)
   is
   begin
      Send_Word (Params, Word (Oid));
      if not Object_Fits_In_Word then
         Send_Word (Params, Word (Word_64 (Oid) / 2 ** 32));
      end if;
   end Send_Object_Id;

   ---------------
   -- Send_Word --
   ---------------

   procedure Send_Word
     (Params : in out Invocation_Record;
      Value  : Rose.Words.Word)
   is
   begin
      if not Params.Control.Flags (Send_Words) then
         Params.Control.Flags (Send_Words) := True;
         Params.Control.Last_Sent_Word := 0;
      else
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
      end if;

      Params.Data (Params.Control.Last_Sent_Word) := Value;
   end Send_Word;

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
