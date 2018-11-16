with System.Storage_Elements;

with Rose.Addresses;
with Rose.Invocation;
with Rose.System_Calls;

package body Init.Calls is

   Local_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

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
      Params.Control.Flags := (Send       => True,
                               Send_Words => Data'Length > 0,
                               Block      => True,
                               Recv_Caps  => True,
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

   function Call
     (Cap       : Rose.Capabilities.Capability;
      Data      : Rose.Words.Word;
      Sent_Caps : Array_Of_Capabilities)
      return Rose.Objects.Object_Id
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send       => True,
                               Send_Words => True,
                               Recv_Words => True,
                               Send_Caps  => Sent_Caps'Length > 0,
                               Block      => True,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Sent_Cap := 0;
      Params.Data (0) := Data;
      for Sent_Cap of Sent_Caps loop
         Params.Caps (Params.Control.Last_Sent_Cap) := Sent_Cap;
         Params.Control.Last_Sent_Cap :=
           Params.Control.Last_Sent_Cap + 1;
      end loop;
      Params.Control.Last_Sent_Cap :=
        Params.Control.Last_Sent_Cap - 1;

      Rose.System_Calls.Invoke_Capability (Params);

      declare
         use Rose.Objects;
         Result : Object_Id := 0;
      begin
         if Params.Control.Flags (Send_Words) then
            Result := Object_Id (Params.Data (0));
            if Params.Control.Last_Sent_Word > 0 then
               Result := Result
                 + Object_Id (Params.Data (1)) * 2 ** 32;
            end if;
         end if;

         return Result;
      end;

   end Call;

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

      Params.Buffer_Address :=
        Rose.Addresses.To_Virtual_Address (Local_Buffer'Address);
      Params.Buffer_Length := Rose.Words.Word (Message'Length);

      Rose.System_Calls.Invoke_Capability (Params);
   end Send_String;

end Init.Calls;
