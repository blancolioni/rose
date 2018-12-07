with System.Storage_Elements;

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

   -------------------
   -- Get_Interface --
   -------------------

   procedure Get_Interface
     (Cap            : Rose.Capabilities.Capability;
      Identifier     : Rose.Objects.Capability_Identifier;
      Interface_Caps : out Array_Of_Capabilities)
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Words       => True,
                               Block            => True,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Recv_Cap :=
        Capability_Index (Interface_Caps'Length - 1);

      Params.Data (0) := Rose.Words.Word (Identifier);

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
     (Cap          : Rose.Capabilities.Capability;
      Module_Index : Rose.Words.Word;
      Priority     : Rose.Words.Word;
      Launch_Caps  : Array_Of_Capabilities)
      return Rose.Objects.Object_Id
   is
      No_Words : Array_Of_Words (1 .. 0);
   begin
      return Launch_Boot_Module
        (Cap, Module_Index, Priority, Launch_Caps, No_Words);
   end Launch_Boot_Module;

   ------------------------
   -- Launch_Boot_Module --
   ------------------------

   function Launch_Boot_Module
     (Cap          : Rose.Capabilities.Capability;
      Module_Index : Rose.Words.Word;
      Priority     : Rose.Words.Word;
      Launch_Caps  : Array_Of_Capabilities;
      Launch_Words : Array_Of_Words)
      return Rose.Objects.Object_Id
   is
      use Rose.Invocation;
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Params.Cap := Cap;
      Params.Control.Flags := (Send             => True,
                               Send_Words       => True,
                               Recv_Words       => True,
                               Send_Caps        => Launch_Caps'Length > 0,
                               Block            => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 1;
      Params.Control.Last_Sent_Cap := 0;
      Params.Data (0) := Module_Index;
      Params.Data (1) := Priority;

      for W of Launch_Words loop
         Params.Control.Last_Sent_Word :=
           Params.Control.Last_Sent_Word + 1;
         Params.Data (Params.Control.Last_Sent_Word) := W;
      end loop;

      for Launch_Cap of Launch_Caps loop
         Params.Caps (Params.Control.Last_Sent_Cap) := Launch_Cap;
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

   end Launch_Boot_Module;

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

      Params.Buffer_Address := Local_Buffer'Address;
      Params.Buffer_Length :=
        System.Storage_Elements.Storage_Count (Message'Length);

      Rose.System_Calls.Invoke_Capability (Params);
   end Send_String;

end Init.Calls;
