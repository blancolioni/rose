with System.Storage_Elements;

with Rose.Addresses;
with Rose.Invocation;

package body Rose.System_Calls.Client is

   Local_Buffer : System.Storage_Elements.Storage_Array (1 .. 4096)
     with Alignment => 4096;

   --------------------
   -- Get_Capability --
   --------------------

   function Get_Capability
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
      return Rose.Capabilities.Capability
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Control.Flags := (Send             => True,
                               Block            => True,
                               Send_Words       => Data'Length > 0,
                               Recv_Caps        => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Cap;

      if Data'Length > 0 then
         for I in Data'Range loop
            Params.Data (Parameter_Word_Index (I - Data'First)) := Data (I);
         end loop;
         Params.Control.Last_Sent_Word :=
           Parameter_Word_Index (Data'Length - 1);
      end if;

      Params.Control.Last_Recv_Word := 0;
      Params.Control.Last_Recv_Cap := 0;

      Invoke_Capability (Params);

      if Params.Control.Flags (Send_Caps) then
         return Params.Caps (0);
      else
         return 0;
      end if;
   end Get_Capability;


   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
      return Rose.Words.Word
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Control.Flags := (Send             => True,
                               Block            => True,
                               Send_Words       => Data'Length > 0,
                               Recv_Words       => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Cap;

      if Data'Length > 0 then
         for I in Data'Range loop
            Params.Data (Parameter_Word_Index (I - Data'First)) := Data (I);
         end loop;
         Params.Control.Last_Sent_Word :=
           Parameter_Word_Index (Data'Length - 1);
      end if;

      Params.Control.Last_Recv_Word := 0;

      Invoke_Capability (Params);

      if Params.Control.Flags (Send_Words) then
         return Params.Data (0);
      else
         return 16#BAAD_F00D#;
      end if;
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Cap  : Rose.Capabilities.Capability;
      Data : Rose.Words.Word)
      return Rose.Words.Word
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Control.Flags := (Send => True, Block => True,
                               Send_Words => True, Recv_Words => True,
                               Create_Reply_Cap => True,
                               others           => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Cap;
      Params.Data (0) := Data;

      Invoke_Capability (Params);

      if Params.Control.Flags (Send_Words) then
         return Params.Data (0);
      else
         return 16#BAAD_F00D#;
      end if;
   end Get_Value;

   ----------
   -- Send --
   ----------

   procedure Send
     (Cap  : Rose.Capabilities.Capability;
      Data : Sent_Words_Array := No_Sent_Words)
   is
      Sent_Caps    : Sent_Caps_Array (1 .. 0);
      Result_Words : Sent_Words_Array (1 .. 0);
      Result_Caps  : Sent_Caps_Array (1 .. 0);
   begin
      Invoke_Blocking_Send (Cap, Data, Sent_Caps, Result_Words, Result_Caps);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Cap  : Rose.Capabilities.Capability;
      Data : Rose.Words.Word)
   is
   begin
      Send (Cap, (1 => Data));
   end Send;

   ---------------
   -- Send_Caps --
   ---------------

   procedure Send_Caps
     (Cap  : Rose.Capabilities.Capability;
      Caps : Sent_Caps_Array := No_Sent_Caps)
   is
      Sent_Words   : Sent_Words_Array (1 .. 0);
      Result_Words : Sent_Words_Array (1 .. 0);
      Result_Caps  : Sent_Caps_Array (1 .. 0);
   begin
      Invoke_Blocking_Send (Cap, Sent_Words, Caps, Result_Words, Result_Caps);
   end Send_Caps;

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

end Rose.System_Calls.Client;
