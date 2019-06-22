with Rose.Invocation;
with Rose.System_Calls.Client;

package body Rose.Devices.Port_IO is

   ---------------
   -- Port_In_8 --
   ---------------

   function Port_In_8
     (Port   : Rose.Capabilities.Capability;
      Offset : Rose.Words.Word_8 := 0)
      return Rose.Words.Word_8
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send       => True,
                               Block      => True,
                               Send_Words => True,
                               Recv_Words => True,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Control.Last_Recv_Word := 0;
      Params.Cap := Port;
      Params.Data (0) := Rose.Words.Word (Offset);
      Rose.System_Calls.Invoke_Capability (Params);
      return Rose.Words.Word_8 (Params.Data (0));
   end Port_In_8;

   ----------------
   -- Port_In_16 --
   ----------------

   function Port_In_16
     (Port  : Rose.Capabilities.Capability)
      return Rose.Words.Word_16
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send       => True,
                               Block      => True,
                               Recv_Words => True,
                               No_Trace   => True,
                               others     => False);
      Params.Control.Last_Recv_Word := 0;
      Params.Cap := Port;
      Rose.System_Calls.Invoke_Capability (Params);
      return Rose.Words.Word_16 (Params.Data (0));
   end Port_In_16;

   ----------------
   -- Port_In_32 --
   ----------------

   function Port_In_32
     (Port  : Rose.Capabilities.Capability)
      return Rose.Words.Word_32
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send   => True,
                               Block  => True,
                               Recv_Words => True,
                               No_Trace   => True,
                               others     => False);
      Params.Control.Last_Recv_Word := 0;
      Params.Cap := Port;
      Rose.System_Calls.Invoke_Capability (Params);
      return Params.Data (0);
   end Port_In_32;

   ----------------
   -- Port_Out_8 --
   ----------------

   procedure Port_Out_8
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_8)
   is
   begin
      Rose.System_Calls.Client.Send (Port, Rose.Words.Word_32 (Value));
   end Port_Out_8;

   ----------------
   -- Port_Out_8 --
   ----------------

   procedure Port_Out_8
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_8)
   is
      use Rose.Words;
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send       => True,
                               Block      => True,
                               Send_Words => True,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Port;
      Params.Data (0) := Word (Offset) + 256 * Word (Value);
      Rose.System_Calls.Invoke_Capability (Params);
   end Port_Out_8;

   ----------------
   -- Port_Out_8 --
   ----------------

   procedure Port_Out_8
     (Port : Rose.Capabilities.Capability;
      Data : Word_8_Data_Array)
   is
      use Rose.Words;
      use Rose.Invocation;
      Params : aliased Invocation_Record;
      Index  : Parameter_Word_Index := Parameter_Word_Index'Last;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send       => True,
                               Block      => True,
                               Send_Words => True,
                               others     => False);
      Params.Cap := Port;

      for I in Data'Range loop
         Index := Index + 1;
         Params.Data (Index) :=
           Word (Data (I).Offset) + 256 * Word (Data (I).Data);
      end loop;

      Params.Control.Last_Sent_Word := Index;
      Rose.System_Calls.Invoke_Capability (Params);
   end Port_Out_8;

   -----------------
   -- Port_Out_16 --
   -----------------

   procedure Port_Out_16
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_16)
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send       => True,
                               Block      => True,
                               Send_Words => True,
                               No_Trace   => True,
                               others     => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Port;
      Params.Data (0) := Rose.Words.Word (Value);
      Rose.System_Calls.Invoke_Capability (Params);
   end Port_Out_16;

   -----------------
   -- Port_Out_16 --
   -----------------

   procedure Port_Out_16
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_16)
   is null;

   -----------------
   -- Port_Out_32 --
   -----------------

   procedure Port_Out_32
     (Port  : Rose.Capabilities.Capability;
      Value : Rose.Words.Word_32)
   is
      use Rose.Invocation;
      Params : aliased Invocation_Record;
   begin
      Params.Cap := Port;
      Params.Control.Flags := (Send   => True,
                               Block  => True,
                               Send_Words => True,
                               others => False);
      Params.Control.Last_Sent_Word := 0;
      Params.Cap := Port;
      Params.Data (0) := Value;
      Rose.System_Calls.Invoke_Capability (Params);
   end Port_Out_32;

   -----------------
   -- Port_Out_32 --
   -----------------

   procedure Port_Out_32
     (Port    : Rose.Capabilities.Capability;
      Offset  : Rose.Words.Word_8;
      Value   : Rose.Words.Word_32)
   is null;

end Rose.Devices.Port_IO;
