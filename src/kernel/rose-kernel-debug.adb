with Rose.Words;
with Rose.Boot.Console;
with Rose.Kernel.Processes.Debug;

package body Rose.Kernel.Debug is

   --------------
   -- Put_Call --
   --------------

   procedure Put_Call
     (Name   : String;
      Layout : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Record)
   is
      use Rose.Boot.Console;
      use Rose.Invocation;
      use Rose.Words;

      procedure Put_Flag
        (Name  : String;
         Flag  : Invocation_Flag);

      procedure Put_Value
        (Name       : String;
         Enabled_By : Invocation_Flag;
         Value      : Natural);

      --------------
      -- Put_Flag --
      --------------

      procedure Put_Flag (Name  : String;
                          Flag  : Invocation_Flag)
      is
      begin
         if Params.Control.Flags (Flag) then
            Put (" ");
            Put (Name);
         end if;
      end Put_Flag;

      ---------------
      -- Put_Value --
      ---------------

      procedure Put_Value
        (Name       : String;
         Enabled_By : Invocation_Flag;
         Value      : Natural)
      is
      begin
         if Params.Control.Flags (Enabled_By) then
            Put (" ");
            Put (Name);
            Put ("=");
            Put (Value);
         end if;
      end Put_Value;

   begin
      Rose.Kernel.Processes.Debug.Put
        (Rose.Kernel.Processes.Current_Process_Id);
      Put (": ");
      Put (Name & ":");
      Put (Natural (Params.Cap));
      Put (" ");
      Put_Cap_Type (Layout.Header.Cap_Type);
      Put (": ep=");
      Put (Rose.Words.Word_16 (Layout.Header.Endpoint));
      Put (" id=");
      Put (Natural (Layout.Header.Identifier));
      Put ("/");
      Put (Natural (Params.Identifier));
      Put (" payload=");
      Put (Rose.Words.Word_32 (Layout.Payload));
      Put_Flag ("E", Error);
      Put_Flag ("S", Send);
      Put_Flag ("R", Receive);
      Put_Flag ("Y", Reply);
      Put_Flag ("B", Block);
      Put_Flag ("D", Send_Buffer);
      Put_Flag ("SW", Send_Words);
      Put_Flag ("RW", Recv_Words);
      Put_Flag ("SC", Send_Caps);
      Put_Flag ("RC", Recv_Caps);
      Put_Flag ("SB", Send_Buffer);
      Put_Flag ("WB", Writable_Buffer);
      Put_Flag ("MR", Create_Reply_Cap);

      Put_Value ("lsw", Send_Words, Natural (Params.Control.Last_Sent_Word));
      Put_Value ("lrw", Recv_Words, Natural (Params.Control.Last_Recv_Word));
      Put_Value ("lsc", Send_Caps, Natural (Params.Control.Last_Sent_Cap));
      Put_Value ("lrc", Recv_Caps, Natural (Params.Control.Last_Recv_Cap));

      Put (" ep=");
      Put (Word_32 (Word_64 (Params.Endpoint) / 2 ** 32));
      Put ("_");
      Put (Word_32 (Params.Endpoint));

      if Params.Control.Flags (Send_Words) then
         Put (" [");
         for I in 0 .. Params.Control.Last_Sent_Word loop
            if I > 0 then
               Put (" ");
            end if;
            Put (Params.Data (I));
         end loop;
         Put ("]");
      end if;

      if Params.Control.Flags (Send_Caps) then
         Put (" <");
         for I in 0 .. Params.Control.Last_Sent_Cap loop
            if I > 0 then
               Put (" ");
            end if;
            Put (Natural (Params.Caps (I)));
         end loop;
         Put (">");
      end if;

      if Params.Control.Flags (Send_Buffer) then
         declare
            Addr : constant Word :=
                     Word (Rose.Addresses.To_Virtual_Address
                           (Params.Buffer_Address));
         begin
            Rose.Boot.Console.Put (" buf=(");
            Rose.Boot.Console.Put (Addr);
            Rose.Boot.Console.Put (",");
            Rose.Boot.Console.Put
              (Addr + Word (Params.Buffer_Length));
            Rose.Boot.Console.Put (")");
         end;
      end if;

      if Params.Control.Flags (Recv_Words) then
         Put (" R ");
         Put (Natural (Params.Control.Last_Recv_Word));
      end if;

      New_Line;
   end Put_Call;

   ------------------
   -- Put_Cap_Type --
   ------------------

   procedure Put_Cap_Type
     (Cap_Type : Rose.Capabilities.Layout.Capability_Type)
   is
      use all type Rose.Capabilities.Layout.Capability_Type;
   begin
      Rose.Boot.Console.Put
        (case Cap_Type is
            when Null_Cap            => "null",
            when Page_Object_Cap     => "page-object",
            when Meta_Cap            => "meta",
            when Process_Cap         => "process",
            when Endpoint_Cap        => "endpoint",
            when Receive_Cap         => "receive",
            when Interrupt_Cap       => "interrupt",
            when Kernel_Cap          => "kernel",
            when Boot_Cap            => "boot",
            when Copy_Cap            => "copy",
            when Physical_Memory_Cap => "physical-memory",
            when Create_Cap          => "create",
            when Page_Table_Cap      => "page-table",
            when Reply_Cap           => "reply",
            when Arch_Cap            => "arch",
            when Other_Cap           => "other");
   end Put_Cap_Type;

end Rose.Kernel.Debug;
