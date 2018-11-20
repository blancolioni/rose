with Rose.Words;
with Rose.Boot.Console;

package body Rose.Kernel.Debug is

   --------------
   -- Put_Call --
   --------------

   procedure Put_Call
     (Name   : String;
      Pid    : Rose.Objects.Process_Id;
      Layout : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Record)
   is
      use Rose.Boot.Console;
      use Rose.Invocation;
      use Rose.Words;

      procedure Put_Flag (Name  : String;
                          Flag  : Invocation_Flag);

      --------------
      -- Put_Flag --
      --------------

      procedure Put_Flag (Name  : String;
                          Flag  : Invocation_Flag)
      is
      begin
         if Params.Control.Flags (Flag) then
            Put (Name);
         end if;
      end Put_Flag;

   begin
      Put (Name & " ");
      Put (Word_8 (Pid));
      Put (": ");
      Put (Word_8 (Params.Cap));
      Put (" ");
      Put_Cap_Type (Layout.Header.Cap_Type);
      Put (": ");
      Put (Rose.Words.Word_16 (Layout.Header.Endpoint));
      Put (":");
      Put (Rose.Words.Word_32 (Layout.Payload));
      Put (" ");
      Put_Flag ("E", Error);
      Put_Flag ("S", Send);
      Put_Flag ("R", Receive);
      Put_Flag ("Y", Reply);
      Put_Flag ("B", Block);
      Put_Flag ("D", Send_Buffer);

      Put (" (");
      Put (Rose.Words.Word_8 (Params.Endpoint));
      Put (")");

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
            Put (Word_8 (Params.Caps (I)));
         end loop;
         Put (">");
      end if;

      if Params.Control.Flags (Send_Buffer) then
         Put (" (");
         Put (Word (Params.Buffer_Address));
         Put (" ");
         Put (Word (Params.Buffer_Address) + Params.Buffer_Length);
         Put (")");
      end if;

      if Params.Control.Flags (Recv_Words) then
         Put (" R ");
         Put (Word_8 (Params.Control.Last_Recv_Word));
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
            when Cap_Set             => "cap-set",
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
