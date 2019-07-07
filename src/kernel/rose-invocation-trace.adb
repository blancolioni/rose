with Rose.Addresses;
with Rose.Boot.Console;

package body Rose.Invocation.Trace is

   ---------
   -- Put --
   ---------

   procedure Put
     (Invocation : Invocation_Record;
      Show_Args  : Boolean)
   is
   begin
      Rose.Boot.Console.Put ("cap=");
      Rose.Boot.Console.Put (Rose.Words.Word_8 (Invocation.Cap));
      if Invocation.Control.Flags (Rose.Invocation.Send) then
         Rose.Boot.Console.Put (" S");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Receive) then
         Rose.Boot.Console.Put (" R");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Reply) then
         Rose.Boot.Console.Put (" Y");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Block) then
         Rose.Boot.Console.Put (" B");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Send_Caps) then
         Rose.Boot.Console.Put (" SC");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Recv_Caps) then
         Rose.Boot.Console.Put (" RC");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Send_Words) then
         Rose.Boot.Console.Put (" SW");
      end if;
      if Invocation.Control.Flags (Rose.Invocation.Recv_Words) then
         Rose.Boot.Console.Put (" RW");
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Send_Buffer) then
         if Invocation.Control.Flags (Rose.Invocation.Writable_Buffer) then
            Rose.Boot.Console.Put (" WB");
         else
            Rose.Boot.Console.Put (" RB");
         end if;
      end if;

      if Invocation.Control.Flags (Rose.Invocation.No_Trace) then
         Rose.Boot.Console.Put (" NT");
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Send_Caps) then
         Rose.Boot.Console.Put (" lsc=");
         Rose.Boot.Console.Put
           (Rose.Words.Word_8 (Invocation.Control.Last_Sent_Cap));
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Recv_Caps) then
         Rose.Boot.Console.Put (" lrc=");
         Rose.Boot.Console.Put
           (Rose.Words.Word_8 (Invocation.Control.Last_Recv_Cap));
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Send_Words) then
         Rose.Boot.Console.Put (" lsw=");
         Rose.Boot.Console.Put
           (Rose.Words.Word_8 (Invocation.Control.Last_Sent_Word));
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Recv_Words) then
         Rose.Boot.Console.Put (" lrw=");
         Rose.Boot.Console.Put
           (Rose.Words.Word_8 (Invocation.Control.Last_Recv_Word));
      end if;

      if Rose.Objects."/=" (Invocation.Endpoint, 0) then
         Rose.Boot.Console.Put (" ep=");
         Rose.Boot.Console.Put (Rose.Words.Word_32 (Invocation.Endpoint));
      end if;

      if Invocation.Control.Flags (Rose.Invocation.Send_Buffer) then
         declare
            Addr : constant Word :=
                     Word (Rose.Addresses.To_Virtual_Address
                           (Invocation.Buffer_Address));
         begin
            Rose.Boot.Console.Put (" buf=(");
            Rose.Boot.Console.Put (Addr);
            Rose.Boot.Console.Put (",");
            Rose.Boot.Console.Put
              (Addr + Word (Invocation.Buffer_Length));
            Rose.Boot.Console.Put (")");
         end;
      end if;

      Rose.Boot.Console.New_Line;

      if Show_Args then
         if Invocation.Control.Flags (Rose.Invocation.Send_Caps) then
            for I in 0 .. Invocation.Control.Last_Sent_Cap loop
               Rose.Boot.Console.Put ("cap ");
               Rose.Boot.Console.Put (Rose.Words.Word_8 (I));
               Rose.Boot.Console.Put (": ");
               Rose.Boot.Console.Put (Rose.Words.Word_8 (Invocation.Caps (I)));
               Rose.Boot.Console.New_Line;
            end loop;
         end if;
         if Invocation.Control.Flags (Rose.Invocation.Send_Words) then
            for I in 0 .. Invocation.Control.Last_Sent_Word loop
               Rose.Boot.Console.Put ("word ");
               Rose.Boot.Console.Put (Rose.Words.Word_8 (I));
               Rose.Boot.Console.Put (": ");
               Rose.Boot.Console.Put (Invocation.Data (I));
               Rose.Boot.Console.New_Line;
            end loop;
         end if;
      end if;

   end Put;

end Rose.Invocation.Trace;
