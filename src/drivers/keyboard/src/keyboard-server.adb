with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Interrupt_Handler;
with Rose.Interfaces.Event_Listener.Client;
with Rose.Interfaces.Event_Source;

with Rose.Devices.Port_IO;

with Rose.Server;

with Keyboard.Codes;

package body Keyboard.Server is

   Event_Source_Cap : Rose.Capabilities.Capability;
   Receive_Cap      : Rose.Capabilities.Capability;
   Add_Listener_Cap : Rose.Capabilities.Capability;
   Key_Cap          : Rose.Capabilities.Capability;

   Have_Listener    : Boolean := False;
   Listener_Open    : Boolean := False;
   Listener_Cap     : Rose.Capabilities.Capability;
   Listener         : Rose.Interfaces.Event_Listener.Client
     .Event_Listener_Client;
   --  The listener's interface cap, from Add_Listener. It is opened at
   --  the first key, not in Add_Listener: the listener is waiting for that
   --  call's reply, so it can't answer the invocation that opens it then.

   Keyboard_Pending   : constant := 16#01#;
--     Keyboard_Not_Ready : constant := 16#02#;

   procedure Register_IRQ;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Receive_Cap :=
        Rose.System_Calls.Server.Create_Receive_Cap
          (Create_Endpoint_Cap);
      Event_Source_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Event_Source.Event_Source_Interface);

      Add_Listener_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Event_Source.Add_Listener_Endpoint);
      Have_Listener := False;

      Register_IRQ;

      Rose.Server.Publish_Interface (Event_Source_Cap);

   end Create_Server;

   ------------------
   -- Register_IRQ --
   ------------------

   procedure Register_IRQ is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Key_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Interrupt_Handler.Handle_Interrupt_Endpoint);
      Rose.System_Calls.Initialize_Send (Params, Register_IRQ_Cap);
      Rose.System_Calls.Send_Cap (Params, Key_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Register_IRQ;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      use Rose.Words;

      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
      Send_Reply  : Boolean;

      function Key_Code_Available return Boolean;
      function Read_Key_Code return Rose.Words.Word_8;

      ------------------------
      -- Key_Code_Available --
      ------------------------

      function Key_Code_Available return Boolean is
         Status : constant Word_8 :=
                    Rose.Devices.Port_IO.Port_In_8 (Read_Status_Cap);
      begin
         return (Status and Keyboard_Pending) /= 0;
      end Key_Code_Available;

      -------------------
      -- Read_Key_Code --
      -------------------

      function Read_Key_Code return Rose.Words.Word_8 is
      begin
         return Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap);
      end Read_Key_Code;

   begin
      loop
         Send_Reply := True;
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               Rose.System_Calls.Send_Cap (Reply, Add_Listener_Cap);

            when Rose.Interfaces.Event_Source.Event_Source_Interface =>
               Rose.System_Calls.Send_Cap (Reply, Add_Listener_Cap);

            when Rose.Interfaces.Event_Source.Add_Listener_Endpoint =>

               if not Have_Listener then
                  Listener_Cap := Params.Caps (0);
                  Have_Listener := True;
               end if;

            when Rose.Interfaces.Interrupt_Handler.Handle_Interrupt_Endpoint =>

               Send_Reply := False;

               while Key_Code_Available loop
                  declare
                     use System.Storage_Elements;
                     Result : Storage_Array (1 .. 10);
                     Last   : Storage_Count;
                  begin

                     --  The keyboard only produces characters. The line
                     --  discipline, in vga-console, echoes them (#397,
                     --  #454).

                     Keyboard.Codes.Handle_Key
                       (Scan_Code => Read_Key_Code,
                        Result    => Result,
                        Last      => Last);

                     if Have_Listener and then not Listener_Open then
                        Rose.Interfaces.Event_Listener.Client.Open
                          (Listener, Listener_Cap);
                        Listener_Open := True;
                     end if;

                     if Have_Listener then
                        for Code of Result (1 .. Last) loop
                           Rose.Interfaces.Event_Listener.Client.On_Event
                             (Item => Listener,
                              Code => Rose.Words.Word (Code));
                        end loop;
                     end if;

                  end;
               end loop;

            when others =>
               Rose.System_Calls.Send_Error
                 (Reply, Rose.Invocation.Invalid_Endpoint);
               Rose.System_Calls.Send_Endpoint (Reply, Params.Endpoint);
         end case;

         if Send_Reply then
            Rose.System_Calls.Invoke_Capability (Reply);
         end if;

      end loop;
   end Start_Server;

end Keyboard.Server;
