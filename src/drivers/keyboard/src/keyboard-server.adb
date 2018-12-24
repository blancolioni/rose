with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Interrupt_Handler;
with Rose.Interfaces.Stream_Reader;

with Rose.Devices.Port_IO;

with Keyboard.Codes;
with Keyboard.Commands;
with Keyboard.Logging;

package body Keyboard.Server is

   Receive_Cap : Rose.Capabilities.Capability;
   Reader_Cap  : Rose.Capabilities.Capability;
   Key_Cap     : Rose.Capabilities.Capability;

   Buffer_Size : constant := 100;
   Buffer         : System.Storage_Elements.Storage_Array (1 .. Buffer_Size);
   Read_Position  : System.Storage_Elements.Storage_Offset := 1;
   Write_Position : System.Storage_Elements.Storage_Offset := 1;

   Have_Blocked_Request : Boolean := False;
   Blocked_Request : aliased Rose.Invocation.Invocation_Record;

--     Initialized : Boolean := False;
--     Have_Aux    : Boolean := False;

   procedure Add_To_Buffer (Keys : System.Storage_Elements.Storage_Array);

   procedure Register_IRQ;

   procedure Send_Buffer
     (To_Address : System.Address;
      Length     : System.Storage_Elements.Storage_Count;
      Reply      : in out Rose.Invocation.Invocation_Record);

   procedure Send (Command : Rose.Words.Word_8);
   procedure Send (Command : Rose.Words.Word_8;
                   Data    : Rose.Words.Word_8);
   function Receive return Rose.Words.Word_8;

   procedure Scan_Keyboard
     (Code    : out Rose.Words.Word_8;
      Aux     : out Boolean;
      Success : out Boolean);

   procedure Wait;
   procedure Clear_Keys;

   procedure Initialize_Keyboard;

   -------------------
   -- Add_To_Buffer --
   -------------------

   procedure Add_To_Buffer (Keys : System.Storage_Elements.Storage_Array) is
      use type System.Storage_Elements.Storage_Offset;
   begin
      for Key of Keys loop
         exit when (Write_Position = Buffer_Size
                    and then Read_Position = 1)
           or else (Write_Position + 1 = Read_Position);

         Buffer (Write_Position) := Key;

         if Write_Position = Buffer_Size then
            Write_Position := 1;
         else
            Write_Position := Write_Position + 1;
         end if;
      end loop;
   end Add_To_Buffer;

   ----------------
   -- Clear_Keys --
   ----------------

   procedure Clear_Keys is
      Code         : Rose.Words.Word_8;
      Aux, Success : Boolean;
   begin
      Scan_Keyboard (Code, Aux, Success);
   end Clear_Keys;

   -------------------
   -- Create_Server --
   -------------------

   procedure Create_Server is
   begin
      Receive_Cap :=
        Rose.System_Calls.Server.Create_Receive_Cap
          (Create_Endpoint_Cap);
      Reader_Cap :=
        Rose.System_Calls.Server.Create_Endpoint
          (Create_Endpoint_Cap,
           Rose.Interfaces.Stream_Reader.Read_Endpoint);
      Initialize_Keyboard;
   end Create_Server;

   -------------------------
   -- Initialize_Keyboard --
   -------------------------

   procedure Initialize_Keyboard is
      use Rose.Words;
      use Keyboard.Commands;
      Current_Config : Word_8;
   begin
      Send (Disable_Aux);
      Send (Disable_Keyboard);
      Clear_Keys;
      Send (Read_Controller);
      Current_Config := Receive;
--        Have_Aux := (Current_Config and 16#10#) /= 0;

      Send (Self_Test);

      declare
         Test_Result : constant Word_8 := Receive;
      begin
         if Test_Result /= 16#55# then
            Keyboard.Logging.Log
              ("keyboard: self-test failed", Test_Result);
            return;
         end if;
      end;

      Register_IRQ;

      Current_Config := Current_Config or 16#01#;

--        if Have_Aux then
--           null;
--        end if;
--
      Send (Write_Controller, Current_Config);
      Send (Enable_Keyboard);

--        if Have_Aux then
--           null;
--        end if;
--
   end Initialize_Keyboard;

   -------------
   -- Receive --
   -------------

   function Receive return Rose.Words.Word_8 is
      use Rose.Devices.Port_IO;
      use Rose.Words;
   begin
      for I in 1 .. 16 loop
         declare
            Status : constant Word_8 := Port_In_8 (Read_Status_Cap);
         begin
            if (Status and Commands.KB_OUT_FULL) /= 0 then
               return Port_In_8 (Read_Key_Cap);
            end if;
         end;
      end loop;
      return 0;
   end Receive;

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

   -------------------
   -- Scan_Keyboard --
   -------------------

   procedure Scan_Keyboard
     (Code    : out Rose.Words.Word_8;
      Aux     : out Boolean;
      Success : out Boolean)
   is
      use Rose.Words;
      use Rose.Devices.Port_IO;
      Status : constant Word_8 := Port_In_8 (Read_Status_Cap);
      pragma Unreferenced (Status);
   begin
      Code := Port_In_8 (Read_Key_Cap);
      Aux := False;
      Success := True;
   end Scan_Keyboard;

   ----------
   -- Send --
   ----------

   procedure Send (Command : Rose.Words.Word_8) is
   begin
      Rose.Devices.Port_IO.Port_Out_8
        (Command_Cap, Command);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send (Command : Rose.Words.Word_8;
                   Data    : Rose.Words.Word_8)
   is
   begin
      Wait;
      Rose.Devices.Port_IO.Port_Out_8 (Command_Cap, Command);
      Wait;
      Rose.Devices.Port_IO.Port_Out_8 (Data_Cap, Data);
   end Send;

   -----------------
   -- Send_Buffer --
   -----------------

   procedure Send_Buffer
     (To_Address : System.Address;
      Length     : System.Storage_Elements.Storage_Count;
      Reply      : in out Rose.Invocation.Invocation_Record)
   is
      use System.Storage_Elements;
      Target_Buffer : Storage_Array (1 .. Length);
      pragma Import (Ada, Target_Buffer);
      for Target_Buffer'Address use To_Address;
      Sent_Count : Storage_Count := 0;
   begin
      for Item of Target_Buffer loop
         exit when Read_Position = Write_Position;

         Item := Buffer (Read_Position);
         Sent_Count := Sent_Count + 1;
         if Read_Position = Buffer_Size then
            Read_Position := 1;
         else
            Read_Position := Read_Position + 1;
         end if;
      end loop;
      Rose.System_Calls.Send_Word (Reply, Rose.Words.Word (Sent_Count));

   end Send_Buffer;

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
         return (Status and 16#20#) /= 0;
      end Key_Code_Available;

      -------------------
      -- Read_Key_Code --
      -------------------

      function Read_Key_Code return Rose.Words.Word_8 is
      begin
         return Rose.Devices.Port_IO.Port_In_8 (Read_Key_Cap);
      end Read_Key_Code;

   begin

      Rose.Devices.Port_IO.Port_Out_8
        (Command_Cap, 16#F4#);

      loop
         Send_Reply := True;
         Rose.System_Calls.Initialize_Receive (Params, Receive_Cap);
         Rose.System_Calls.Receive_Words (Params, 1);
         Rose.System_Calls.Invoke_Capability (Params);
         Rose.System_Calls.Initialize_Reply (Reply, Params.Reply_Cap);

         case Params.Endpoint is
            when Rose.Interfaces.Get_Interface_Endpoint =>
               Rose.System_Calls.Send_Cap (Reply, Reader_Cap);

            when Rose.Interfaces.Stream_Reader.Read_Endpoint =>
               declare
                  use System.Storage_Elements;
               begin
                  if Read_Position = Write_Position then
                     Send_Reply := False;
                     if not Have_Blocked_Request then
                        Have_Blocked_Request := True;
                        Blocked_Request := Params;
                     else
                        --  more than one reader is problematic.
                        --  late-comers will block ... forever!
                        null;
                     end if;
                  else
                     Send_Buffer
                       (Params.Buffer_Address, Params.Buffer_Length, Reply);
                  end if;
               end;

            when Rose.Interfaces.Interrupt_Handler.Handle_Interrupt_Endpoint =>
               while Key_Code_Available loop
                  declare
                     use System.Storage_Elements;
                     Code  : constant Rose.Words.Word_8 :=
                               Read_Key_Code;
                     Result : Storage_Array (1 .. 10);
                     Last   : Storage_Count;
                  begin
                     Keyboard.Codes.Handle_Key
                       (Code    => Word (Code and 16#7F#),
                        Pressed => Code < 16#80#,
                        Result  => Result,
                        Last    => Last);

                     Add_To_Buffer (Result (1 .. Last));

                     if Have_Blocked_Request then
                        Rose.System_Calls.Initialize_Reply
                          (Reply, Blocked_Request.Reply_Cap);
                        Send_Buffer
                          (Blocked_Request.Buffer_Address,
                           Blocked_Request.Buffer_Length,
                           Reply);
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

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      for I in 1 .. 16 loop
         declare
            use Rose.Words;
            Status : constant Word_8 :=
                       Rose.Devices.Port_IO.Port_In_8 (Read_Status_Cap);
         begin
            if (Status and Commands.KB_OUT_FULL) /= 0 then
               Clear_Keys;
            end if;
            exit when
              (Status and (Commands.KB_IN_FULL or Commands.KB_OUT_FULL))
                = 0;
         end;
      end loop;
   end Wait;

end Keyboard.Server;
