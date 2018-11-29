with System.Storage_Elements;

with Rose.Invocation;
with Rose.System_Calls.Server;
with Rose.Words;

with Rose.Interfaces.Keyboard_Handler;
with Rose.Interfaces.Stream_Reader;

with Keyboard.Codes;

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

   procedure Add_To_Buffer (Keys : System.Storage_Elements.Storage_Array);

   procedure Register_IRQ;

   procedure Send_Buffer
     (To_Address : System.Address;
      Length     : System.Storage_Elements.Storage_Count;
      Reply      : in out Rose.Invocation.Invocation_Record);

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
      Register_IRQ;
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
           Rose.Interfaces.Keyboard_Handler.Handle_Key_Endpoint);
      Rose.System_Calls.Initialize_Send (Params, Register_IRQ_Cap);
      Rose.System_Calls.Send_Cap (Params, Key_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Register_IRQ;

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
      Params      : aliased Rose.Invocation.Invocation_Record;
      Reply       : aliased Rose.Invocation.Invocation_Record;
      Send_Reply  : Boolean;
   begin
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

            when Rose.Interfaces.Keyboard_Handler.Handle_Key_Endpoint =>
               declare
                  use System.Storage_Elements;
                  use type Rose.Words.Word;
                  Code  : constant Rose.Words.Word :=
                            Params.Data (0);
                  State : constant Rose.Words.Word :=
                            Params.Data (1);
                  Result : Storage_Array (1 .. 10);
                  Last   : Storage_Count;
               begin
                  Keyboard.Codes.Handle_Key
                    (Code    => Code,
                     Pressed => State /= 0,
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