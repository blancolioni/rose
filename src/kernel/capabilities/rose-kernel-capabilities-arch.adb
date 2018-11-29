with Rose.Arch;
with Rose.Words;

with Rose.Kernel.Processes;

with Rose.Boot.Console;

package body Rose.Kernel.Capabilities.Arch is

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Cap    : Rose.Capabilities.Layout.Capability_Layout;
      Params : Rose.Invocation.Invocation_Access)
   is
      use Rose.Invocation;
      use type Rose.Objects.Object_Id;
      use Rose.Words;
      First_Port : constant Word_16 :=
                     Word_16 (Cap.Payload mod 65536);
      Last_Port  : constant Word_16 :=
                     Word_16 (Cap.Payload / 2 ** 32 mod 65536);
      Size       : constant Port_IO_Size :=
                     Port_IO_Size'Val
                       (Natural (Cap.Header.Identifier) mod 4);
      Log_Calls : constant Boolean := Log_Port_IO;

      procedure Send_To_Encoded_Port (Value : Word);
      procedure Send_To_First_Port (Value : Word);

      procedure Log
        (Port  : Word_16;
         Value : Word_32;
         Force : Boolean := False);

      procedure Log_Error
        (Port    : Word_16;
         Value   : Word_32;
         Message : String);

      ---------
      -- Log --
      ---------

      procedure Log
        (Port  : Word_16;
         Value : Word_32;
         Force : Boolean := False)
      is
         use Rose.Boot.Console;
         Port_Out : constant Boolean :=
                      Rose.Objects.Endpoint_Id (Cap.Header.Endpoint)
                      in Port_Out_Range_Endpoint | Port_Out_Endpoint;
      begin
         if Log_Calls or else Force then
            Put ("port-");
            Put (if Port_Out then "out" else "in");
            Put (": ");
            Put (Port);
            Put (if Port_Out then " " else " -> ");
            case Size is
            when Data_8 =>
               Put (Word_8 (Value));
            when Data_16 =>
               Put (Word_16 (Value));
            when Data_32 =>
               Put (Value);
            when Data_64 =>
               Put ("64-bit port not available");
            end case;
            New_Line;
         end if;
      end Log;

      ---------------
      -- Log_Error --
      ---------------

      procedure Log_Error
        (Port    : Word_16;
         Value   : Word_32;
         Message : String)
      is
      begin
         Rose.Boot.Console.Put ("arch-cap: error: ");
         Rose.Boot.Console.Put (Message);
         Rose.Boot.Console.Put (" while executing: ");
         Log (Port, Value, True);
      end Log_Error;

      --------------------------
      -- Send_To_Encoded_Port --
      --------------------------

      procedure Send_To_Encoded_Port (Value : Word) is
         Offset : constant Word_16 := Word_16 (Value mod 256);
         Data   : constant Word    := Value / 256;
      begin
         if Offset < 255
           and then First_Port + Offset <= Last_Port
         then
            case Size is
               when Data_8 =>
                  Log (First_Port + Offset, Data);
                  Rose.Arch.Port_Out_8
                    (Port  => First_Port + Offset,
                     Value => Word_8 (Data));
               when Data_16 =>
                  Log (First_Port + Offset, Data);
                  Rose.Arch.Port_Out_16
                    (Port  => First_Port + Offset,
                     Value => Word_16 (Data));
               when others =>
                  Log_Error (First_Port + Offset, Data,
                             "bad size for encoded port");
            end case;
         end if;
      end Send_To_Encoded_Port;

      ------------------------
      -- Send_To_First_Port --
      ------------------------

      procedure Send_To_First_Port (Value : Word) is
      begin
         Log (First_Port, Value);
         case Size is
            when Data_8 =>
               Rose.Arch.Port_Out_8
                 (Port  => First_Port,
                  Value => Word_8 (Value mod 256));
            when Data_16 =>
               Rose.Arch.Port_Out_16
                 (Port  => First_Port,
                  Value => Word_16 (Value mod 65536));
            when Data_32 =>
               Rose.Arch.Port_Out_32
                 (Port  => First_Port,
                  Value => Value);
            when Data_64 =>
               null;
         end case;
      end Send_To_First_Port;

   begin
      case Rose.Objects.Endpoint_Id (Cap.Header.Endpoint) is
         when Port_Out_Range_Endpoint =>
            if Size in Data_8 | Data_16 then
               Rose.Invocation.For_Each_Sent_Word
                 (Params.all, Send_To_Encoded_Port'Access);
            elsif Size = Data_32 then
               declare
                  Index : Parameter_Word_Index := 0;
                  Last  : constant Parameter_Word_Index :=
                            (if Params.Control.Last_Sent_Word = 15
                             then 11
                             elsif Params.Control.Last_Sent_Word >= 10
                             then 6
                             elsif Params.Control.Last_Sent_Word >= 5
                             then 1
                             else 0);
               begin
                  while Index < Last loop
                     declare
                        Ports  : Word := Params.Data (Index);
                        Data   : constant array (1 .. 4) of Word_32 :=
                                   (Params.Data (Index + 1),
                                    Params.Data (Index + 2),
                                    Params.Data (Index + 3),
                                    Params.Data (Index + 4));
                        Offset : Word_16;
                     begin

                        for I in Data'Range loop
                           Offset := Word_16 (Ports mod 256);
                           Ports := Ports / 256;

                           if Offset < 255
                             and then First_Port + Offset < Last_Port
                           then
                              Log (First_Port + Offset, Data (I));
                              Rose.Arch.Port_Out_32
                                (Port  => First_Port + Offset,
                                 Value => Data (I));
                           end if;
                        end loop;
                     end;
                     Index := Index + 5;
                  end loop;
               end;
            end if;

            Params.Control.Flags (Send_Words) := False;

         when Port_Out_Endpoint =>
            Rose.Invocation.For_Each_Sent_Word
              (Params.all, Send_To_First_Port'Access);
            Params.Control.Flags (Send_Words) := False;

         when Port_In_Range_Endpoint | Port_In_Endpoint =>
            if Params.Control.Flags (Recv_Words) then
               for I in 0 .. Params.Control.Last_Recv_Word loop
                  case Size is
                     when Data_8 =>
                        declare
                           Port : Word_16 := First_Port;
                        begin
                           if Params.Control.Flags (Send_Words)
                             and then I <= Params.Control.Last_Sent_Word
                           then
                              Port := Port + Word_16 (Params.Data (I));
                           end if;

                           if Port in First_Port .. Last_Port then
                              Params.Data (I) :=
                                Word (Rose.Arch.Port_In_8 (Port));
                              Log (Port, Params.Data (I));
                           else
                              Log_Error (Port, 0, "bad port");
                              Params.Data (I) := Word'Last;
                           end if;
                        end;

                     when Data_16 =>
                        Params.Data (I) :=
                          Word (Rose.Arch.Port_In_16 (First_Port));
                        Log (First_Port, Params.Data (I));
                     when Data_32 =>
                        Params.Data (I) :=
                          Word (Rose.Arch.Port_In_32 (First_Port));
                        Log (First_Port, Params.Data (I));
                     when Data_64 =>
                        Log_Error (First_Port, 0, "64-bit not available");
                  end case;
               end loop;
            end if;

            Params.Control.Last_Sent_Word := Params.Control.Last_Recv_Word;

         when others =>
            Rose.Boot.Console.Put ("arch-cap: bad endpoint: ");
            Rose.Boot.Console.Put (Rose.Words.Word_8 (Cap.Header.Endpoint));
            Rose.Boot.Console.New_Line;

      end case;

      Rose.Kernel.Processes.Set_Current_State
        (Rose.Kernel.Processes.Current_Process_Id,
         Rose.Kernel.Processes.Ready);

   end Handle;

end Rose.Kernel.Capabilities.Arch;
