with System.Machine_Code;
with System.Storage_Elements;

with Rose.Devices.Port_IO;
with Rose.Invocation;
with Rose.System_Calls;

package body Virtio_Net.Device is

   use Ada.Streams;
   use System.Storage_Elements;
   use Rose.Words;

   Page_Size      : constant := 4096;
   Base           : constant := First_DMA_Page * Page_Size;
   Rx_Queue_Base  : constant := Base;
   Tx_Queue_Base  : constant := Base + 3 * Page_Size;
   Rx_Buffer_Base : constant := Base + 6 * Page_Size;
   Tx_Buffer_Base : constant := Base + 14 * Page_Size;

   Buffer_Size    : constant := 2048;
   Header_Size    : constant := 10;
   --  The legacy virtio-net header, without mergeable receive buffers
   Frame_Offset   : constant := 16;
   --  Where the frame starts in its buffer
   Rx_Buffers     : constant := 16;
   Tx_Buffers     : constant := 4;
   Max_Queue_Size : constant := 256;
   Min_Queue_Size : constant := 2 * Rx_Buffers;
   --  Each buffer takes two descriptors

   --  The legacy registers, as offsets in BAR 0

   Host_Features  : constant Word_8 := 16#00#;
   Guest_Features : constant Word_8 := 16#04#;
   Queue_Address  : constant Word_8 := 16#08#;
   Queue_Size     : constant Word_8 := 16#0C#;
   Queue_Select   : constant Word_8 := 16#0E#;
   Queue_Notify   : constant Word_8 := 16#10#;
   Device_Status  : constant Word_8 := 16#12#;
   ISR_Status     : constant Word_8 := 16#13#;
   MAC_Config     : constant Word_8 := 16#14#;

   Status_Acknowledge : constant Word_8 := 1;
   Status_Driver      : constant Word_8 := 2;
   Status_Driver_OK   : constant Word_8 := 4;
   Status_Failed      : constant Word_8 := 128;

   MAC_Feature : constant Word_32 := 16#20#;
   --  VIRTIO_NET_F_MAC: the card's address is in its configuration

   Next_Flag  : constant Word_16 := 1;
   Write_Flag : constant Word_16 := 2;
   --  Descriptor flags: the chain goes on, and the card writes the buffer

   Receive_Queue  : constant Word_16 := 0;
   Transmit_Queue : constant Word_16 := 1;

   type Queue_Record is
      record
         Base        : Integer_Address := 0;
         Size        : Word_16 := 0;
         Avail_Index : Word_16 := 0;
         --  The available ring's index, as this driver last wrote it
         Last_Used   : Word_16 := 0;
         --  The used ring's index, as far as this driver has read it
      end record;

   Rx, Tx  : Queue_Record;
   Tx_Busy : array (0 .. Tx_Buffers - 1) of Boolean := (others => False);
   MAC     : Word_64 := 0;

   function Avail_Base (Q : Queue_Record) return Integer_Address
   is (Q.Base + 16 * Integer_Address (Q.Size));

   function Used_Base (Q : Queue_Record) return Integer_Address
   is ((Avail_Base (Q) + 6 + 2 * Integer_Address (Q.Size) + Page_Size - 1)
       / Page_Size * Page_Size);
   --  The legacy layout puts the used ring on the next page boundary

   function Used_Index (Q : Queue_Record) return Word_16;

   procedure Barrier
     with Inline;
   --  Keep the compiler from moving memory accesses across this point.
   --  The card sees memory as the CPU writes it, in order, on x86.

   function Get_16 (Address : Integer_Address) return Word_16;
   function Get_32 (Address : Integer_Address) return Word_32;
   procedure Put_16 (Address : Integer_Address; Value : Word_16);
   procedure Put_32 (Address : Integer_Address; Value : Word_32);
   procedure Put_64 (Address : Integer_Address; Value : Word_64);

   function In_8 (Register : Word_8) return Word_8
   is (Rose.Devices.Port_IO.Port_In_8 (Port_In_8_Cap, Register));

   function In_16 (Register : Word_8) return Word_16
   is (Rose.Devices.Port_IO.Port_In_16 (Port_In_16_Cap, Register));

   function In_32 (Register : Word_8) return Word_32
   is (Rose.Devices.Port_IO.Port_In_32 (Port_In_32_Cap, Register));

   procedure Out_8 (Register : Word_8; Value : Word_8);
   procedure Out_16 (Register : Word_8; Value : Word_16);
   procedure Out_32 (Register : Word_8; Value : Word_32);

   procedure Notify (Queue : Word_16);
   --  Tell the card that Queue has new buffers

   procedure Set_Descriptor
     (Q       : Queue_Record;
      Index   : Word_16;
      Address : Integer_Address;
      Length  : Word_32;
      Flags   : Word_16;
      Next    : Word_16);

   procedure Make_Available
     (Q    : in out Queue_Record;
      Head : Word_16);
   --  Put the chain that starts at descriptor Head in Q's available ring

   procedure Set_Up_Queue
     (Number  : Word_16;
      Q       : in out Queue_Record;
      Base    : Integer_Address;
      Success : out Boolean);
   --  Select queue Number, read its size, clear its pages and give the
   --  card their page number

   procedure Reclaim_Transmit;
   --  Mark the transmit buffers that the card has finished with as free

   -------------
   -- Barrier --
   -------------

   procedure Barrier is
   begin
      System.Machine_Code.Asm
        ("", Volatile => True, Clobber => "memory");
   end Barrier;

   -------------------
   -- Frame_Waiting --
   -------------------

   function Frame_Waiting return Boolean is
   begin
      return Rx.Size > 0 and then Rx.Last_Used /= Used_Index (Rx);
   end Frame_Waiting;

   ------------
   -- Get_16 --
   ------------

   function Get_16 (Address : Integer_Address) return Word_16 is
      Value : Word_16
        with Import, Volatile, Address => To_Address (Address);
   begin
      return Value;
   end Get_16;

   ------------
   -- Get_32 --
   ------------

   function Get_32 (Address : Integer_Address) return Word_32 is
      Value : Word_32
        with Import, Volatile, Address => To_Address (Address);
   begin
      return Value;
   end Get_32;

   -----------------
   -- Interrupted --
   -----------------

   procedure Interrupted is
      Status : constant Word_8 := In_8 (ISR_Status);
      pragma Unreferenced (Status);
   begin
      Reclaim_Transmit;
   end Interrupted;

   -----------------
   -- MAC_Address --
   -----------------

   function MAC_Address return Rose.Words.Word_64 is
   begin
      return MAC;
   end MAC_Address;

   --------------------
   -- Make_Available --
   --------------------

   procedure Make_Available
     (Q    : in out Queue_Record;
      Head : Word_16)
   is
   begin
      Put_16 (Avail_Base (Q) + 4
              + 2 * Integer_Address (Q.Avail_Index mod Q.Size),
              Head);
      Barrier;
      Q.Avail_Index := Q.Avail_Index + 1;
      Put_16 (Avail_Base (Q) + 2, Q.Avail_Index);
   end Make_Available;

   ---------------
   -- Map_Pages --
   ---------------

   procedure Map_Pages is
      Params : aliased Rose.Invocation.Invocation_Record;
   begin
      Rose.System_Calls.Initialize_Send (Params, DMA_Pages_Cap);
      Rose.System_Calls.Invoke_Capability (Params);
   end Map_Pages;

   ------------
   -- Notify --
   ------------

   procedure Notify (Queue : Word_16) is
   begin
      Barrier;
      Out_16 (Queue_Notify, Queue);
   end Notify;

   -----------
   -- Out_8 --
   -----------

   procedure Out_8 (Register : Word_8; Value : Word_8) is
   begin
      Rose.Devices.Port_IO.Port_Out_8 (Port_Out_8_Cap, Register, Value);
   end Out_8;

   ------------
   -- Out_16 --
   ------------

   procedure Out_16 (Register : Word_8; Value : Word_16) is
   begin
      Rose.Devices.Port_IO.Port_Out_16 (Port_Out_16_Cap, Register, Value);
   end Out_16;

   ------------
   -- Out_32 --
   ------------

   procedure Out_32 (Register : Word_8; Value : Word_32) is
   begin
      Rose.Devices.Port_IO.Port_Out_32 (Port_Out_32_Cap, Register, Value);
   end Out_32;

   ------------
   -- Put_16 --
   ------------

   procedure Put_16 (Address : Integer_Address; Value : Word_16) is
      Target : Word_16
        with Import, Volatile, Address => To_Address (Address);
   begin
      Target := Value;
   end Put_16;

   ------------
   -- Put_32 --
   ------------

   procedure Put_32 (Address : Integer_Address; Value : Word_32) is
      Target : Word_32
        with Import, Volatile, Address => To_Address (Address);
   begin
      Target := Value;
   end Put_32;

   ------------
   -- Put_64 --
   ------------

   procedure Put_64 (Address : Integer_Address; Value : Word_64) is
      Target : Word_64
        with Import, Volatile, Address => To_Address (Address);
   begin
      Target := Value;
   end Put_64;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Frame : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Last := Frame'First - 1;

      if not Frame_Waiting then
         return;
      end if;

      Barrier;

      declare
         Used_Entry : constant Integer_Address :=
                        Used_Base (Rx) + 4
                        + 8 * Integer_Address (Rx.Last_Used mod Rx.Size);
         Head       : constant Word_32 := Get_32 (Used_Entry);
         Length     : constant Word_32 := Get_32 (Used_Entry + 4);
         Buffer     : constant Word_32 := Head / 2;
      begin
         if Buffer < Rx_Buffers and then Length > Header_Size then
            declare
               Count   : constant Stream_Element_Offset :=
                           Stream_Element_Offset'Min
                             (Frame'Length,
                              Stream_Element_Offset'Min
                                (Stream_Element_Offset
                                   (Length - Header_Size),
                                 Max_Frame_Size));
               Address : constant Integer_Address :=
                           Rx_Buffer_Base + Frame_Offset
                           + Integer_Address (Buffer) * Buffer_Size;
               Data    : Stream_Element_Array (1 .. Count)
                 with Import, Address => To_Address (Address);
            begin
               Frame (Frame'First .. Frame'First + Count - 1) := Data;
               Last := Frame'First + Count - 1;
            end;
         end if;

         Rx.Last_Used := Rx.Last_Used + 1;

         --  The buffer goes back to the card, with its descriptors as
         --  they were set at start.

         if Buffer < Rx_Buffers then
            Barrier;
            Make_Available (Rx, Word_16 (Head));
            Notify (Receive_Queue);
         end if;
      end;
   end Receive;

   ----------------------
   -- Reclaim_Transmit --
   ----------------------

   procedure Reclaim_Transmit is
   begin
      while Tx.Size > 0 and then Tx.Last_Used /= Used_Index (Tx) loop
         Barrier;
         declare
            Head : constant Word_32 :=
                     Get_32 (Used_Base (Tx) + 4
                             + 8 * Integer_Address
                               (Tx.Last_Used mod Tx.Size));
         begin
            if Head / 2 < Tx_Buffers then
               Tx_Busy (Natural (Head / 2)) := False;
            end if;
            Tx.Last_Used := Tx.Last_Used + 1;
         end;
      end loop;
   end Reclaim_Transmit;

   ----------
   -- Send --
   ----------

   procedure Send
     (Frame : Ada.Streams.Stream_Element_Array;
      Sent  : out Boolean)
   is
   begin
      Sent := False;
      Reclaim_Transmit;

      if Tx.Size = 0
        or else Frame'Length = 0
        or else Frame'Length > Max_Frame_Size
      then
         return;
      end if;

      for T in Tx_Busy'Range loop
         if not Tx_Busy (T) then
            declare
               Buffer : constant Integer_Address :=
                          Tx_Buffer_Base
                          + Integer_Address (T) * Buffer_Size;
               Header : Storage_Array (1 .. Header_Size)
                 with Import, Address => To_Address (Buffer);
               Data   : Stream_Element_Array (1 .. Frame'Length)
                 with Import,
                      Address => To_Address (Buffer + Frame_Offset);
               Head   : constant Word_16 := Word_16 (2 * T);
            begin
               Header := (others => 0);
               Data := Frame;
               Set_Descriptor
                 (Tx, Head, Buffer, Header_Size, Next_Flag, Head + 1);
               Set_Descriptor
                 (Tx, Head + 1, Buffer + Frame_Offset,
                  Word_32 (Frame'Length), 0, 0);
               Barrier;
               Make_Available (Tx, Head);
               Tx_Busy (T) := True;
               Notify (Transmit_Queue);
               Sent := True;
               return;
            end;
         end if;
      end loop;
   end Send;

   --------------------
   -- Set_Descriptor --
   --------------------

   procedure Set_Descriptor
     (Q       : Queue_Record;
      Index   : Word_16;
      Address : Integer_Address;
      Length  : Word_32;
      Flags   : Word_16;
      Next    : Word_16)
   is
      Descriptor : constant Integer_Address :=
                     Q.Base + 16 * Integer_Address (Index);
   begin
      Put_64 (Descriptor, Word_64 (Address));
      Put_32 (Descriptor + 8, Length);
      Put_16 (Descriptor + 12, Flags);
      Put_16 (Descriptor + 14, Next);
   end Set_Descriptor;

   ------------------
   -- Set_Up_Queue --
   ------------------

   procedure Set_Up_Queue
     (Number  : Word_16;
      Q       : in out Queue_Record;
      Base    : Integer_Address;
      Success : out Boolean)
   is
   begin
      Out_16 (Queue_Select, Number);
      Q := (Base        => Base,
            Size        => In_16 (Queue_Size),
            Avail_Index => 0,
            Last_Used   => 0);
      Success := Q.Size in Min_Queue_Size .. Max_Queue_Size;

      if Success then
         declare
            Pages : Storage_Array (1 .. 3 * Page_Size)
              with Import, Volatile, Address => To_Address (Base);
         begin
            Pages := (others => 0);
         end;
         Barrier;
         Out_32 (Queue_Address, Word_32 (Base / Page_Size));
      else
         Q.Size := 0;
      end if;
   end Set_Up_Queue;

   -----------
   -- Start --
   -----------

   procedure Start (Success : out Boolean) is
      Rx_Ready, Tx_Ready : Boolean;
   begin
      Rx := (others => <>);
      Tx := (others => <>);
      Tx_Busy := (others => False);

      Out_8 (Device_Status, 0);
      Out_8 (Device_Status, Status_Acknowledge);
      Out_8 (Device_Status, Status_Acknowledge + Status_Driver);
      Out_32 (Guest_Features, In_32 (Host_Features) and MAC_Feature);

      Set_Up_Queue (Receive_Queue, Rx, Rx_Queue_Base, Rx_Ready);
      Set_Up_Queue (Transmit_Queue, Tx, Tx_Queue_Base, Tx_Ready);

      if not Rx_Ready or else not Tx_Ready then
         Out_8 (Device_Status, Status_Failed);
         Rx.Size := 0;
         Tx.Size := 0;
         Success := False;
         return;
      end if;

      for B in 0 .. Rx_Buffers - 1 loop
         declare
            Buffer : constant Integer_Address :=
                       Rx_Buffer_Base + Integer_Address (B) * Buffer_Size;
            Head   : constant Word_16 := Word_16 (2 * B);
         begin
            Set_Descriptor
              (Rx, Head, Buffer, Header_Size,
               Next_Flag + Write_Flag, Head + 1);
            Set_Descriptor
              (Rx, Head + 1, Buffer + Frame_Offset,
               Buffer_Size - Frame_Offset, Write_Flag, 0);
            Make_Available (Rx, Head);
         end;
      end loop;

      MAC := 0;
      for I in Word_8 range 0 .. 5 loop
         MAC := MAC * 256 + Word_64 (In_8 (MAC_Config + I));
      end loop;

      Out_8 (Device_Status,
             Status_Acknowledge + Status_Driver + Status_Driver_OK);
      Notify (Receive_Queue);
      Success := True;
   end Start;

   ----------------
   -- Used_Index --
   ----------------

   function Used_Index (Q : Queue_Record) return Word_16 is
   begin
      return Get_16 (Used_Base (Q) + 2);
   end Used_Index;

end Virtio_Net.Device;
